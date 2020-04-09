module online_coarse_graining_mod

  use fv_arrays_mod, only: fv_atmos_type
  use fms_mod, only: check_nml_error, close_file, open_namelist_file
  use mpp_domains_mod, only: domain2d, mpp_define_io_domain, mpp_define_mosaic, mpp_get_compute_domain
  use mpp_mod, only: input_nml_file, mpp_npes
  
  implicit none
  private

  public :: online_coarse_graining_init, weighted_block_average

  interface weighted_block_average
     module procedure weighted_block_average_2d
     module procedure weighted_block_average_3d_field_2d_weights
  end interface weighted_block_average
  
  ! Global variables for the module, initialized in online_coarse_graining_init
  type(domain2d) :: coarse_domain
  integer :: is, ie, js, je, npz
  integer :: is_coarse, ie_coarse, js_coarse, je_coarse
  integer :: target_resolution
  
  ! Namelist parameters initialized with default values
  integer :: coarsening_factor = 8
  integer :: coarse_io_layout(2) = (/1, 1/)
  character(len=64) :: coarse_graining_strategy = 'model_level'
  logical :: write_coarse_grained_restart_files = .false.
  logical :: write_coarse_grained_diagnostics = .false.
  
  namelist /online_coarse_graining_nml/ coarsening_factor, coarse_io_layout, &
       coarse_graining_strategy, write_coarse_grained_restart_files, &
       write_coarse_grained_diagnostics

contains

  subroutine online_coarse_graining_init(Atm)
    type(fv_atmos_type), intent(inout) :: Atm

    character(len=256) :: error_message
    logical :: exists
    integer :: namelist_file, error_code, iostat

#ifdef INTERNAL_FILE_NML
    read(input_nml_file, online_coarse_graining_nml, iostat=iostat)
    error_code = check_nml_error(iostat, 'online_coarse_graining_nml')
#else
    namelist_file = open_namelist_file(Atm%nml_filename)
    read(namelist_file, online_coarse_graining_nml, iostat=iostat)
    error_code = check_nml_error(iostat, 'online_coarse_graining_nml')
    call close_file(namelist_file)
#endif

    npz = Atm%npz
    call compute_target_resolution(Atm, coarsening_factor, target_resolution)
    call define_cubic_mosaic(coarse_domain, target_resolution, target_resolution, Atm%layout)
    call mpp_define_io_domain(coarse_domain, coarse_io_layout)
    call mpp_get_compute_domain(coarse_domain, is_coarse, ie_coarse, js_coarse, je_coarse)

    Atm%coarse_bd%is_coarse = is_coarse
    Atm%coarse_bd%ie_coarse = ie_coarse
    Atm%coarse_bd%js_coarse = js_coarse
    Atm%coarse_bd%je_coarse = je_coarse

    Atm%target_coarse_resolution = target_resolution
    Atm%coarse_domain = coarse_domain
  end subroutine online_coarse_graining_init

  subroutine compute_target_resolution(Atm, coarsening_factor, target_resolution)
    type(fv_atmos_type), intent(in) :: Atm
    integer, intent(in) :: coarsening_factor
    integer, intent(out) :: target_resolution
    integer :: native_resolution
    native_resolution = Atm%flagstruct%npx - 1
    target_resolution = native_resolution / coarsening_factor
  end subroutine compute_target_resolution
  
  ! This subroutine is copied from fms/horiz_interp/horiz_interp_test.F90;
  ! domain_decomp in fv_mp_mod.F90 does something similar, but it does a
  ! few other unnecessary things (and requires more arguments).
  subroutine define_cubic_mosaic(domain, ni, nj, layout)
    type(domain2d), intent(inout) :: domain
    integer,        intent(in)    :: layout(:)
    integer,        intent(in)    :: ni, nj
    integer   :: pe_start(6), pe_end(6)
    integer   :: global_indices(4,6), layout2d(2,6)
    integer, dimension(12)        :: istart1, iend1, jstart1, jend1, tile1
    integer, dimension(12)        :: istart2, iend2, jstart2, jend2, tile2
    integer                       :: ntiles, num_contact
    integer :: p, npes_per_tile, i

    ntiles = 6
    num_contact = 12
    p = 0
    npes_per_tile = mpp_npes()/ntiles
    do i = 1, 6
       layout2d(:,i) = layout(:)
       global_indices(1,i) = 1
       global_indices(2,i) = ni
       global_indices(3,i) = 1
       global_indices(4,i) = nj
       pe_start(i) = p
       p = p + npes_per_tile
       pe_end(i) = p-1
    enddo

    !--- Contact line 1, between tile 1 (EAST) and tile 2 (WEST)
    tile1(1) = 1;     tile2(1) = 2
    istart1(1) = ni;  iend1(1) = ni;  jstart1(1) = 1;      jend1(1) = nj
    istart2(1) = 1;   iend2(1) = 1;   jstart2(1) = 1;      jend2(1) = nj

    !--- Contact line 2, between tile 1 (NORTH) and tile 3 (WEST)
    tile1(2) = 1; tile2(2) = 3
    istart1(2) = 1;      iend1(2) = ni;  jstart1(2) = nj;  jend1(2) = nj
    istart2(2) = 1;      iend2(2) = 1;   jstart2(2) = nj;  jend2(2) = 1

    !--- Contact line 3, between tile 1 (WEST) and tile 5 (NORTH)
    tile1(3) = 1;     tile2(3) = 5
    istart1(3) = 1;   iend1(3) = 1;      jstart1(3) = 1;   jend1(3) = nj
    istart2(3) = ni;  iend2(3) = 1;      jstart2(3) = nj;  jend2(3) = nj

    !--- Contact line 4, between tile 1 (SOUTH) and tile 6 (NORTH)
    tile1(4) = 1; tile2(4) = 6
    istart1(4) = 1;      iend1(4) = ni;  jstart1(4) = 1;   jend1(4) = 1
    istart2(4) = 1;      iend2(4) = ni;  jstart2(4) = nj;  jend2(4) = nj

    !--- Contact line 5, between tile 2 (NORTH) and tile 3 (SOUTH)
    tile1(5) = 2;        tile2(5) = 3
    istart1(5) = 1;      iend1(5) = ni;  jstart1(5) = nj;  jend1(5) = nj
    istart2(5) = 1;      iend2(5) = ni;  jstart2(5) = 1;   jend2(5) = 1

    !--- Contact line 6, between tile 2 (EAST) and tile 4 (SOUTH)
    tile1(6) = 2; tile2(6) = 4
    istart1(6) = ni;  iend1(6) = ni;  jstart1(6) = 1;      jend1(6) = nj
    istart2(6) = ni;  iend2(6) = 1;   jstart2(6) = 1;      jend2(6) = 1

    !--- Contact line 7, between tile 2 (SOUTH) and tile 6 (EAST)
    tile1(7) = 2; tile2(7) = 6
    istart1(7) = 1;   iend1(7) = ni;  jstart1(7) = 1;   jend1(7) = 1
    istart2(7) = ni;  iend2(7) = ni;  jstart2(7) = nj;  jend2(7) = 1

    !--- Contact line 8, between tile 3 (EAST) and tile 4 (WEST)
    tile1(8) = 3; tile2(8) = 4
    istart1(8) = ni;  iend1(8) = ni;  jstart1(8) = 1;      jend1(8) = nj
    istart2(8) = 1;   iend2(8) = 1;   jstart2(8) = 1;      jend2(8) = nj

    !--- Contact line 9, between tile 3 (NORTH) and tile 5 (WEST)
    tile1(9) = 3; tile2(9) = 5
    istart1(9) = 1;      iend1(9) = ni;  jstart1(9) = nj;  jend1(9) = nj
    istart2(9) = 1;      iend2(9) = 1;   jstart2(9) = nj;  jend2(9) = 1

    !--- Contact line 10, between tile 4 (NORTH) and tile 5 (SOUTH)
    tile1(10) = 4; tile2(10) = 5
    istart1(10) = 1;     iend1(10) = ni; jstart1(10) = nj; jend1(10) = nj
    istart2(10) = 1;     iend2(10) = ni; jstart2(10) = 1;  jend2(10) = 1

    !--- Contact line 11, between tile 4 (EAST) and tile 6 (SOUTH)
    tile1(11) = 4; tile2(11) = 6
    istart1(11) = ni; iend1(11) = ni; jstart1(11) = 1;     jend1(11) = nj
    istart2(11) = ni; iend2(11) = 1;  jstart2(11) = 1;     jend2(11) = 1

    !--- Contact line 12, between tile 5 (EAST) and tile 6 (WEST)
    tile1(12) = 5; tile2(12) = 6
    istart1(12) = ni; iend1(12) = ni; jstart1(12) = 1;     jend1(12) = nj
    istart2(12) = 1;  iend2(12) = 1;  jstart2(12) = 1;     jend2(12) = nj

    call mpp_define_mosaic(global_indices, layout2d, domain, ntiles, &
         num_contact, tile1, tile2, istart1, iend1, jstart1, jend1, &
         istart2, iend2, jstart2, jend2, pe_start, pe_end, &
         symmetry=.true., name='coarse cubic mosaic')

    return    
  end subroutine define_cubic_mosaic

  subroutine weighted_block_average_2d(weights, fine, coarse)
    real, intent(in) :: weights(is:ie,js:je), fine(is:ie,js:je)
    real, intent(out) :: coarse(is_coarse:ie_coarse,js_coarse:je_coarse)

    real, allocatable :: weighted_fine(:,:)
    integer :: i, j, i_coarse, j_coarse, offset

    offset = coarsening_factor - 1
    do i = is, ie, coarsening_factor
       i_coarse = (i - 1) / coarsening_factor + 1
       do j = js, je, coarsening_factor
          j_coarse = (j - 1) / coarsening_factor + 1
          coarse(i_coarse,j_coarse) = sum(weighted_fine(i:i+offset,j:j+offset)) / sum(weights(i:i+offset,j:j+offset))
       enddo
    enddo
  end subroutine weighted_block_average_2d

  subroutine weighted_block_average_3d_field_2d_weights(weights, fine, coarse)
    real, intent(in) :: weights(is:ie,js:je), fine(is:ie,js:je,1:npz)
    real, intent(out) :: coarse(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz)

    integer :: k

    do k = 1, npz
       call weighted_block_average_2d(weights, fine(is:ie,js:je,k), coarse(is_coarse:ie_coarse,js_coarse:je_coarse,k))
    enddo
  end subroutine weighted_block_average_3d_field_2d_weights
  
end module online_coarse_graining_mod
