module coarse_graining_mod

  use fv_arrays_mod, only: fv_coarse_grid_bounds_type, fv_grid_bounds_type, fv_coarse_graining_type
  use fms_mod, only: check_nml_error, close_file, open_namelist_file
  use mpp_domains_mod, only: domain2d, mpp_define_io_domain, mpp_define_mosaic, mpp_get_compute_domain
  use mpp_mod, only: FATAL, input_nml_file, mpp_error, mpp_npes
  
  implicit none
  private

  public :: block_sum, get_fine_array_bounds, get_coarse_array_bounds, coarse_graining_init, weighted_block_average, MODEL_LEVEL

  interface block_sum
     module procedure block_sum_2d
  end interface block_sum
  
  interface weighted_block_average
     module procedure weighted_block_average_2d
     module procedure weighted_block_average_3d_field_2d_weights
  end interface weighted_block_average
  
  ! Global variables for the module, initialized in coarse_graining_init
  type(domain2d) :: coarse_domain
  integer :: is, ie, js, je, npz
  integer :: is_coarse, ie_coarse, js_coarse, je_coarse
  integer :: nx_coarse
  character(len=11) :: MODEL_LEVEL = 'model_level'
  
  ! Namelist parameters initialized with default values
  integer :: coarsening_factor = 8
  integer :: coarse_io_layout(2) = (/1, 1/)
  character(len=64) :: strategy = 'model_level'  ! Valid values are 'model_level'
  logical :: do_coarse_graining = .false.
  
  namelist /coarse_graining_nml/ coarsening_factor, coarse_io_layout, strategy, do_coarse_graining

contains

  subroutine coarse_graining_init(npx, atm_npz, layout, bd, coarse_graining)
    integer, intent(in) :: npx
    integer, intent(in) :: atm_npz
    integer, intent(in) :: layout(2)
    type(fv_grid_bounds_type), intent(in) :: bd
    type(fv_coarse_graining_type), intent(inout) :: coarse_graining

    character(len=256) :: error_message
    logical :: exists
    integer :: error_code, iostat

    read(input_nml_file, coarse_graining_nml, iostat=iostat)
    error_code = check_nml_error(iostat, 'coarse_graining_nml')
    coarse_graining%do_coarse_graining = do_coarse_graining

    if (do_coarse_graining) then
       call assert_valid_strategy(strategy)
       call compute_nx_coarse(npx, coarsening_factor, nx_coarse)
       call assert_valid_domain_layout(nx_coarse, layout)
       call define_cubic_mosaic(coarse_domain, nx_coarse, nx_coarse, layout)
       call mpp_define_io_domain(coarse_domain, coarse_io_layout)
       call mpp_get_compute_domain(coarse_domain, is_coarse, ie_coarse, js_coarse, je_coarse)
       call get_fine_array_bounds(bd, is, ie, js, je)
       npz = atm_npz
       
       coarse_graining%bd%is_coarse = is_coarse
       coarse_graining%bd%ie_coarse = ie_coarse
       coarse_graining%bd%js_coarse = js_coarse
       coarse_graining%bd%je_coarse = je_coarse
       coarse_graining%nx_coarse = nx_coarse
       coarse_graining%domain = coarse_domain
       coarse_graining%strategy = strategy
    endif    
  end subroutine coarse_graining_init

  subroutine compute_nx_coarse(npx, coarsening_factor, nx_coarse)
    integer, intent(in) :: npx
    integer, intent(in) :: coarsening_factor
    integer, intent(out) :: nx_coarse

    character(len=256) :: error_message
    integer :: nx
    
    nx = npx - 1
    if (mod(nx, coarsening_factor) > 0) then
       write(error_message, *) 'coarse_graining_init: coarsening_factor does not evenly divide the native resolution.'
       call mpp_error(FATAL, error_message)
    endif
    nx_coarse = nx / coarsening_factor
  end subroutine compute_nx_coarse

  subroutine assert_valid_domain_layout(nx_coarse, layout)
    integer, intent(in) :: nx_coarse
    integer, intent(in) :: layout(2)

    character(len=256) :: error_message
    integer :: layout_x, layout_y
    layout_x = layout(1)
    layout_y = layout(2)

    if (mod(nx_coarse, layout_x) > 0 .or. mod(nx_coarse, layout_y) > 0) then 
       write(error_message, *) 'coarse_graining_init: domain decomposition layout does not evenly divide the coarse grid.'
       call mpp_error(FATAL, error_message)
    endif
  end subroutine assert_valid_domain_layout

  subroutine assert_valid_strategy(strategy)
    character(len=64), intent(in) :: strategy

    character(len=256) :: error_message

    if (trim(strategy) .ne. MODEL_LEVEL) then
       write(error_message, *) 'Invalid coarse graining strategy provided.'
       call mpp_error(FATAL, error_message)
    endif
  end subroutine assert_valid_strategy

  subroutine get_fine_array_bounds(bd, is, ie, js, je)
    type(fv_grid_bounds_type), intent(in) :: bd
    integer, intent(out) :: is, ie, js, je

    is = bd%is
    ie = bd%ie
    js = bd%js
    je = bd%je
  end subroutine get_fine_array_bounds
  
  subroutine get_coarse_array_bounds(coarse_bd, is_coarse, ie_coarse, js_coarse, je_coarse)
    type(fv_coarse_grid_bounds_type), intent(in) :: coarse_bd
    integer, intent(out) :: is_coarse, ie_coarse, js_coarse, je_coarse

    is_coarse = coarse_bd%is_coarse
    ie_coarse = coarse_bd%ie_coarse
    js_coarse = coarse_bd%js_coarse
    je_coarse = coarse_bd%je_coarse
  end subroutine get_coarse_array_bounds
  
  subroutine block_sum_2d(fine, coarse)
    real, intent(in) :: fine(is:ie,js:je)
    real, intent(out) :: coarse(is_coarse:ie_coarse,js_coarse:je_coarse)

    integer :: i, j, i_coarse, j_coarse, offset

    offset = coarsening_factor - 1
    do i = is, ie, coarsening_factor
       i_coarse = (i - 1) / coarsening_factor + 1
       do j = js, je, coarsening_factor
          j_coarse = (j - 1) / coarsening_factor + 1
          coarse(i_coarse,j_coarse) = sum(fine(i:i+offset,j:j+offset))
       enddo
    enddo
  end subroutine
  
  subroutine weighted_block_average_2d(weights, fine, coarse)
    real, intent(in) :: weights(is:ie,js:je), fine(is:ie,js:je)
    real, intent(out) :: coarse(is_coarse:ie_coarse,js_coarse:je_coarse)

    real, allocatable :: weighted_fine(:,:), weighted_block_sum(:,:), block_sum_weights(:,:)

    allocate(weighted_fine(is:ie,js:je))
    allocate(weighted_block_sum(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(block_sum_weights(is_coarse:ie_coarse,js_coarse:je_coarse))

    weighted_fine = weights * fine
    call block_sum_2d(weighted_fine, weighted_block_sum)
    call block_sum_2d(weights, block_sum_weights)
    coarse = weighted_block_sum / block_sum_weights
  end subroutine weighted_block_average_2d

  subroutine weighted_block_average_3d_field_2d_weights(weights, fine, coarse)
    real, intent(in) :: weights(is:ie,js:je), fine(is:ie,js:je,1:npz)
    real, intent(out) :: coarse(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz)

    integer :: k

    do k = 1, npz
       call weighted_block_average_2d(weights, fine(is:ie,js:je,k), coarse(is_coarse:ie_coarse,js_coarse:je_coarse,k))
    enddo
  end subroutine weighted_block_average_3d_field_2d_weights

  ! This subroutine is copied from FMS/test_fms/horiz_interp/test2_horiz_interp.F90.
  ! domain_decomp in fv_mp_mod.F90 does something similar, but it does a
  ! few other unnecessary things (and requires more arguments).
  subroutine define_cubic_mosaic(domain, ni, nj, layout)
    type(domain2d), intent(inout) :: domain
    integer,        intent(in)    :: layout(2)
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
  end subroutine define_cubic_mosaic
  
end module coarse_graining_mod
