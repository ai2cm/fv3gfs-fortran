module coarse_grained_diagnostics_mod

  use diag_manager_mod, only: diag_axis_init, register_diag_field, send_data
  use fv_arrays_mod, only: fv_atmos_type, fv_coarse_diag_type
  use mpp_domains_mod, only: domain2d
  use online_coarse_graining_mod, only: weighted_block_average
  use time_manager_mod, only: time_type
  
  implicit none
  private
  
  public :: fv_coarse_diag_init, fv_coarse_diag

contains

  subroutine fv_coarse_diag_init(Atm, coarse_axes_t, Time)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    integer, intent(inout) :: coarse_axes_t(4)
    type(time_type), intent(in) :: Time
    type(domain2d) :: coarse_domain
    integer :: tile_count
    integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse
    integer :: npz, target_resolution
    
    tile_count = 1
    target_resolution = Atm(tile_count)%target_coarse_resolution
    coarse_domain = Atm(tile_count)%coarse_domain

    call get_fine_array_bounds(Atm(tile_count), is, ie, js, je)
    call get_coarse_array_bounds(Atm(tile_count), is_coarse, ie_coarse, js_coarse, je_coarse)
    npz = Atm(tile_count)%npz
    
    call register_coarse_grained_diagnostics(Atm, coarse_axes_t, Time, &
         coarse_domain, target_resolution, is, ie, js, je, is_coarse, &
         ie_coarse, js_coarse, je_coarse, npz)
  end subroutine fv_coarse_diag_init

  subroutine fv_coarse_diag(Atm, Time)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    type(time_type), intent(in) :: Time
    integer :: tile_count
    integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse
    integer :: npz
 
    tile_count = 1

    call get_fine_array_bounds(Atm(tile_count), is, ie, js, je)
    call get_coarse_array_bounds(Atm(tile_count), is_coarse, ie_coarse, js_coarse, je_coarse)
    npz = Atm(tile_count)%npz
    
    call compute_coarse_grained_diagnostics(Atm, Time, is, ie, js, je, &
         is_coarse, ie_coarse, js_coarse, je_coarse, npz)
  end subroutine fv_coarse_diag
  
  subroutine register_coarse_grained_diagnostics(Atm, coarse_axes_t, Time, &
         coarse_domain, target_resolution, is, ie, js, je, is_coarse, &
         ie_coarse, js_coarse, je_coarse, npz)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    integer, intent(out) :: coarse_axes_t(4)
    type(time_type), intent(in) :: Time
    type(domain2d), intent(in) :: coarse_domain
    integer, intent(in) :: target_resolution
    integer, intent(in) :: is, ie, js, je
    integer, intent(in) :: is_coarse, ie_coarse, js_coarse, je_coarse
    integer, intent(in) :: npz

    integer :: id_coarse_x, id_coarse_y, id_coarse_xt, id_coarse_yt
    integer :: tile_count
    integer :: coarse_axes(4)
    integer :: id_pfull, id_phalf
    real :: missing_value

    missing_value = -1.0e10  ! Following fv_diagnostics.F90
    tile_count = 1  ! Following fv_diagnostics.F90
    
    call initialize_coarse_diagnostic_axes(coarse_domain, &
         target_resolution, tile_count, &
         id_coarse_x, id_coarse_y, id_coarse_xt, id_coarse_yt)

    id_pfull = Atm(tile_count)%atmos_axes(3)
    id_phalf = Atm(tile_count)%atmos_axes(4)
    
    coarse_axes(1) = id_coarse_x
    coarse_axes(2) = id_coarse_y
    coarse_axes(3) = id_pfull
    coarse_axes(4) = id_phalf

    coarse_axes_t(1) = id_coarse_xt
    coarse_axes_t(2) = id_coarse_yt
    coarse_axes_t(3) = id_pfull
    coarse_axes_t(4) = id_phalf
    
    Atm(tile_count)%idiag_coarse%id_omega_coarse = register_diag_field('dynamics', &
         'omega_coarse', coarse_axes_t(1:3), Time, &
         'coarse-grained omega', &
         'Pa/s', missing_value=missing_value)
  end subroutine register_coarse_grained_diagnostics

  subroutine initialize_coarse_diagnostic_axes(coarse_domain, &
    target_resolution, tile_count, &
    id_coarse_x, id_coarse_y, id_coarse_xt, id_coarse_yt)
    type(domain2d), intent(in) :: coarse_domain
    integer, intent(in) :: target_resolution, tile_count
    integer, intent(out) :: id_coarse_x, id_coarse_y, id_coarse_xt, id_coarse_yt

    integer :: i, j
    real, allocatable :: grid_x_coarse(:), grid_y_coarse(:), grid_xt_coarse(:), grid_yt_coarse(:)

    allocate(grid_x_coarse(target_resolution + 1))
    allocate(grid_y_coarse(target_resolution + 1))
    allocate(grid_xt_coarse(target_resolution))
    allocate(grid_yt_coarse(target_resolution))

    grid_x_coarse = (/ (i, i=1, target_resolution + 1) /)
    grid_y_coarse = (/ (j, j=1, target_resolution + 1) /)
    grid_xt_coarse = (/ (i, i=1, target_resolution) /)
    grid_yt_coarse = (/ (j, j=1, target_resolution) /)
    
    id_coarse_x = diag_axis_init('grid_x_coarse', grid_x_coarse, &
         'degrees_E', 'x', 'Corner longitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)
    id_coarse_y = diag_axis_init('grid_y_coarse', grid_y_coarse, &
         'degrees_N', 'y', 'Corner latitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)

    id_coarse_xt = diag_axis_init('grid_xt_coarse', grid_xt_coarse, &
         'degrees_E', 'x', 'T-cell longitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)
    id_coarse_yt = diag_axis_init('grid_yt_coarse', grid_yt_coarse, &
         'degrees_N', 'y', 'T-cell latitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)
    
  end subroutine initialize_coarse_diagnostic_axes

  subroutine compute_coarse_grained_diagnostics(Atm, Time, is, ie, js, je,&
       & is_coarse, ie_coarse, js_coarse, je_coarse, npz)
    type(fv_atmos_type), intent(in), target :: Atm(:)
    type(time_type), intent(in) :: Time
    integer, intent(in) :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse
    integer, intent(in) :: npz
    
    real, allocatable :: work_3d_coarse(:,:,:)
    logical :: used
    integer :: tile_count

    tile_count = 1  ! Following fv_diag
    allocate(work_3d_coarse(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz))

    if (Atm(tile_count)%idiag_coarse%id_omega_coarse > 0) then
       call weighted_block_average( &
            Atm(tile_count)%gridstruct%area(is:ie,js:je), &
            Atm(tile_count)%omga(is:ie,js:je,1:npz), &
            work_3d_coarse)
       used = send_data(Atm(tile_count)%idiag_coarse%id_omega_coarse, work_3d_coarse, Time)
    endif    
  end subroutine compute_coarse_grained_diagnostics

  subroutine get_fine_array_bounds(Atm, is, ie, js, je)
    type(fv_atmos_type), intent(in) :: Atm
    integer, intent(out) :: is, ie, js, je

    is = Atm%bd%is
    ie = Atm%bd%ie
    js = Atm%bd%js
    je = Atm%bd%je
  end subroutine get_fine_array_bounds
  
  subroutine get_coarse_array_bounds(Atm, is_coarse, ie_coarse, js_coarse, je_coarse)
    type(fv_atmos_type), intent(in) :: Atm
    integer, intent(out) :: is_coarse, ie_coarse, js_coarse, je_coarse

    is_coarse = Atm%coarse_bd%is_coarse
    ie_coarse = Atm%coarse_bd%ie_coarse
    js_coarse = Atm%coarse_bd%js_coarse
    je_coarse = Atm%coarse_bd%je_coarse
  end subroutine get_coarse_array_bounds

end module coarse_grained_diagnostics_mod
