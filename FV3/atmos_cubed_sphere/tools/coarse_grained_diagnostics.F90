module coarse_grained_diagnostics_mod

  use diag_manager_mod, only: diag_axis_init, register_diag_field, register_static_field, send_data
  use fv_arrays_mod, only: fv_atmos_type, fv_coarse_diag_type
  use mpp_domains_mod, only: domain2d
  use mpp_mod, only: FATAL, mpp_error
  use online_coarse_graining_mod, only: block_sum, get_fine_array_bounds, get_coarse_array_bounds, MODEL_LEVEL, weighted_block_average
  use time_manager_mod, only: time_type
  
  implicit none
  private
  
  public :: fv_coarse_diag_init, fv_coarse_diag

  integer :: tile_count = 1  ! Following fv_diagnostics.F90

contains
  
  subroutine fv_coarse_diag_init(Atm, coarse_axes_t, Time)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    integer, intent(out) :: coarse_axes_t(4)
    type(time_type), intent(in) :: Time

    integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse
    integer :: id_x_coarse, id_y_coarse, id_xt_coarse, id_yt_coarse
    integer :: coarse_axes(4)
    integer :: id_pfull, id_phalf

    call get_fine_array_bounds(Atm(tile_count), is, ie, js, je)
    call get_coarse_array_bounds(Atm(tile_count), is_coarse, ie_coarse, js_coarse, je_coarse)    
    call initialize_coarse_diagnostic_axes( &
         Atm(tile_count)%coarse_graining_attributes%coarse_domain, &
         Atm(tile_count)%coarse_graining_attributes%target_coarse_resolution, &         
         id_x_coarse, id_y_coarse, id_xt_coarse, id_yt_coarse)

    id_pfull = Atm(tile_count)%atmos_axes(3)
    id_phalf = Atm(tile_count)%atmos_axes(4)

    ! We don't use these in this minimal example, but they will be needed
    ! later.  These are the axes for variables that live on grid cell edges.
    coarse_axes(1) = id_x_coarse
    coarse_axes(2) = id_y_coarse
    coarse_axes(3) = id_pfull
    coarse_axes(4) = id_phalf

    ! These are the axes for values that live on grid cell centers.
    coarse_axes_t(1) = id_xt_coarse
    coarse_axes_t(2) = id_yt_coarse
    coarse_axes_t(3) = id_pfull
    coarse_axes_t(4) = id_phalf

    call register_coarse_diagnostics(Atm, Time, coarse_axes_t)
  end subroutine fv_coarse_diag_init

  subroutine initialize_coarse_diagnostic_axes(coarse_domain, &
    target_resolution, id_x_coarse, id_y_coarse, id_xt_coarse, id_yt_coarse)
    type(domain2d), intent(in) :: coarse_domain
    integer, intent(in) :: target_resolution
    integer, intent(out) :: id_x_coarse, id_y_coarse, id_xt_coarse, id_yt_coarse

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
    
    id_x_coarse = diag_axis_init('grid_x_coarse', grid_x_coarse, &
         'degrees_E', 'x', 'Corner longitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)
    id_y_coarse = diag_axis_init('grid_y_coarse', grid_y_coarse, &
         'degrees_N', 'y', 'Corner latitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)

    id_xt_coarse = diag_axis_init('grid_xt_coarse', grid_xt_coarse, &
         'degrees_E', 'x', 'T-cell longitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)
    id_yt_coarse = diag_axis_init('grid_yt_coarse', grid_yt_coarse, &
         'degrees_N', 'y', 'T-cell latitude', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)    
  end subroutine initialize_coarse_diagnostic_axes
  
  subroutine register_coarse_diagnostics(Atm, Time, coarse_axes_t)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    type(time_type), intent(in) :: Time
    integer, intent(in) :: coarse_axes_t(4)

    real :: missing_value = -1.0e10  ! Following fv_diagnostics.F90
    
    Atm(tile_count)%idiag_coarse%id_omega_coarse = register_diag_field('dynamics', &
         'omega_coarse', coarse_axes_t(1:3), Time, &
         'coarse-grained omega', &
         'Pa/s', missing_value=missing_value)
  end subroutine register_coarse_diagnostics
  
  subroutine fv_coarse_diag(Atm, Time)
    type(fv_atmos_type), intent(in), target :: Atm(:)
    type(time_type), intent(in) :: Time
 
    character(len=256) :: error_message

    if (trim(Atm(tile_count)%coarse_graining_attributes%coarse_graining_strategy) .eq. MODEL_LEVEL) then
       call fv_coarse_diag_model_levels(Atm, Time)
    else
       write(error_message, *) 'Invalid coarse_graining_strategy provided.'
       call mpp_error(FATAL, error_message)
    endif    
  end subroutine fv_coarse_diag
  
  subroutine fv_coarse_diag_model_levels(Atm, Time)
    type(fv_atmos_type), intent(in), target :: Atm(:)
    type(time_type), intent(in) :: Time
    
    real, allocatable :: work_3d_coarse(:,:,:)
    integer :: diagnostic_ids_3d(Atm(tile_count)%idiag_coarse%n_3d_diagnostics)
    integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse, npz
    logical :: used

    call get_diagnostic_ids_3d(Atm, diagnostic_ids_3d)
    call get_fine_array_bounds(Atm(tile_count), is, ie, js, je)
    call get_coarse_array_bounds(Atm(tile_count), is_coarse, ie_coarse, js_coarse, je_coarse)
    npz = Atm(tile_count)%npz

    if (any(diagnostic_ids_3d > 0)) then
       allocate(work_3d_coarse(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz))       
    endif

    if (Atm(tile_count)%idiag_coarse%id_omega_coarse > 0) then
       call weighted_block_average( &
            Atm(tile_count)%gridstruct%area(is:ie,js:je), &
            Atm(tile_count)%omga(is:ie,js:je,1:npz), &
            work_3d_coarse)
       used = send_data(Atm(tile_count)%idiag_coarse%id_omega_coarse, work_3d_coarse, Time)
    endif    
  end subroutine fv_coarse_diag_model_levels
  
  subroutine get_diagnostic_ids_3d(Atm, diagnostic_ids_3d)
    type(fv_atmos_type), intent(in) :: Atm(:)
    integer, intent(out) :: diagnostic_ids_3d(Atm(tile_count)%idiag_coarse%n_3d_diagnostics)

    diagnostic_ids_3d = (/ Atm(tile_count)%idiag_coarse%id_omega_coarse /)
  end subroutine 
  
end module coarse_grained_diagnostics_mod
