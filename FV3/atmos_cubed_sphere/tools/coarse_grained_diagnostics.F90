module coarse_grained_diagnostics_mod

  use diag_manager_mod, only: diag_axis_init, register_diag_field, register_static_field, send_data
  use fv_arrays_mod, only: fv_atmos_type, fv_coarse_diag_type, fv_coarse_graining_type
  use mpp_domains_mod, only: domain2d
  use mpp_mod, only: FATAL, mpp_error
  use coarse_graining_mod, only: block_sum, get_fine_array_bounds, get_coarse_array_bounds, MODEL_LEVEL, &
                                 weighted_block_average, PRESSURE_LEVEL, vertically_remap_field, &
                                 vertical_remapping_requirements, mask_area_weights
  use time_manager_mod, only: time_type
  
  implicit none
  private
  
  public :: fv_coarse_diag_init, fv_coarse_diag

  integer :: tile_count = 1  ! Following fv_diagnostics.F90

contains
  
  subroutine fv_coarse_diag_init(Time, id_pfull, id_phalf, coarse_graining)
    type(time_type), intent(in) :: Time
    integer, intent(in) :: id_pfull, id_phalf
    type(fv_coarse_graining_type), intent(inout) :: coarse_graining

    integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse

    call get_fine_array_bounds(is, ie, js, je)
    call get_coarse_array_bounds(is_coarse, ie_coarse, js_coarse, je_coarse)
    call initialize_coarse_diagnostic_axes(coarse_graining%domain, coarse_graining%nx_coarse, &
         coarse_graining%id_x_coarse, coarse_graining%id_y_coarse, coarse_graining%id_xt_coarse, &
         coarse_graining%id_yt_coarse)
    
    coarse_graining%id_pfull = id_pfull
    coarse_graining%id_phalf = id_phalf

    call register_coarse_diagnostics(coarse_graining%idiag, Time, &
         coarse_graining%id_xt_coarse, coarse_graining%id_yt_coarse, id_pfull)
  end subroutine fv_coarse_diag_init

  subroutine initialize_coarse_diagnostic_axes(coarse_domain, &
    nx_coarse, id_x_coarse, id_y_coarse, id_xt_coarse, id_yt_coarse)
    type(domain2d), intent(in) :: coarse_domain
    integer, intent(in) :: nx_coarse
    integer, intent(inout) :: id_x_coarse, id_y_coarse, id_xt_coarse, id_yt_coarse

    integer :: i, j
    real, allocatable :: grid_x_coarse(:), grid_y_coarse(:), grid_xt_coarse(:), grid_yt_coarse(:)

    allocate(grid_x_coarse(nx_coarse + 1))
    allocate(grid_y_coarse(nx_coarse + 1))
    allocate(grid_xt_coarse(nx_coarse))
    allocate(grid_yt_coarse(nx_coarse))

    grid_x_coarse = (/ (i, i=1, nx_coarse + 1) /)
    grid_y_coarse = (/ (j, j=1, nx_coarse + 1) /)
    grid_xt_coarse = (/ (i, i=1, nx_coarse) /)
    grid_yt_coarse = (/ (j, j=1, nx_coarse) /)
    
    id_x_coarse = diag_axis_init('grid_x_coarse', grid_x_coarse, &
         'index', 'x', 'x-index of cell corner points', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)
    id_y_coarse = diag_axis_init('grid_y_coarse', grid_y_coarse, &
         'index', 'y', 'y-index of cell corner points', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)

    id_xt_coarse = diag_axis_init('grid_xt_coarse', grid_xt_coarse, &
         'index', 'x', 'x-index of cell center points', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)
    id_yt_coarse = diag_axis_init('grid_yt_coarse', grid_yt_coarse, &
         'index', 'y', 'y-index of cell center points', set_name='coarse_grid', &
         Domain2=coarse_domain, tile_count=tile_count)    
  end subroutine initialize_coarse_diagnostic_axes
  
  subroutine register_coarse_diagnostics(idiag_coarse, Time, id_xt_coarse,&
       id_yt_coarse, id_pfull)
    type(fv_coarse_diag_type), intent(inout) :: idiag_coarse
    type(time_type), intent(in) :: Time
    integer, intent(in) :: id_xt_coarse, id_yt_coarse, id_pfull

    integer :: coarse_axes_t(3)
    real :: missing_value = -1.0e10  ! Following fv_diagnostics.F90

    coarse_axes_t = (/ id_xt_coarse, id_yt_coarse, id_pfull /)
    idiag_coarse%id_omega_coarse = register_diag_field('dynamics', &
         'omega_coarse', coarse_axes_t(1:3), Time, &
         'coarse-grained omega', &
         'Pa/s', missing_value=missing_value)
  end subroutine register_coarse_diagnostics
  
  subroutine fv_coarse_diag(Atm, Time)
    type(fv_atmos_type), intent(in), target :: Atm(:)
    type(time_type), intent(in) :: Time
 
    character(len=256) :: error_message

    if (trim(Atm(tile_count)%coarse_graining%strategy) .eq. MODEL_LEVEL) then
       call fv_coarse_diag_model_levels(Atm, Time)
    else if (trim(Atm(tile_count)%coarse_graining%strategy) .eq. PRESSURE_LEVEL) then
       call fv_coarse_diag_pressure_levels(Atm, Time)
    endif
  end subroutine fv_coarse_diag
  
  subroutine fv_coarse_diag_model_levels(Atm, Time)
    type(fv_atmos_type), intent(in), target :: Atm(:)
    type(time_type), intent(in) :: Time
    
    real, allocatable :: work_3d_coarse(:,:,:)
    integer :: diagnostic_ids_3d(Atm(tile_count)%coarse_graining%idiag%n_3d_diagnostics)
    integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse, npz
    logical :: used

    call get_diagnostic_ids_3d(Atm(tile_count)%coarse_graining%idiag, diagnostic_ids_3d)
    call get_fine_array_bounds(is, ie, js, je)
    call get_coarse_array_bounds(is_coarse, ie_coarse, js_coarse, je_coarse)
    npz = Atm(tile_count)%npz

    if (any(diagnostic_ids_3d > 0)) then
       allocate(work_3d_coarse(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz))       
    endif

    if (Atm(tile_count)%coarse_graining%idiag%id_omega_coarse > 0) then
       call weighted_block_average( &
            Atm(tile_count)%gridstruct%area(is:ie,js:je), &
            Atm(tile_count)%omga(is:ie,js:je,1:npz), &
            work_3d_coarse)
       used = send_data(Atm(tile_count)%coarse_graining%idiag%id_omega_coarse, work_3d_coarse, Time)
    endif    
  end subroutine fv_coarse_diag_model_levels
  
  subroutine fv_coarse_diag_pressure_levels(Atm, Time)
     type(fv_atmos_type), intent(in), target :: Atm(:)
     type(time_type), intent(in) :: Time
     
     real, allocatable, dimension(:,:,:) :: work_3d_coarse, remapped_field, phalf, upsampled_coarse_phalf, masked_area_weights
     integer :: diagnostic_ids_3d(Atm(tile_count)%coarse_graining%idiag%n_3d_diagnostics)
     integer :: area_weighted_diagnostic_ids(1)
     integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse, npz
     logical :: used
 
     call get_diagnostic_ids_3d(Atm(tile_count)%coarse_graining%idiag, diagnostic_ids_3d)
     call get_area_weighted_diagnostic_ids(Atm(tile_count)%coarse_graining%idiag, area_weighted_diagnostic_ids)
     call get_fine_array_bounds(is, ie, js, je)
     call get_coarse_array_bounds(is_coarse, ie_coarse, js_coarse, je_coarse)
     npz = Atm(tile_count)%npz
 
     if (any(diagnostic_ids_3d > 0)) then
        allocate(work_3d_coarse(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz))
        allocate(remapped_field(is:ie,js:je,1:npz))
        allocate(phalf(is:ie,js:je,1:npz+1))      
        allocate(upsampled_coarse_phalf(is:ie,js:je,1:npz+1))

        call vertical_remapping_requirements( &
             Atm(tile_count)%delp(is:ie,js:je,1:npz), &
             Atm(tile_count)%gridstruct%area(is:ie,js:je), &
             Atm(tile_count)%ptop, &
             phalf, &
             upsampled_coarse_phalf)
     endif

     if (any(area_weighted_diagnostic_ids > 0)) then
          allocate(masked_area_weights(is:ie,js:je,1:npz))
          call mask_area_weights( &
             Atm(tile_count)%gridstruct%area(is:ie,js:je), &
             phalf, &
             upsampled_coarse_phalf, &
             masked_area_weights)
     endif
 
     if (Atm(tile_count)%coarse_graining%idiag%id_omega_coarse > 0) then
        call vertically_remap_field( &
             phalf, &
             Atm(tile_count)%omga(is:ie,js:je,1:npz), &
             upsampled_coarse_phalf, &
             Atm(tile_count)%ptop, &
             remapped_field)
        call weighted_block_average( &
             masked_area_weights, &
             remapped_field, &
             work_3d_coarse)
        used = send_data(Atm(tile_count)%coarse_graining%idiag%id_omega_coarse, work_3d_coarse, Time)
     endif    
   end subroutine fv_coarse_diag_pressure_levels

  subroutine get_diagnostic_ids_3d(idiag_coarse, diagnostic_ids_3d)
    type(fv_coarse_diag_type), intent(in) :: idiag_coarse
    integer, intent(out) :: diagnostic_ids_3d(idiag_coarse%n_3d_diagnostics)

    diagnostic_ids_3d = (/ idiag_coarse%id_omega_coarse /)
  end subroutine 

  subroutine get_area_weighted_diagnostic_ids(idiag_coarse, area_weighted_diagnostic_ids)
     type(fv_coarse_diag_type), intent(in) :: idiag_coarse
     integer, intent(out) :: area_weighted_diagnostic_ids(1)
 
     area_weighted_diagnostic_ids = (/ idiag_coarse%id_omega_coarse /)
   end subroutine
end module coarse_grained_diagnostics_mod
