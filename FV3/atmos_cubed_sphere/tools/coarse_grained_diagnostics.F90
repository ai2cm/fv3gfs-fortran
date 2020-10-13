module coarse_grained_diagnostics_mod

  use diag_manager_mod, only: diag_axis_init, register_diag_field, register_static_field, send_data
  use field_manager_mod,  only: MODEL_ATMOS
  use fv_arrays_mod, only: fv_atmos_type, fv_coarse_diag_type, fv_coarse_graining_type
  use mpp_domains_mod, only: domain2d
  use mpp_mod, only: FATAL, mpp_error
  use coarse_graining_mod, only: block_sum, get_fine_array_bounds, get_coarse_array_bounds, MODEL_LEVEL, &
                                 weighted_block_average, PRESSURE_LEVEL, vertically_remap_field, &
                                 vertical_remapping_requirements, mask_area_weights, mask_mass_weights
  use time_manager_mod, only: time_type
  use tracer_manager_mod, only: get_tracer_names
  
  implicit none
  private
  
  type data_subtype
    real, dimension(:,:),   pointer :: var2 => null()
    real, dimension(:,:,:), pointer :: var3 => null()
  end type data_subtype

  type coarse_diag_type
    integer :: id = -99
    integer :: axes  ! 2 or 3, depending on whether the variable is 2D or 3D
    character(len=64) :: module_name
    character(len=128) :: name
    character(len=128) :: description
    character(len=64) :: units
    character(len=64) :: reduction_method
    type(data_subtype) :: data
  end type coarse_diag_type

  public :: fv_coarse_diag_init, fv_coarse_diag

  integer :: tile_count = 1  ! Following fv_diagnostics.F90
  integer :: DIAG_SIZE = 64
  type(coarse_diag_type), dimension(64) :: coarse_diagnostics

  ! Reduction methods
  character(len=11) :: AREA_WEIGHTED = 'area_weighted'
  character(len=11) :: MASS_WEIGHTED = 'mass_weighted'

contains

  subroutine populate_coarse_diag_type(Atm, coarse_diagnostics)
    type(fv_atmos_type), intent(in), target :: Atm(:)
    type(coarse_diag_type), intent(out) :: coarse_diagnostics(:)

    integer :: is, ie, js, je, npz, n_tracers, n_prognostic, t
    integer :: index = 1
    character(len=128) :: tracer_name
    character(len=256) :: tracer_long_name, tracer_units
    character(len=8) :: DYNAMICS = 'dynamics'

    npz = Atm(tile_count)%npz
    n_prognostic = size(Atm(tile_count)%q, 4)
    n_tracers = Atm(tile_count)%ncnst
    call get_fine_array_bounds(is, ie, js, je)

    coarse_diagnostics(index)%axes = 3
    coarse_diagnostics(index)%module_name = DYNAMICS
    coarse_diagnostics(index)%name = 'omega_coarse'
    coarse_diagnostics(index)%description = 'coarse-grained pressure velocity'
    coarse_diagnostics(index)%units = 'Pa/s'
    coarse_diagnostics(index)%reduction_method = AREA_WEIGHTED
    coarse_diagnostics(index)%data%var3 => Atm(tile_count)%omga(is:ie,js:je,1:npz)

    index = index + 1
    coarse_diagnostics(index)%axes = 3
    coarse_diagnostics(index)%module_name = DYNAMICS
    coarse_diagnostics(index)%name = 'ucomp_coarse'
    coarse_diagnostics(index)%description = 'coarse-grained zonal wind'
    coarse_diagnostics(index)%units = 'm/s'
    coarse_diagnostics(index)%reduction_method = MASS_WEIGHTED
    coarse_diagnostics(index)%data%var3 => Atm(tile_count)%ua(is:ie,js:je,1:npz)

    index = index + 1
    coarse_diagnostics(index)%axes = 3
    coarse_diagnostics(index)%module_name = DYNAMICS
    coarse_diagnostics(index)%name = 'vcomp_coarse'
    coarse_diagnostics(index)%description = 'coarse-grained meridional wind'
    coarse_diagnostics(index)%units = 'm/s'
    coarse_diagnostics(index)%reduction_method = MASS_WEIGHTED
    coarse_diagnostics(index)%data%var3 => Atm(tile_count)%va(is:ie,js:je,1:npz)

    do t = 1, n_tracers
      call get_tracer_names(MODEL_ATMOS, t, tracer_name, tracer_long_name, tracer_units)
      index = index + 1
      coarse_diagnostics(index)%axes = 3
      coarse_diagnostics(index)%module_name = DYNAMICS
      coarse_diagnostics(index)%name = trim(tracer_name) // '_coarse'
      coarse_diagnostics(index)%description = 'coarse-grained ' // trim(tracer_long_name)
      coarse_diagnostics(index)%units = tracer_units
      coarse_diagnostics(index)%reduction_method = MASS_WEIGHTED
      if (t .gt. n_prognostic) then
        coarse_diagnostics(index)%data%var3 => Atm(tile_count)%qdiag(is:ie,js:je,1:npz,t)
      else
        coarse_diagnostics(index)%data%var3 => Atm(tile_count)%q(is:ie,js:je,1:npz,t)
      endif
    enddo
  end subroutine populate_coarse_diag_type

  subroutine register_coarse_diagnostics(coarse_diagnostics, Time, id_xt_coarse, id_yt_coarse, id_pfull_coarse)
    type(coarse_diag_type), intent(inout) :: coarse_diagnostics(:)
    type(time_type), intent(in) :: Time
    integer, intent(in) :: id_xt_coarse, id_yt_coarse, id_pfull_coarse

    integer :: index, n_valid_diagnostics
    integer :: axes(3)
    real :: missing_value = -1.0e10  ! Following fv_diagnostics.F90

    axes = (/  id_xt_coarse, id_yt_coarse, id_pfull_coarse /)
    do index = 1, DIAG_SIZE
      if (trim(coarse_diagnostics(index)%name) == '') exit
      n_valid_diagnostics = index
    enddo 

    do index = 1, n_valid_diagnostics
      coarse_diagnostics(index)%id = register_diag_field( &
        trim(coarse_diagnostics(index)%module_name), &
        trim(coarse_diagnostics(index)%name), &
        axes(1:coarse_diagnostics(index)%axes), &
        Time, &
        trim(coarse_diagnostics(index)%description), &
        trim(coarse_diagnostics(index)%units), &
        missing_value=missing_value &
      )
    enddo 
  end subroutine register_coarse_diagnostics
     
  subroutine fv_coarse_diag_init(Atm, Time, id_pfull, id_phalf, coarse_graining)
    type(fv_atmos_type), intent(in) :: Atm(:)
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

    call populate_coarse_diag_type(Atm, coarse_diagnostics)
    call register_coarse_diagnostics(coarse_diagnostics, Time, &
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
    
    real, allocatable :: work_2d_coarse(:,:), work_3d_coarse(:,:,:), mass(:,:,:)
    integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse, npz
    logical :: used
    logical :: need_2d_work_array, need_3d_work_array, need_mass_array
    integer :: index, i, j
    character(len=256) :: error_message

    call get_need_nd_work_array(2, need_2d_work_array)
    call get_need_nd_work_array(3, need_3d_work_array)
    call get_need_mass_array(need_mass_array)

    call get_fine_array_bounds(is, ie, js, je)
    call get_coarse_array_bounds(is_coarse, ie_coarse, js_coarse, je_coarse)
    npz = Atm(tile_count)%npz

    if (need_2d_work_array) then
      allocate(work_2d_coarse(is_coarse:ie_coarse,js_coarse:je_coarse))
    endif

    if (need_3d_work_array) then
       allocate(work_3d_coarse(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz))       
    endif

    if (need_mass_array) then
      allocate(mass(is:ie,js:je,1:npz))
      call compute_mass(Atm(tile_count), is, ie, js, je, npz, mass)
    endif

    do index = 1, DIAG_SIZE
      if (coarse_diagnostics(index)%id .gt. 0) then
        if (coarse_diagnostics(index)%axes .eq. 2) then
          if (trim(coarse_diagnostics(index)%reduction_method) .eq. AREA_WEIGHTED) then
            call weighted_block_average( &
              Atm(tile_count)%gridstruct%area(is:ie,js:je), &
              coarse_diagnostics(index)%data%var2, &
              work_2d_coarse &
            )
          else
            write(error_message, *) 'fv_coarse_diag_model_levels: invalid reduction_method, ' // &
              trim(coarse_diagnostics(index)%reduction_method) // ', provided for 2D variable, ' // &
              trim(coarse_diagnostics(index)%name)
            call mpp_error(FATAL, error_message)
          endif
          used = send_data(coarse_diagnostics(index)%id, work_2d_coarse, Time)
        elseif (coarse_diagnostics(index)%axes .eq. 3) then
          if (trim(coarse_diagnostics(index)%reduction_method) .eq. AREA_WEIGHTED) then
            call weighted_block_average( &
              Atm(tile_count)%gridstruct%area(is:ie,js:je), &
              coarse_diagnostics(index)%data%var3, &
              work_3d_coarse &
            )
          elseif (trim(coarse_diagnostics(index)%reduction_method) .eq. MASS_WEIGHTED) then
            call weighted_block_average( &
              mass(is:ie,js:je,1:npz), &
              coarse_diagnostics(index)%data%var3, &
              work_3d_coarse &
            )
          else
            write(error_message, *) 'fv_coarse_diag_model_levels: invalid reduction_method, ' // &
              trim(coarse_diagnostics(index)%reduction_method) // ', provided for 3D variable, ' // &
              trim(coarse_diagnostics(index)%name)
            call mpp_error(FATAL, error_message)
          endif
          used = send_data(coarse_diagnostics(index)%id, work_3d_coarse, Time)
        endif
      endif
    enddo 
  end subroutine fv_coarse_diag_model_levels
  
  subroutine fv_coarse_diag_pressure_levels(Atm, Time)
     type(fv_atmos_type), intent(in), target :: Atm(:)
     type(time_type), intent(in) :: Time
     
     real, allocatable, dimension(:,:) :: work_2d_coarse
     real, allocatable, dimension(:,:,:) :: work_3d_coarse
     real, allocatable, dimension(:,:,:) :: remapped_field, phalf, upsampled_coarse_phalf
     real, allocatable, dimension(:,:,:) :: masked_area_weights, masked_mass_weights
     integer :: is, ie, js, je, is_coarse, ie_coarse, js_coarse, je_coarse, npz, index
     logical :: need_2d_work_array, need_3d_work_array, need_masked_area_array, need_masked_mass_array, used
     character(len=256) :: error_message
 
     call get_need_nd_work_array(2, need_2d_work_array)
     call get_need_nd_work_array(3, need_3d_work_array)
     call get_need_mass_array(need_masked_mass_array)
     call get_need_masked_area_array(need_masked_area_array)
 
     call get_fine_array_bounds(is, ie, js, je)
     call get_coarse_array_bounds(is_coarse, ie_coarse, js_coarse, je_coarse)
     npz = Atm(tile_count)%npz
 
     if (need_2d_work_array) then
       allocate(work_2d_coarse(is_coarse:ie_coarse,js_coarse:je_coarse))
     endif

     if (need_3d_work_array) then
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

     if (need_masked_area_array) then
        allocate(masked_area_weights(is:ie,js:je,1:npz))
        call mask_area_weights( &
             Atm(tile_count)%gridstruct%area(is:ie,js:je), &
             phalf, &
             upsampled_coarse_phalf, &
             masked_area_weights)
     endif

     if (need_masked_mass_array) then
        allocate(masked_mass_weights(is:ie,js:je,1:npz))
        call mask_mass_weights( &
             Atm(tile_count)%gridstruct%area(is:ie,js:je), &
             Atm(tile_count)%delp(is:ie,js:je,1:npz), &
             phalf, &
             upsampled_coarse_phalf, &
             masked_mass_weights)
     endif

     do index = 1, DIAG_SIZE
      if (coarse_diagnostics(index)%id .gt. 0) then
        if (coarse_diagnostics(index)%axes .eq. 2) then
          if (trim(coarse_diagnostics(index)%reduction_method) .eq. AREA_WEIGHTED) then
            call weighted_block_average( &
              Atm(tile_count)%gridstruct%area(is:ie,js:je), &
              coarse_diagnostics(index)%data%var2, &
              work_2d_coarse &
            ) 
          else
            write(error_message, *) 'fv_coarse_diag_pressure_levels: invalid reduction_method, ' // &
              trim(coarse_diagnostics(index)%reduction_method) // ', provided for 2D variable, ' // &
              trim(coarse_diagnostics(index)%name)
            call mpp_error(FATAL, error_message)
          endif
          used = send_data(coarse_diagnostics(index)%id, work_2d_coarse, Time)
        elseif (coarse_diagnostics(index)%axes .eq. 3) then
          call vertically_remap_field( &
            phalf, &
            coarse_diagnostics(index)%data%var3, &
            upsampled_coarse_phalf, &
            Atm(tile_count)%ptop, &
            remapped_field)
          if (trim(coarse_diagnostics(index)%reduction_method) .eq. AREA_WEIGHTED) then
            call weighted_block_average( &
              masked_area_weights(is:ie,js:je,1:npz), &
              remapped_field(is:ie,js:je,1:npz), &
              work_3d_coarse &
            )
          elseif (trim(coarse_diagnostics(index)%reduction_method) .eq. MASS_WEIGHTED) then
            call weighted_block_average( &
              masked_mass_weights(is:ie,js:je,1:npz), &
              remapped_field(is:ie,js:je,1:npz), &
              work_3d_coarse &
            )
          else
            write(error_message, *) 'fv_coarse_diag_pressure_levels: invalid reduction_method, ' // &
              trim(coarse_diagnostics(index)%reduction_method) // ', provided for 3D variable, ' // &
              trim(coarse_diagnostics(index)%name)
            call mpp_error(FATAL, error_message)
          endif
          used = send_data(coarse_diagnostics(index)%id, work_3d_coarse, Time)
        endif
      endif
    enddo 
   end subroutine fv_coarse_diag_pressure_levels

   subroutine get_need_nd_work_array(dimension, need_nd_work_array)
     integer, intent(in) :: dimension
     logical, intent(out) :: need_nd_work_array

     integer :: index

     need_nd_work_array = .false.
     do index = 1, DIAG_SIZE
       if ((coarse_diagnostics(index)%axes == dimension) .and. (coarse_diagnostics(index)%id > 0)) then
         need_nd_work_array = .true.
         exit
       endif
     enddo
   end subroutine get_need_nd_work_array

   subroutine get_need_mass_array(need_mass_array)
     logical, intent(out) :: need_mass_array

     integer :: index

     need_mass_array = .false.
     do index = 1, DIAG_SIZE
       if ((coarse_diagnostics(index)%axes == 3) .and. & 
           (trim(coarse_diagnostics(index)%reduction_method) .eq. MASS_WEIGHTED) .and. &
           (coarse_diagnostics(index)%id > 0)) then
           need_mass_array = .true.
           exit
       endif
     enddo
  end subroutine get_need_mass_array

  subroutine get_need_masked_area_array(need_masked_area_array)
    logical, intent(out) :: need_masked_area_array

    integer :: index

    need_masked_area_array = .false.
    do index = 1, DIAG_SIZE
      if ((coarse_diagnostics(index)%axes == 3) .and. & 
          (trim(coarse_diagnostics(index)%reduction_method) .eq. AREA_WEIGHTED) .and. &
          (coarse_diagnostics(index)%id > 0)) then
         need_masked_area_array = .true.
         exit
      endif
   enddo
 end subroutine get_need_masked_area_array

  subroutine compute_mass(Atm, is, ie, js, je, npz, mass)
    type(fv_atmos_type), intent(in) :: Atm
    integer, intent(in) :: is, ie, js, je, npz
    real, intent(out) :: mass(is:ie,js:je,1:npz)

    integer :: k

    do k = 1, npz
      mass(is:ie,js:je,k) = Atm%delp(is:ie,js:je,k) * Atm%gridstruct%area(is:ie,js:je)
    enddo
  end subroutine compute_mass

end module coarse_grained_diagnostics_mod
