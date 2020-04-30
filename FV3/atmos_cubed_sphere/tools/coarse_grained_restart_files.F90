module coarse_grained_restart_files_mod

  use coarse_graining_mod, only: compute_mass_weights, get_coarse_array_bounds,&
       get_fine_array_bounds, MODEL_LEVEL, weighted_block_average, &
       weighted_block_edge_average_x, weighted_block_edge_average_y
  use field_manager_mod, only: MODEL_ATMOS
  use fms_io_mod,      only: register_restart_field, save_restart
  use fv_arrays_mod, only: coarse_restart_type, fv_atmos_type, fv_coarse_grid_bounds_type, fv_grid_bounds_type
  use mpp_domains_mod, only: domain2d, EAST, NORTH
  use tracer_manager_mod, only: get_tracer_names, set_tracer_profile

  implicit none
  private

  public :: deallocate_coarse_restart_type, fv_coarse_restart_init, fv_io_write_restart_coarse

  ! Global variables for this module, initialized in fv_coarse_restart_init
  integer :: is, ie, js, je, npz
  integer :: is_coarse, ie_coarse, js_coarse, je_coarse
  integer :: n_prognostic_tracers, n_diagnostic_tracers, n_tracers

contains

  subroutine fv_coarse_restart_init(tile_count, nz, nt_prog, &
       nt_phys, hydrostatic, hybrid_z, agrid_vel_rst, fv_land, &
       restart_from_agrid_winds, coarse_domain, fine_bd, coarse_bd, restart)
    integer, intent(in) :: tile_count, nz, nt_prog, nt_phys
    logical, intent(in) :: hydrostatic, hybrid_z, agrid_vel_rst, fv_land, restart_from_agrid_winds
    type(domain2d), intent(inout) :: coarse_domain
    type(fv_grid_bounds_type), intent(in) :: fine_bd
    type(fv_coarse_grid_bounds_type), intent(in) :: coarse_bd
    type(coarse_restart_type), intent(inout) :: restart

    call get_fine_array_bounds(fine_bd, is, ie, js, je)
    call get_coarse_array_bounds(coarse_bd, is_coarse, ie_coarse, js_coarse, je_coarse)
    n_prognostic_tracers = nt_prog
    n_diagnostic_tracers = nt_phys
    n_tracers = nt_prog + nt_phys
    npz = nz

    call allocate_coarse_restart_type(hydrostatic, hybrid_z, &
         agrid_vel_rst, fv_land, restart_from_agrid_winds, restart)
    call register_coarse_restart_files(tile_count, hydrostatic, &
         hybrid_z, agrid_vel_rst, fv_land, restart_from_agrid_winds, &
         coarse_domain, restart)
  end subroutine fv_coarse_restart_init

  subroutine fv_io_write_restart_coarse(Atm, grids_on_this_pe, timestamp)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    logical, intent(in) :: grids_on_this_pe(:)
    character(len=*), optional, intent(in) :: timestamp

    integer :: tile_count, n_tiles

    n_tiles = size(Atm(:))

    do tile_count = 1, n_tiles
       if (.not. grids_on_this_pe(tile_count)) cycle
       call coarse_grain_restart_data(Atm(tile_count))
       call save_restart(Atm(tile_count)%coarse_graining%restart%fv_core_coarse, timestamp)
       call save_restart(Atm(tile_count)%coarse_graining%restart%fv_tracer_coarse, timestamp)
       call save_restart(Atm(tile_count)%coarse_graining%restart%fv_srf_wnd_coarse, timestamp)
       if (Atm(tile_count)%flagstruct%fv_land) then
          call save_restart(Atm(tile_count)%coarse_graining%restart%mg_drag_coarse, timestamp)
          call save_restart(Atm(tile_count)%coarse_graining%restart%fv_land_coarse, timestamp)
       endif
    enddo
  end subroutine fv_io_write_restart_coarse

  subroutine allocate_coarse_restart_type(hydrostatic, hybrid_z, agrid_vel_rst,&
       fv_land, restart_from_agrid_winds, restart)
    logical, intent(in) :: hydrostatic, hybrid_z, agrid_vel_rst, fv_land, restart_from_agrid_winds
    type(coarse_restart_type), intent(inout) :: restart

    allocate(restart%u(is_coarse:ie_coarse,js_coarse:je_coarse+1,npz))
    allocate(restart%v(is_coarse:ie_coarse+1,js_coarse:je_coarse,npz))
    allocate(restart%u_srf(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(restart%v_srf(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(restart%delp(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%pt(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%q(is_coarse:ie_coarse,js_coarse:je_coarse,npz,n_prognostic_tracers))
    allocate(restart%qdiag(is_coarse:ie_coarse,js_coarse:je_coarse,npz,n_prognostic_tracers+1:n_tracers))
    allocate(restart%phis(is_coarse:ie_coarse,js_coarse:je_coarse))

    if (agrid_vel_rst .or. restart_from_agrid_winds) then
       allocate(restart%ua(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
       allocate(restart%va(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    endif

    if (.not. hydrostatic) then
       allocate(restart%w(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
       allocate(restart%delz(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
       if (hybrid_z) allocate(restart%ze0(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    endif

    if (fv_land) then
       allocate(restart%sgh(is_coarse:ie_coarse,js_coarse:je_coarse))
       allocate(restart%oro(is_coarse:ie_coarse,js_coarse:je_coarse))
    endif
  end subroutine allocate_coarse_restart_type

  subroutine deallocate_coarse_restart_type(restart)
    type(coarse_restart_type), intent(inout) :: restart

    if (allocated(restart%u)) deallocate(restart%u)
    if (allocated(restart%v)) deallocate(restart%v)
    if (allocated(restart%w)) deallocate(restart%w)
    if (allocated(restart%pt)) deallocate(restart%pt)
    if (allocated(restart%delp)) deallocate(restart%delp)
    if (allocated(restart%delz)) deallocate(restart%delz)
    if (allocated(restart%ua)) deallocate(restart%ua)
    if (allocated(restart%va)) deallocate(restart%va)
    if (allocated(restart%phis)) deallocate(restart%phis)
    if (allocated(restart%q)) deallocate(restart%q)
    if (allocated(restart%qdiag)) deallocate(restart%qdiag)
    if (allocated(restart%u_srf)) deallocate(restart%u_srf)
    if (allocated(restart%v_srf)) deallocate(restart%v_srf)
    if (allocated(restart%sgh)) deallocate(restart%sgh)
    if (allocated(restart%oro)) deallocate(restart%oro)
    if (allocated(restart%ze0)) deallocate(restart%ze0)
  end subroutine deallocate_coarse_restart_type

  subroutine register_coarse_restart_files(tile_count, hydrostatic, &
       hybrid_z, agrid_vel_rst, fv_land, restart_from_agrid_winds, coarse_domain, restart)
    integer, intent(in) :: tile_count
    logical, intent(in) :: hydrostatic, hybrid_z, agrid_vel_rst, fv_land, restart_from_agrid_winds
    type(domain2d), intent(in) :: coarse_domain
    type(coarse_restart_type), intent(inout) :: restart

    call register_fv_core_coarse(tile_count, hydrostatic, hybrid_z, agrid_vel_rst, &
         restart_from_agrid_winds, coarse_domain, restart)
    call register_fv_tracer_coarse(tile_count, coarse_domain, restart)
    call register_fv_srf_wnd_coarse(tile_count, coarse_domain, restart)
    if (fv_land) then
       call register_mg_drag_coarse(tile_count, coarse_domain, restart)
       call register_fv_land_coarse(tile_count, coarse_domain, restart)
    endif
  end subroutine register_coarse_restart_files

  subroutine register_fv_core_coarse(tile_count, hydrostatic, hybrid_z, &
       agrid_vel_rst, restart_from_agrid_winds, coarse_domain, restart)
    integer, intent(in) :: tile_count
    logical, intent(in) :: hydrostatic, hybrid_z, agrid_vel_rst, restart_from_agrid_winds
    type(domain2d), intent(in) :: coarse_domain
    type(coarse_restart_type), intent(inout) :: restart

    character(len=64) :: filename
    integer :: id_restart

    filename = 'fv_core_coarse.res.nc'

    id_restart = register_restart_field(restart%fv_core_coarse, &
         filename, 'u', restart%u, domain=coarse_domain, position=NORTH, tile_count=tile_count)
    id_restart = register_restart_field(restart%fv_core_coarse, &
         filename, 'v', restart%v, domain=coarse_domain, position=EAST, tile_count=tile_count)

    if (.not. hydrostatic) then
       id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'W', restart%w, domain=coarse_domain, mandatory=.false., tile_count=tile_count)
       id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'DZ', restart%delz, domain=coarse_domain, mandatory=.false., tile_count=tile_count)
       if (hybrid_z) then
          id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'ZE0', restart%ze0, domain=coarse_domain, mandatory=.false., tile_count=tile_count)
       endif
    endif

    id_restart = register_restart_field(restart%fv_core_coarse, &
         filename, 'T', restart%pt, domain=coarse_domain, tile_count=tile_count)
    id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'delp', restart%delp, domain=coarse_domain, tile_count=tile_count)
    id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'phis', restart%phis, domain=coarse_domain, tile_count=tile_count)

    if (agrid_vel_rst) then
       id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'ua', restart%ua, domain=coarse_domain, mandatory=.false., tile_count=tile_count)
       id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'va', restart%va, domain=coarse_domain, mandatory=.false., tile_count=tile_count)
    endif

    if (restart_from_agrid_winds) then
       id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'ua', restart%ua, domain=coarse_domain, tile_count=tile_count)
       id_restart = register_restart_field(restart%fv_core_coarse, &
            filename, 'va', restart%va, domain=coarse_domain, tile_count=tile_count)
    endif
  end subroutine register_fv_core_coarse

  subroutine register_fv_tracer_coarse(tile_count, coarse_domain, restart)
    integer, intent(in) :: tile_count
    type(domain2d), intent(in) :: coarse_domain
    type(coarse_restart_type), intent(inout) :: restart

    character(len=64) :: filename, tracer_name
    integer :: id_restart, n_tracer

    filename = 'fv_tracer_coarse.res.nc'

    do n_tracer = 1, n_prognostic_tracers
       call get_tracer_names(MODEL_ATMOS, n_tracer, tracer_name)
       call set_tracer_profile(MODEL_ATMOS, n_tracer, restart%q(:,:,:,n_tracer))
       id_restart = register_restart_field(restart%fv_tracer_coarse, &
            filename, tracer_name, restart%q(:,:,:,n_tracer), domain=coarse_domain, &
            mandatory=.false., tile_count=tile_count)
    enddo

    do n_tracer = n_prognostic_tracers + 1, n_tracers
       call get_tracer_names(MODEL_ATMOS, n_tracer, tracer_name)
       call set_tracer_profile(MODEL_ATMOS, n_tracer, restart%qdiag(:,:,:,n_tracer))
       id_restart = register_restart_field(restart%fv_tracer_coarse, &
            filename, tracer_name, restart%qdiag(:,:,:,n_tracer), domain=coarse_domain, &
            mandatory=.false., tile_count=tile_count)
    enddo
  end subroutine register_fv_tracer_coarse

  subroutine register_fv_srf_wnd_coarse(tile_count, coarse_domain, restart)
    integer, intent(in) :: tile_count
    type(domain2d), intent(in) :: coarse_domain
    type(coarse_restart_type), intent(inout) :: restart

    character(len=64) :: filename
    integer :: id_restart

    filename = 'fv_srf_wnd_coarse.res.nc'

    id_restart = register_restart_field(restart%fv_srf_wnd_coarse, &
         filename, 'u_srf', restart%u_srf, domain=coarse_domain, &
         tile_count=tile_count)
    id_restart = register_restart_field(restart%fv_srf_wnd_coarse, &
         filename, 'v_srf', restart%v_srf, domain=coarse_domain, &
         tile_count=tile_count)
  end subroutine register_fv_srf_wnd_coarse

  subroutine register_mg_drag_coarse(tile_count, coarse_domain, restart)
    integer, intent(in) :: tile_count
    type(domain2d), intent(in) :: coarse_domain
    type(coarse_restart_type), intent(out) :: restart

    character(len=64) :: filename
    integer :: id_restart

    filename = 'mg_drag_coarse.res.nc'

    id_restart = register_restart_field(restart%mg_drag_coarse, &
         filename, 'ghprime', restart%sgh, domain=coarse_domain, &
         tile_count=tile_count)
  end subroutine register_mg_drag_coarse

  subroutine register_fv_land_coarse(tile_count, coarse_domain, restart)
    integer, intent(in) :: tile_count
    type(domain2d), intent(in) :: coarse_domain
    type(coarse_restart_type), intent(inout) :: restart

    character(len=64) :: filename
    integer :: id_restart

    filename = 'fv_land_coarse.res.nc'

    id_restart = register_restart_field(restart%fv_land_coarse, &
         filename, 'oro', restart%oro, domain=coarse_domain, &
         tile_count=tile_count)
  end subroutine register_fv_land_coarse

  subroutine coarse_grain_restart_data(Atm)
    type(fv_atmos_type), intent(inout) :: Atm

    if (trim(Atm%coarse_graining%strategy) .eq. MODEL_LEVEL) then
       call coarse_grain_restart_data_on_model_levels(Atm)
    endif
  end subroutine coarse_grain_restart_data
  
  subroutine coarse_grain_restart_data_on_model_levels(Atm)
    type(fv_atmos_type), intent(inout) :: Atm

    real, allocatable :: mass(:,:,:)

    allocate(mass(is:ie,js:je,1:npz))
    call compute_mass_weights(Atm%gridstruct%area(is:ie,js:je), Atm%delp(is:ie,js:je,1:npz), mass)

    call coarse_grain_fv_core_restart_data_on_model_levels(Atm, mass)
    call coarse_grain_fv_tracer_restart_data_on_model_levels(Atm, mass)
    call coarse_grain_fv_srf_wnd_restart_data_on_model_levels(Atm)
    if (Atm%flagstruct%fv_land) then
       call coarse_grain_mg_drag_restart_data_on_model_levels(Atm)
       call coarse_grain_fv_land_restart_data_on_model_levels(Atm)
    endif
  end subroutine coarse_grain_restart_data_on_model_levels

  subroutine coarse_grain_fv_core_restart_data_on_model_levels(Atm, mass)
    type(fv_atmos_type), intent(inout) :: Atm
    real, intent(in) :: mass(is:ie,js:je,1:npz)

    call weighted_block_edge_average_x(Atm%gridstruct%dx(is:ie,js:je+1), &
         Atm%u(is:ie,js:je+1,1:npz), Atm%coarse_graining%restart%u)
    call weighted_block_edge_average_y(Atm%gridstruct%dy(is:ie+1,js:je), &
         Atm%v(is:ie+1,js:je,1:npz), Atm%coarse_graining%restart%v)

    if (.not. Atm%flagstruct%hydrostatic) then
       call weighted_block_average(mass(is:ie,js:je,1:npz), &
            Atm%w(is:ie,js:je,1:npz), Atm%coarse_graining%restart%w)
       call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
            Atm%delz(is:ie,js:je,1:npz), Atm%coarse_graining%restart%delz)
       if (Atm%flagstruct%hybrid_z) then
          call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
            Atm%ze0(is:ie,js:je,1:npz), Atm%coarse_graining%restart%ze0)
       endif
    endif

    call weighted_block_average(mass(is:ie,js:je,1:npz), &
         Atm%pt(is:ie,js:je,1:npz), Atm%coarse_graining%restart%pt)
    call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
         Atm%delp(is:ie,js:je,1:npz), Atm%coarse_graining%restart%delp)
    call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
         Atm%phis(is:ie,js:je), Atm%coarse_graining%restart%phis)

    if (Atm%flagstruct%agrid_vel_rst) then
       call weighted_block_average(mass(is:ie,js:je,1:npz), &
            Atm%ua(is:ie,js:je,1:npz), Atm%coarse_graining%restart%ua)
       call weighted_block_average(mass(is:ie,js:je,1:npz), &
            Atm%va(is:ie,js:je,1:npz), Atm%coarse_graining%restart%va)
    endif
  end subroutine coarse_grain_fv_core_restart_data_on_model_levels

  subroutine coarse_grain_fv_tracer_restart_data_on_model_levels(Atm, mass)
    type(fv_atmos_type), intent(inout) :: Atm
    real, intent(in) :: mass(is:ie,js:je,1:npz)

    character(len=64) :: tracer_name
    integer :: n_tracer

    do n_tracer = 1, n_prognostic_tracers
       call get_tracer_names(MODEL_ATMOS, n_tracer, tracer_name)
       if (trim(tracer_name) .eq. 'cld_amt') then
          call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
               Atm%q(is:ie,js:je,1:npz,n_tracer), &
               Atm%coarse_graining%restart%q(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz,n_tracer))
       else
          call weighted_block_average(mass(is:ie,js:je,1:npz), &
               Atm%q(is:ie,js:je,1:npz,n_tracer), &
               Atm%coarse_graining%restart%q(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz,n_tracer))
       endif
    enddo

    do n_tracer = n_prognostic_tracers + 1, n_tracers
       call weighted_block_average(mass(is:ie,js:je,1:npz), &
               Atm%qdiag(is:ie,js:je,1:npz,n_tracer), &
               Atm%coarse_graining%restart%qdiag(is_coarse:ie_coarse,js_coarse:je_coarse,1:npz,n_tracer))
    enddo
  end subroutine coarse_grain_fv_tracer_restart_data_on_model_levels

  subroutine coarse_grain_fv_srf_wnd_restart_data_on_model_levels(Atm)
    type(fv_atmos_type), intent(inout) :: Atm

    call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
         Atm%u_srf(is:ie,js:je), Atm%coarse_graining%restart%u_srf)
    call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
         Atm%v_srf(is:ie,js:je), Atm%coarse_graining%restart%v_srf)
  end subroutine coarse_grain_fv_srf_wnd_restart_data_on_model_levels

  subroutine coarse_grain_mg_drag_restart_data_on_model_levels(Atm)
    type(fv_atmos_type), intent(inout) :: Atm

    call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
         Atm%sgh(is:ie,js:je), Atm%coarse_graining%restart%sgh)
  end subroutine coarse_grain_mg_drag_restart_data_on_model_levels

  subroutine coarse_grain_fv_land_restart_data_on_model_levels(Atm)
    type(fv_atmos_type), intent(inout) :: Atm

    call weighted_block_average(Atm%gridstruct%area(is:ie,js:je), &
         Atm%oro(is:ie,js:je), Atm%coarse_graining%restart%oro)
  end subroutine coarse_grain_fv_land_restart_data_on_model_levels

end module coarse_grained_restart_files_mod
