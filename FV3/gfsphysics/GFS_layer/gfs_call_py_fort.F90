module gfs_call_py_fort
  ! Call py fort hooks for some common FV3 datastructures
  use callpy_mod, only: set_state, call_function, get_state
  use machine, only: kind_phys
  use GFS_typedefs, only: GFS_statein_type, GFS_stateout_type
  implicit none
  private
  public :: send_statein, send_stateout, get_stateout, python_start_physics, python_end_physics

  character(len=*), parameter :: T = "air_temperature"
  character(len=*), parameter :: qv = "specific_humidity" 
  character(len=*), parameter :: qc = "cloud_water_mixing_ratio"
  character(len=*), parameter :: u = "eastward_wind"
  character(len=*), parameter :: v = "northward_wind"

  character(len=*), parameter :: phii = "geopotential_height_interface"
  character(len=*), parameter :: prsi = "pressure_interface"
  character(len=*), parameter :: prsik = "exner_function_interface"

  character(len=*), parameter :: phil = "geopotential_height_center"
  character(len=*), parameter :: prsl = "pressure_center"
  character(len=*), parameter :: prslk = "exner_fucntion_center"

  character(len=*), parameter :: vvl = "omega"

  integer, parameter :: n_sphum = 1
  integer, parameter :: n_cloud = 2

  character(len=*), parameter :: python_module = "emulation"

contains

  subroutine send_statein(statein, prefix)
    type(GFS_statein_type), intent(in) :: statein
    character(len=*), intent(in) :: prefix
    real(kind=kind_phys), dimension(size(statein%tgrs, 1) ,size(statein%tgrs, 2)) :: tmp
    call set_state(trim(prefix) // T, statein%tgrs)
    tmp = statein%qgrs(:, :, n_sphum)
    call set_state(trim(prefix) // qv, tmp)
    tmp = statein%qgrs(:, :, n_cloud)
    call set_state(trim(prefix) // qc, tmp)
    call set_state(trim(prefix) // u, statein%ugrs)
    call set_state(trim(prefix) // v, statein%vgrs)
    call set_state(trim(prefix) // phii, statein%phii)
    call set_state(trim(prefix) // prsi, statein%prsi)
    call set_state(trim(prefix) // prsik, statein%prsik)
    call set_state(trim(prefix) // phil, statein%phil)
    call set_state(trim(prefix) // prsl, statein%prsl)
    call set_state(trim(prefix) // prslk, statein%prslk)
    call set_state(trim(prefix) // vvl, statein%vvl)
  end subroutine

  subroutine get_stateout(stateout, prefix)
    type(GFS_stateout_type), intent(in) :: stateout
    character(len=*), intent(in) :: prefix
    real(kind=kind_phys), dimension(1:size(stateout%gu0, 1) ,1:size(stateout%gu0, 2)) :: tmp
    call get_state(trim(prefix) // T, Stateout%gt0)
    call get_state(trim(prefix) // qv, tmp)
    stateout%gq0(:, :, n_sphum) = tmp
    call get_state(trim(prefix) // qc, tmp)
    stateout%gq0(:, :, n_cloud) = tmp
    call get_state(trim(prefix) // u, Stateout%gu0)
    call get_state(trim(prefix) // v, Stateout%gv0)
  end subroutine

  subroutine send_stateout(stateout, prefix)
    type(GFS_stateout_type), intent(inout) :: stateout
    character(len=*), intent(in) :: prefix
    real(kind=kind_phys), dimension(size(stateout%gu0, 1) ,size(stateout%gu0, 2)) :: tmp
    call set_state(trim(prefix) // T, Stateout%gt0)
    tmp = stateout%gq0(:, :, n_sphum)
    call set_state(trim(prefix) // qv, tmp)
    tmp = stateout%gq0(:, :, n_cloud)
    call set_state(trim(prefix) // qc, tmp)
    call set_state(trim(prefix) // u, Stateout%gu0)
    call set_state(trim(prefix) // v, Stateout%gv0)
  end subroutine

  subroutine python_start_physics()
    call call_function(python_module, "start_physics")
  end subroutine

  subroutine python_end_physics()
    call call_function(python_module, "end_physics")
  end subroutine

end module
