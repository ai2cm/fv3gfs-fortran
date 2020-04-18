module coarse_grained_restart_files_mod

  use coarse_graining_mod, only: get_coarse_array_bounds
  use fv_arrays_mod, only: coarse_restart_type

  implicit none
  private

  public :: coarse_grained_restart_files_init

contains

  subroutine coarse_grained_restart_files_init(npz, nt_prog, nt_diag, coarse_bd, restart)
    integer, intent(in) :: npz, nt_prog, nt_diag
    type(fv_coarse_grid_bounds_type) :: coarse_bd
    type(coarse_restart_type), intent(out) :: restart

    call allocate_coarse_restart_type(npz, nt_prog, nt_diag, coarse_bd, restart)
  end subroutine coarse_grained_restart_files_init

  subroutine allocate_coarse_restart_type(npz, nt_prog, nt_diag, coarse_bd, restart)
    integer, intent(in) :: npz, nt_prog, nt_diag
    type(fv_coarse_grid_bounds_type) :: coarse_bd
    type(coarse_restart_type), intent(out) :: restart

    integer :: is_coarse, ie_coarse, js_coarse, je_coarse

    call get_coarse_array_bounds(coarse_bd, is_coarse, ie_coarse, js_coarse, je_coarse)

    allocate(restart%u(is_coarse:ie_coarse,js_coarse:je_coarse+1,npz))
    allocate(restart%v(is_coarse:ie_coarse+1,js_coarse:je_coarse,npz))
    allocate(restart%ua(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%va(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%u_srf(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(restart%v_srf(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(restart%w(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%delp(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%delz(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%pt(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    allocate(restart%q(is_coarse:ie_coarse,js_coarse:je_coarse,npz,nt_prog))
    allocate(restart%qdiag(is_coarse:ie_coarse,js_coarse:je_coarse,npz,nt_prog+1:nt_diag + nt_prog))
    allocate(restart%sgh(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(restart%oro(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(restart%phis(is_coarse:ie_coarse,js_coarse:je_coarse))
    allocate(restart%ze0(is_coarse:ie_coarse,js_coarse:je_coarse,npz))
    
  end subroutine allocate_coarse_restart_type
  
end module coarse_grained_restart_files_mod
