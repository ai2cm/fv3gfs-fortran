! For use with serialbox
module k_checkpoint
  implicit none
  public set_k,get_k,set_nz,get_nz
  integer :: k,nz,serialize
  contains
  subroutine set_k(in_k)
    integer, intent(in):: in_k
    k = in_k
  end subroutine set_k
 
  subroutine get_k(out_k)
    integer, intent(out) ::out_k
    out_k = k
  end subroutine get_k
  
  subroutine set_nz(in_nz)
    integer, intent(in):: in_nz
    nz = in_nz
  end subroutine set_nz

  subroutine get_nz(out_nz)
    integer, intent(out) ::out_nz
    out_nz = nz
  end subroutine get_nz

  
end module k_checkpoint
