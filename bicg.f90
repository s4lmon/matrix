module bicg
  use constants
  use matrixmain
  implicit none
contains
  subroutine bicgsolve(matrixin,rhs,soln)
    class(matrix),intent(inout) :: matrixin !no longer class-bound subroutine
    real(kind=dp),dimension(matrixin%matrixsize) :: rhs,soln,rold,rnew
    real(kind=dp) :: err !can't give intent to variables not in argumentlist
    real(kind=dp) :: v,p,alpha,rho

    v=0.0_dp
    p=0.0_dp

    call matrixin%multiply(s,t)

  end subroutine
end module
