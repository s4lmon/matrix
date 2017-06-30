module bicg
  use constants
  use matrixmain
  implicit none
contains
  subroutine bicgsolve(matrixin,rhs,soln)
    class(matrix),intent(inout) :: matrixin !no longer class-bound subroutine
    real(kind=dp),dimension(matrixin%matrixsize) :: rhs,soln,r_old,r_new,r_hat,vv,p_old,p_new,hh,x_old,x_new
    real(kind=dp) :: err !can't give intent to variables not in argumentlist
    real(kind=dp) :: alpha,rho

    v=0.0_dp
    p=0.0_dp

    call matrixin%multiply(soln,r_new)
    r_hat=r_new
    rho=1.0_dp
    alpha=1.0_dp
    vv=0
    p_old=0

!1
    rho_new=dot_product(r_hat,r_old)
!2
    beta=(rho_new/rho_old)*(alpha/omega_old)
!3
    p_new=r_old+beta*(p_old-omega_old*v_old)
!4
    call matrixin%multiply(p_new,v_new)
!5
    alpha=rho_new/(dot_product(r_hat,v_new))
!6
    hh=x_old+alpha*p_new
!7
    if (hh) then !h is accurate enough
      x_new=hh
    else
!8
      ss=r_old-alpha*v_new
!9
      call matrixin%multiply(ss,tt)
!10
      omega_new=(dot_product(tt,ss)/dot_product(tt,tt))
!11
      x_new=hh+omega*ss
!12 if x accurate enough then quit
      do ii=1,matrixin%matrixsize
        if (max(x_new(ii)-x_old(ii))<1E-10) then
          soln=x_new
        else
          x_new=x_old
    end if

print*, soln



  end subroutine
end module
