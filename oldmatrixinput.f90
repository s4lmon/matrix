program matrixinput
  use constants
  use matrixmain
  implicit none
  integer :: matrixsize
  type(matrix) :: matrixtest
  integer,dimension(:),allocatable :: row
  integer,dimension(:),allocatable :: col
  real(kind=dp),dimension(:),allocatable :: values
  !remember to delcare all variables
  integer :: ii
!is this where I input values? or is that in matrixmain
    matrixsize = 4
    values=(/1,5,9,6,1,2/)
    row = (/1,2,3,3,4,4/)
    col = (/1,2,1,3,2,4/)
  call matrixtest%initialise(matrixsize)
do ii=1,size(values)
  call matrixtest%addvalue(row(ii),col(ii),values(ii))

end do

end program
