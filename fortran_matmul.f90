program matrix_multiplication
  implicit none
  integer, parameter :: dp=kind(1.0d0)

interface
   subroutine create_matrix(m,n,mat)
   integer, parameter :: dp=kind(1.0d0)
   integer m,n
   real(kind=dp), dimension(m,n) :: mat
   end subroutine create_matrix
   end interface

interface
   subroutine write_matrix(a)
   integer, parameter :: dp=kind(1.0d0)
   real(kind=dp), dimension(:,:) :: a
   end subroutine write_matrix
   end interface

  integer, parameter :: m=8000, n=4000, k=12000
  real :: duration
  integer :: start_time, stop_time
  real(kind=dp), dimension(m,n) :: mat1
  real(kind=dp), dimension(n,k) :: mat2
  real(kind=dp), dimension(m,k) :: mat3


  call create_matrix(m,n,mat1)
  write(*,*) 'mat1 created!'
!  call write_matrix(mat1)
  call create_matrix(n,k,mat2)
  write(*,*) 'mat2 created!'
!  call write_matrix(mat2)

  call system_clock(start_time)
  mat3 = matmul(mat1,mat2)
  call system_clock(stop_time)

  duration = (real(stop_time)-real(start_time))/1000

  write (*,*) 'matrix multiplication took [s]: ', duration

!  call write_matrix(mat3)

end program matrix_multiplication

subroutine create_matrix (m,n, mat)
   integer, parameter :: dp=kind(1.0d0)
   integer m,n
   real(kind=dp), dimension(m,n) :: mat

    do  i=1,m
      do  j=1,n
         mat(i,j) = i+j
      end do
    end do

end subroutine create_matrix


subroutine write_matrix(a)
   integer, parameter :: dp=kind(1.0d0)
   real(kind=dp), dimension(:,:) :: a
   write(*,*)
   
   do i = lbound(a,1), ubound(a,1)
      write(*,*) (a(i,j), j = lbound(a,2), ubound(a,2))
   end do
end subroutine write_matrix
