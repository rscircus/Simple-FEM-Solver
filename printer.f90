! 
! Debugging-Helper: A nice interface to print whatever value you like to see.
! 

      module printer

        interface p
          module procedure printInteger, printReal, print2dMatrix, &
            printVector
        end interface

      contains
        
        ! pointers:
        subroutine printInteger(iValue)
          integer :: iValue
          print *, iValue
        end subroutine

        subroutine printReal(rValue)
          real :: rValue
          print *, rValue
        end subroutine

        subroutine print2dMatrix(mValue, arguments)
          real, dimension(:,:), pointer :: mValue
          character, optional :: arguments

          integer :: dimX, dimY
          integer :: m, n

          dimX = ubound(mValue,1)
          dimY = ubound(mValue,2)

          if (present(arguments)) then
            print *, "---- Verbose Mode -----"
            print *, "     Size Y: ", size(mValue,1)
            print *, "     Size X: ", size(mValue,2)
            print *, "Lower bound: ", lbound(mValue)
            print *, "Upper bound: ", ubound(mValue)
            print *, ""
            print *, "===== Show Matrix ====="
          else
            print *, "----- Normal Mode -----"
          end if

          do m=1,dimX
            !write(*,'(f8.3)', advance ='no') "y(", m, ") = "
            do n=1,dimY
              write(*,'(f8.3)', advance='no') mValue(m,n)
            end do
            print *, ""
          end do

          print *, ""

        end subroutine

        subroutine printVector(vValue, arguments)
          real, dimension(:), pointer :: vValue
          character, optional :: arguments
          integer :: dimY
          integer :: m

          dimY = ubound(vValue,1)

          if (present(arguments)) then
            print *, "---- Verbose Mode -----"
            print *, "Lower bound: ", lbound(vValue)
            print *, "Upper bound: ", ubound(vValue)
            print *, ""
            print *, "===== Show Vector ====="
          else
            print *, "----- Normal Mode -----"
          end if

          do m=1,dimY
            print *, vValue(m)
          end do

          print *, ""

        end subroutine

        ! pointers:
!        subroutine print2dMatrixAlloc(aValue)
!          integer, dimension(:,:), pointer :: aValue
!          integer :: dimX, dimY
!          integer :: m, n
!
!          dimX = ubound(aValue,1)
!          dimY = ubound(aValue,2)
!
!          do m=1,dimY
!            print *, ""
!            !write(*,'(f8.3)', advance ='no') "y(", m, ") = "
!            do n=1,dimX
!              write(*,'(f8.3)', advance='no') aValue(m,n)
!            end do
!          end do
!        end subroutine
!
!        subroutine printVectorAlloc(vValue)
!          real, dimension(:), pointer :: vValue
!          integer :: dimY
!          integer :: m
!
!          dimY = ubound(vValue,1)
!
!          do m=1,dimY
!            print *, vValue(m)
!          end do
!        end subroutine


      end module
