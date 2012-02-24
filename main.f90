! ********************************************
! main.f90 - control program flow
! ********************************************
! Solving the 
!
!   Convection Diffusion Problem
!
! with the Finite Element Method
! ********************************************
! Changelog:
!
! 2012.02.06: Start basic outline
!
! 1. Gather System Discretization Data
! 2. Create Assembly Matrix
! 3. Create Force Vector
! 4. Solve
!
! ********************************************
! Problem statement:
! ********************************************
!
! a*grad(v) + grad( \nu ( grad(v)) = s
!
! ********************************************

      program main
        use global
        use printer

        implicit none

        ! VARIABLES
        ! counters
        integer :: i, j, k
        integer :: e, n
        real*8 :: x
        real*8 :: sum

        ! Assemblymatrix, forcevector:

        real,dimension(:,:),pointer :: A
        real,dimension(:),pointer :: b

        real,dimension(:,:),pointer :: C_e, K_e
        real,dimension(:),pointer   :: f_e

        ! basic ingredients
        type(mesh) :: mesh1D
        type(fluid) :: fluid1D

        ! play
        fluid1D%a   = 1.0
        fluid1D%nu  = 0.01

        ! prepare assembly matrix and force vector
        allocate( A(numberOfNodes, numberOfNodes) )
        allocate( b(numberOfNodes) )

        allocate( C_e(2,2) )
        allocate( K_e(2,2) )
        allocate( f_e(2) )

        ! reserve space for matrices - 1D
        allocate( mesh1D%boundaryFlag(numberOfNodes) )
        allocate( mesh1D%nodePositions(numberOfNodes) )
        allocate( mesh1D%elementConnectivity(numberOfElements, &
        numberOfNodesPerElement) )


        ! Initialize for security
        A(:,:) = 0.0
        b(:) = 0.0

        ! fill matrices - 1D: points on a line
        do i=1,numberOfNodes 
          mesh1D%nodePositions(i) = i * elementLength
        end do

        print *, "Position of nodes:"
        call p(mesh1D%nodePositions, "v")


        ! fill matrices - 1D: connectivity:
        do i=1,numberOfElements
          mesh1D%elementConnectivity(i,1) = i
          mesh1D%elementConnectivity(i,2) = i+1
        end do

        print *, "Mesh-Element-Connectivity:"
        call p(mesh1D%elementConnectivity, "v")


        ! Create element matrices: (by row)
        C_e(1,1:2) = (/ -1, 1 /)
        C_e(2,1:2) = (/ 1, -1 /)
        C_e = (fluid1D%a / 2.0) * C_e

        K_e(1,1:2) = (/ 1, -1 /)
        K_e(2,1:2) = (/ -1, 1 /)
        K_e = (fluid1D%nu / elementLength ) * K_e

        f_e = (/ 1, 1 /)
        f_e = elementLength/2 * f_e

        ! Check for sanity
        print *, "Convection Matrix:"
        call p(C_e, "v")

        print *, "Diffusion Matrix:"
        call p(K_e, "v")
        
        print *, "Force Vector:"
        call p(f_e, "v")

        ! inlet and outlet - mark nodes
        mesh1D%boundaryFlag(:) = 0.0
        mesh1D%boundaryFlag(1) = 1.0
        mesh1D%boundaryFlag(numberOfNodes)=1.0

        ! Assemble A and b
        do i=1,numberOfElements
          e = mesh1D%elementConnectivity(i,1)
          A(e:e+2,e:e+2) = A(e:e+2,e:e+2) + C_e + K_e
          b(e:e+2) = b(e:e+2) + f_e
        end do

        ! Fill marked boundaries
        do i = 1,numberOfNodes
          if (mesh1D%boundaryFlag(i) .eq. 1.0) then
            ! Apply to Matrix/EQS
            A(i,:) = 0.0      ! I-matrix @ relating row
            A(i,i) = 1.0
            b(i) = 0.0        ! Dirichlet boundaries (here == 0)

          end if
        end do

        call p(A, "v")
        call p(b, "v")


        ! Solve equation system via gauss elemination:
        ! Number of equations = number of nodes:
        n = numberOfNodes

        ! Upper triangular matrix:
        do k = 1,n-1
          if (abs(A(k,k)) .gt. 1.E-6) then
            do i=k+1,n                              ! move through column
              x = A(i,k)/A(k,k)
              do j = k+1,n
                A(i,j) = A(i,j) - A(k,j)*x          ! multiply row
              end do

              b(i) = b(i) - b(k)*x
            end do

          else
            print *, "Can't solve EQS because of ", k
            stop
          end if
        end do
          
        ! Back substitution:
        do i = n,1,-1
          sum = b(i)
          if(i .lt. n) then
            do j=i+1,n
              sum = sum - A(i,j)*b(j)
            end do
          end if
          b(i) = sum/A(i,i)
        end do

        print *, "==== Solution: ===="
        call p(b, "v")

        deallocate(A,b)
        deallocate(mesh1D%elementConnectivity, mesh1D%nodePositions, &
        mesh1D%boundaryFlag)
        deallocate(C_e, K_e, f_e)
      end program main
