! ********************************************
! global.h - global vars
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
! ********************************************

      module global
        integer,parameter :: numberOfNodes              = 8
        integer,parameter :: numberOfElements           = numberOfNodes - 1
        integer,parameter :: numberOfSpatialDimensions  = 1
        integer,parameter :: numberOfNodesPerElement    = 2
        integer,parameter :: numberOfDOFPerNode         = 1
        real,parameter    :: elementLength              = 1
       ! integer :: numberOfEquations

        ! Fluid Properties
        type fluid
          real :: a
          real :: nu
        end type

        ! Mesh Properties
        type mesh
          real, dimension(:), pointer :: boundaryFlag     
          real, dimension(:,:), pointer :: elementConnectivity 
          real, dimension(:), pointer :: nodePositions
        end type mesh


      !  allocate(boundary(lwb:upb,lwb1:upb1), STAT=ierr)
      !  if ( ierr .ne. 0 ) then
      !    print *, "global: mesh.boundary allocation failed"

      !  function deallocateEverything()
      !    if allocated(mesh%boundary) then deallocate mesh%boundary
      !  end function deallocateEverything

      end module global
