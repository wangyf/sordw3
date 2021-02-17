! Initialization of volume stresses 
module m_inivolstress
implicit none
contains

subroutine inivolstress 
use m_globals
use m_diffcn
use m_bc
use m_util
use m_fieldio
use m_collective
integer :: i1(3), i2(3), i, ic, iid, id,  bc(3), &
           j1, k1, l1, j2, k2, l2, j3, k3, l3, j4, k4, l4
real, allocatable, dimension(:,:,:) :: tmpw1n,tmpw1p

if ( verb ) write( 0, * ) 'Initialization of volume stresses'

si1 = 0.0
si2 = 0.0

call fieldio( '<', 'a11', si1(:,:,:,1) )
call fieldio( '<', 'a22', si1(:,:,:,2) )
call fieldio( '<', 'a33', si1(:,:,:,3) )
call fieldio( '<', 'a23', si2(:,:,:,1) )
call fieldio( '<', 'a31', si2(:,:,:,2) )
call fieldio( '<', 'a12', si2(:,:,:,3) )

call vector_swap_halo( si1, nhalo )
call vector_swap_halo( si2, nhalo )

z1 = si1
z2 = si2

select case( ifn )
case( 1 ); z1(irup,:,:,:) = 0.0; z2(irup,:,:,:) = 0.0;
case( 2 ); z1(:,irup,:,:) = 0.0; z2(:,irup,:,:) = 0.0;
case( 3 ); z1(:,:,irup,:) = 0.0; z2(:,:,irup,:) = 0.0;
end select

call vector_swap_halo( z1, nhalo )
call vector_swap_halo( z2, nhalo )

! Fill halo, bc=4 means copy into halo, need this for fault traction at boundary (free(0) and pml(10))
if (ifn == 0) return

bc = 4
i1 = i1bc - 1
i2 = i2bc 
call vector_bc( z1, bc, bc, i1, i2 )
call vector_bc( z2, bc, bc, i1, i2 )

s1 = 0.0
call set_halo( s1, 0.0, i1cell, i2cell )


i1 = 1
i2 = nm
! Modify mesh (w1) near the fault to make sure the traction computation is correct
select case(ifn)
case(1)
    allocate(tmpw1n(i2(2),i2(3),3), &
             tmpw1p(i2(2),i2(3),3))

    tmpw1n(i1(2):i2(2),i1(3):i2(3),1:3) = w1(irup-1,i1(2):i2(2),i1(3):i2(3),1:3)
    tmpw1p(i1(2):i2(2),i1(3):i2(3),1:3) = w1(irup+2,i1(2):i2(2),i1(3):i2(3),1:3)

    w1(irup-1,i1(2):i2(2),i1(3):i2(3),1) = w1(irup,  i1(2):i2(2),i1(3):i2(3),1)-dx(1)
    w1(irup+2,i1(2):i2(2),i1(3):i2(3),1) = w1(irup+1,i1(2):i2(2),i1(3):i2(3),1)+dx(1)
    w1(irup-1,i1(2):i2(2),i1(3):i2(3),2) = w1(irup,  i1(2):i2(2),i1(3):i2(3),2)
    w1(irup+2,i1(2):i2(2),i1(3):i2(3),2) = w1(irup+1,i1(2):i2(2),i1(3):i2(3),2)
    w1(irup-1,i1(2):i2(2),i1(3):i2(3),3) = w1(irup,  i1(2):i2(2),i1(3):i2(3),3)
    w1(irup+2,i1(2):i2(2),i1(3):i2(3),3) = w1(irup+1,i1(2):i2(2),i1(3):i2(3),3)

case(2)
    allocate(tmpw1n(i2(1),i2(3),3), &
             tmpw1p(i2(1),i2(3),3))

    tmpw1n(i1(1):i2(1),i1(3):i2(3),1:3) = w1(i1(1):i2(1),irup-1,i1(3):i2(3),1:3)
    tmpw1p(i1(1):i2(1),i1(3):i2(3),1:3) = w1(i1(1):i2(1),irup+2,i1(3):i2(3),1:3)

    w1(i1(1):i2(1),irup-1,i1(3):i2(3),1) = w1(i1(1):i2(1),irup,  i1(3):i2(3),1)
    w1(i1(1):i2(1),irup+2,i1(3):i2(3),1) = w1(i1(1):i2(1),irup+1,i1(3):i2(3),1)
    w1(i1(1):i2(1),irup-1,i1(3):i2(3),2) = w1(i1(1):i2(1),irup,  i1(3):i2(3),2)-dx(2)
    w1(i1(1):i2(1),irup+2,i1(3):i2(3),2) = w1(i1(1):i2(1),irup+1,i1(3):i2(3),2)+dx(2)
    w1(i1(1):i2(1),irup-1,i1(3):i2(3),3) = w1(i1(1):i2(1),irup,  i1(3):i2(3),3)
    w1(i1(1):i2(1),irup+2,i1(3):i2(3),3) = w1(i1(1):i2(1),irup+1,i1(3):i2(3),3)

case(3)
    allocate(tmpw1n(i2(1),i2(2),3), &
             tmpw1p(i2(1),i2(2),3))

    tmpw1n(i1(1):i2(1),i1(2):i2(2),1:3) = w1(i1(1):i2(1),i1(2):i2(2),irup-1,1:3)
    tmpw1p(i1(1):i2(1),i1(2):i2(2),1:3) = w1(i1(1):i2(1),i1(2):i2(2),irup+2,1:3)

    w1(i1(1):i2(1),i1(2):i2(2),irup-1,1) = w1(i1(1):i2(1),i1(2):i2(2),irup,  1)
    w1(i1(1):i2(1),i1(2):i2(2),irup+2,1) = w1(i1(1):i2(1),i1(2):i2(2),irup+1,1)
    w1(i1(1):i2(1),i1(2):i2(2),irup-1,2) = w1(i1(1):i2(1),i1(2):i2(2),irup,  2)
    w1(i1(1):i2(1),i1(2):i2(2),irup+2,2) = w1(i1(1):i2(1),i1(2):i2(2),irup+1,2)
    w1(i1(1):i2(1),i1(2):i2(2),irup-1,3) = w1(i1(1):i2(1),i1(2):i2(2),irup,  3)-dx(3)
    w1(i1(1):i2(1),i1(2):i2(2),irup+2,3) = w1(i1(1):i2(1),i1(2):i2(2),irup+1,3)+dx(3)
end select


i1 = i1node
i2 = i2node
i1(ifn) = irup
i2(ifn) = irup+1

! Loop over component and derivative direction
doic: do ic  = 1, 3
doid: do iid = 1, 3; id = modulo( ic + iid - 2, 3 ) + 1

! Apr 9, 2020 Yongfei
! note: possible bug:
! To compute traction from intiatial stress tensor through the following method,
! it is required that the volumes (positive and negative side) adjacent to the 
! fault are numerically identical. Otherwise, the initial traction will be distorted
! or inaccurate due to inidentical volume pairs. 
! Because S(tensor) * nhat = \int \partial S(tensor) /\partial x /Volume

! f_i = w_ij,j
! 2020 May: Traction is computed through diffcn with oplevel of 5 (use mesh(w1))
! 2020 Oct: I changed the oplevel of 5 to default (automatic chosed to 0 and 6);
!           mesh adjacent to fault is already modified
if ( ic == id ) then
    call diffcn( s1, z1, ic, id, i1, i2, oplevel, bb, w1, dx1, dx2, dx3, dx )
else
    i = 6 - ic - id
    call diffcn( s1, z2, i, id, i1, i2, oplevel, bb, w1, dx1, dx2, dx3, dx )
end if

! Add contribution to force vector
if ( ic == id ) then
    z1(:,:,:,ic) = s1
else
    z1(:,:,:,ic) = z1(:,:,:,ic) + s1
end if

end do doid
end do doic

call vector_bc( z1, bc1, bc2, i1bc, i2bc )
!call vector_swap_halo( z1, nhalo )

! Extract tractions on fault

i1 = 1
i2 = nm
i1(ifn) = irup
i2(ifn) = irup
j1 = i1(1); j2 = i2(1)
k1 = i1(2); k2 = i2(2)
l1 = i1(3); l2 = i2(3)

i1(ifn) = irup + 1
i2(ifn) = irup + 1
j3 = i1(1); j4 = i2(1)
k3 = i1(2); k4 = i2(2)
l3 = i1(3); l4 = i2(3)

f1 = area
call invert( f1 )

do i = 1, 3
      t0(:,:,:,i) = f1/2 * ( z1(j3:j4,k3:k4,l3:l4,i) - z1(j1:j2,k1:k2,l1:l2,i) )
end do

! recover mesh (w1)
select case(ifn)
case(1)
    w1(irup-1,i1(2):i2(2),i1(3):i2(3),1:3) = tmpw1n(i1(2):i2(2),i1(3):i2(3),1:3)
    w1(irup+2,i1(2):i2(2),i1(3):i2(3),1:3) = tmpw1p(i1(2):i2(2),i1(3):i2(3),1:3)
    deallocate(tmpw1n,tmpw1p)
case(2)
    w1(i1(1):i2(1),irup-1,i1(3):i2(3),1:3) = tmpw1n(i1(1):i2(1),i1(3):i2(3),1:3)
    w1(i1(1):i2(1),irup+2,i1(3):i2(3),1:3) = tmpw1p(i1(1):i2(1),i1(3):i2(3),1:3)
    deallocate(tmpw1n,tmpw1p)
case(3)
    w1(i1(1):i2(1),i1(2):i2(2),irup-1,1:3) = tmpw1n(i1(1):i2(1),i1(2):i2(2),1:3)
    w1(i1(1):i2(1),i1(2):i2(2),irup+2,1:3) = tmpw1p(i1(1):i2(1),i1(2):i2(2),1:3)
    deallocate(tmpw1n,tmpw1p)
end select

end subroutine

end module
