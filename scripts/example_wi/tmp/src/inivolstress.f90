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
integer :: i1(3), i2(3), i, ic, iid, id,   &
           j1, k1, l1, j2, k2, l2, j3, k3, l3, j4, k4, l4

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

s1 = 0.0
call set_halo( s1, 0.0, i1cell, i2cell )

! Loop over component and derivative direction
doic: do ic  = 1, 3
doid: do iid = 1, 3; id = modulo( ic + iid - 2, 3 ) + 1

! f_i = w_ij,j
i1 = i1node
i2 = i2node
if ( ic == id ) then
    call diffcn( s1, z1, ic, id, i1, i2, oplevel, bb, xx, dx1, dx2, dx3, dx )
else
    i = 6 - ic - id
    call diffcn( s1, z2, i, id, i1, i2, oplevel, bb, xx, dx1, dx2, dx3, dx )
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

end subroutine

end module
