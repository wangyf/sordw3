! Off-fault plasticity models
module m_plastic
implicit none
contains

subroutine update_drucker_prager_1
! Original viscoplasticity of Drucker-Prager model
! Used in Andrews 2005; Duan and Day 2010; Shi and Day, 2013...
! Originally used in SORD
use m_globals
use m_util
use m_fieldio

integer :: i,i1(3)

if (eplasticity /= 'plastic') return
if (plmodel /= 'DP1') return

! add initial stress
w1 = w1 + si1
w2 = w2 + si2

s1 = w1(:,:,:,1) + w1(:,:,:,2) + w1(:,:,:,3)
s2 = 0.

do i = 1, 3
    z1(:,:,:,i) = w1(:,:,:,i) - s1 / 3.
    z2(:,:,:,i) = w2(:,:,:,i)
    s2 = s2 + z1(:,:,:,i) * z1(:,:,:,i) + 2.0 * z2(:,:,:,i) * z2(:,:,:,i)
end do

s2 = sqrt( 0.5 * s2 )
call invert( s2 )
r3 = - s1 / 3.0 * sin( phi ) + mco * cos( phi )
r3 = r3 * s2

call fieldio( '<>', 'plcls', r3 ) !input or output the ratio between t_yield/t_bar (>1 elastic)
! if (it == 1 .and. any(r3(i1cell(1):i2cell(1), &
!                           i1cell(2):i2cell(2), &
!                           i1cell(3):i2cell(3)) < 1)) then
!     write( 0, * ) 'ERROR: Yield surface is reached at beginning'
!     stop
! end if

if (it==1 .and. debug > 1 .and. minval( r3(i1cell(1):i2cell(1), &
                                           i1cell(2):i2cell(2), &
                                           i1cell(3):i2cell(3))) < 1) then
    i1 = minloc(r3(i1cell(1):i2cell(1), &
                               i1cell(2):i2cell(2), &
                               i1cell(3):i2cell(3)))
    write(0,*) 'ERROR: Yield surface is reached at', i1,' ip=', ip
end if
if (it==1 .and. minval( r3(i1cell(1):i2cell(1), &
                           i1cell(2):i2cell(2), &
                           i1cell(3):i2cell(3))) < 1) &
    stop 'ERROR: Yield surface is reached at beginning'


if ( tv > 1.e-6 ) then
   s2 = r3 + ( 1.0 - r3 ) * exp( - dt / tv )
else
   s2 = r3
end if

do i = 1, 3
    r1 = z1(:,:,:,i)
    r2 = z2(:,:,:,i)
    where ( r3 < 1.0 )
        r4 = r1 * s2 
        r5 = r2 * s2
        r1 = r1 - r4
        r2 = r2 - r5
    elsewhere
        r4 = r1
        r5 = r2
        r1 = 0.0
        r2 = 0.0
    end where

    dep1(:,:,:,i) = 0.5 * r1 * mur 
    dep2(:,:,:,i) = 0.5 * r2 * mur 

    w1(:,:,:,i) = s1 / 3.0 + r4
    w2(:,:,:,i) = r5
end do

s2 = 0.0
do i = 1, 3
    s2 = s2 + dep1(:,:,:,i) * dep1(:,:,:,i) + 2.0 * dep2(:,:,:,i) * dep2(:,:,:,i)
end do
epm = epm + sqrt( 2.0 / 3.0 * s2 )

call fieldio( '<>', 'epm', epm )

ep1 = ep1 + dep1
ep2 = ep2 + dep2

w1 = w1 - si1
w2 = w2 - si2

end subroutine

subroutine update_drucker_prager_2
! Implementation from Dunham et al, 2011a
! Containing hardening/softening and dilation
use m_globals
use m_fieldio
integer:: i

! add initial stress
w1 = w1 + si1
w2 = w2 + si2

! compute invariants
s1 = w1(:,:,:,1) + w1(:,:,:,2) + w1(:,:,:,3) 
s2 = 0.

do i = 1, 3
    z1(:,:,:,i) = w1(:,:,:,i) - s1 / 3. !deviatoric stress (normal conponents)
    z2(:,:,:,i) = w2(:,:,:,i)           !deviatoric stress (shear  conponents)
    s2 = s2 + z1(:,:,:,i) * z1(:,:,:,i) + 2.0 * z2(:,:,:,i) * z2(:,:,:,i)
end do
s1 = s1/3. ! sigma
s2 = sqrt( 0.5 * s2 ) !tau

! compute yield surface
r1 = mco * cos( phi ) + gammap * plh
r2 = s2 - max(-sin( phi )*s1+r1, 0.0)

where ( r2 > 0.0 ) !plastic flow

    lambda = (s2 + sin(phi)*s1 - r1)/&
               (pleta+dt*(plh + mur + sin(phi)*plb*bk))
    
    s2 = s2 - dt * lambda * mur        ! update tau
    s1 = s1 - dt * lambda * plb * bk   ! update sigma

elsewhere  !Elasticity

    lambda = 0.0

end where

! update deviatoric and absolute stress
do i = 1, 3
    z1(:,:,:,i) = z1(:,:,:,i) * s2 / (s2 + dt * lambda * mur)
    z2(:,:,:,i) = z2(:,:,:,i) * s2 / (s2 + dt * lambda * mur)

    w1(:,:,:,i) = z1(:,:,:,i) + s1(:,:,:)
    w2(:,:,:,i) = z2(:,:,:,i)

    dep1(:,:,:,i) = dt * lambda * (z1(:,:,:,i)/(2*s2)+plb/3)
    dep2(:,:,:,i) = dt * lambda * (z1(:,:,:,i)/(2*s2))
end do 

gammap = gammap + dt * lambda

call fieldio( '<>', 'gammap', gammap )

ep1 = ep1 + dep1
ep2 = ep2 + dep2

w1 = w1 - si1
w2 = w2 - si2
end subroutine

end module

