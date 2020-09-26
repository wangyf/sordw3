! Thermal pressurization
module m_thermpres
implicit none
integer :: nz, subnt
real,dimension(:),allocatable :: z
real,dimension(:,:,:,:),allocatable :: tmpt,pprs
real,dimension(:,:,:,:),allocatable :: vold2,vold1,tsold1
real,dimension(:,:,:), allocatable :: shearheat
real,allocatable,dimension(:) :: omega,dtmpt,dtmpp
contains

! Initiate array used in thermalpressurization
subroutine ini_thermpres
use m_globals, only : nm,ifn,dt,tpsubdt,tplz,tpdz,tini,pini,temp,porep
use m_fieldio
integer :: i1(3),j,k,l

! initialization of diffusion temperature and pore pressure array
if ( ifn /= 0 ) then
    i1 = nm
    i1(ifn) = 1
else
    return
end if

! initialization diffusion distance array
nz = floor( tplz / tpdz ) + 1
allocate (z(nz))

do j = 1, nz
    z(j) = ( j - 1 ) * tpdz
end do

subnt = floor(dt/tpsubdt)

j = i1(1)
k = i1(2)
l = i1(3)

! allocate temperature and pore pressure in diffusion distance
allocate( &
    tmpt(j,k,l,nz+2),  &
    pprs(j,k,l,nz+2)   )

! allocate previous slip velocity and shear traction
allocate( &
    vold2(j,k,l,3),  &
    vold1(j,k,l,3),  &
    tsold1(j,k,l,3)  )

! allocate heat production array
allocate( &
    shearheat(j,k,l))

allocate(omega(nz))
allocate(dtmpt(nz))
allocate(dtmpp(nz))

! initialization
tmpt = 0.0
pprs = 0.0
vold2 = 0.0
vold1 = 0.0
tsold1 = 0.0
shearheat = 0.0
omega = 0.0
dtmpt = 0.0
dtmpp = 0.0

!set up initial value in shear zone 
do j = 1, nz+2
    tmpt(:,:,:,j) = tini(:,:,:)
    pprs(:,:,:,j) = pini(:,:,:)
end do

temp = tini
porep = pini

end subroutine ini_thermpres

subroutine record_svel_ts(field,op)
    use m_globals, only : master,debug
    real,intent(in) :: field(:,:,:,:)
    character(4),intent(in) :: op

    select case(op)
    case ('svo2'); vold2 = field; if(master.and.debug>1) write(0,*) 'Record SV at n-3/2'
    case ('svo1'); vold1 = field; if(master.and.debug>1) write(0,*) 'Record SV at n-1/2'
    case ('tso1'); tsold1 = field; if(master.and.debug>1) write(0,*) 'Record Ts at n-1'
    end select

end subroutine

! compute shear heat at n-1 step
subroutine compute_shearheat
    shearheat = (sum(vold2 * tsold1,4) + sum(vold1 * tsold1,4))/2.
    ! write(0,*) shearheat(151,2,1)
end subroutine

subroutine init_vw
use m_globals
    vw = pi * ath * ((Tempw - tini)/(tau_c/rhoc))**2/AspD*1e-3
end subroutine


subroutine update_vw
use m_globals
    vw = pi * ath * ((Tempw - temp)/(tau_c/rhoc))**2/AspD*1e-3
end subroutine

! solve thermal pressurization equation
subroutine update_thermpres
use m_globals
integer :: j,k,l,i1(3),i2(3),subit

! initialization of diffusion temperature and pore pressure array
if ( ifn /= 0 ) then
    i1 = i1node
    i2 = i2node
    i1(ifn) = 1
    i2(ifn) = 1
else
    return
end if

! loop over subfault patch
do l = i1(3), i2(3)
    do k = i1(2), i2(2)
        do j = i1(1), i2(1)


            if (tpw(j,k,l)<1e-5) then ! slip on plane
                omega = 0
                omega(1) = shearheat(j,k,l)/tpdz
            else
                omega = shearheat(j,k,l) * &
                       exp(-z*z/(2*tpw(j,k,l)*tpw(j,k,l)))/&
                        (tpw(j,k,l)*sqrt(2*pi))
            end if

            ! loop over substep in thermal pressurization
            do subit = 1, subnt

                !update boundary condition
                tmpt(j,k,l,1) = tmpt(j,k,l,3)
                tmpt(j,k,l,nz+2) = tmpt(j,k,l,nz)
                pprs(j,k,l,1) = pprs(j,k,l,3)
                pprs(j,k,l,nz+2) = pprs(j,k,l,nz)

                ! diffusion
                dtmpt = ath(j,k,l)*(tmpt(j,k,l,3:nz+2)-2*tmpt(j,k,l,2:nz+1)+tmpt(j,k,l,1:nz))/(tpdz*tpdz)+&
                        omega/rhoc(j,k,l) 
                dtmpp = ahy(j,k,l)*(pprs(j,k,l,3:nz+2)-2*pprs(j,k,l,2:nz+1)+pprs(j,k,l,1:nz))/(tpdz*tpdz)+&
                        tplam(j,k,l)*dtmpt

                ! undrained, adiabatic
                ! dtmpt = omega/rhoc(j,k,l)
                ! dtmpp = tplam(j,k,l)*dtmpt

                tmpt(j,k,l,2:nz+1) = tmpt(j,k,l,2:nz+1) + dtmpt*tpsubdt
                pprs(j,k,l,2:nz+1) = pprs(j,k,l,2:nz+1) + dtmpp*tpsubdt

            end do

            ! store temperature and pore pressure on fault
            temp(j,k,l) = tmpt(j,k,l,2)
            porep(j,k,l) = pprs(j,k,l,2)
        end do 
    end do
end do

end subroutine update_thermpres


! destroy all variable allocated
subroutine destroy_thermpres
    deallocate(z)
    deallocate(tmpt)
    deallocate(pprs)
    deallocate(vold2)
    deallocate(vold1)
    deallocate(tsold1)
    deallocate(shearheat)
end subroutine

end module

