! Thermal pressurization
module m_thermpres
implicit none
integer :: nz, subnt
real,dimension(:),allocatable :: z
real,dimension(:,:,:,:),allocatable :: tmpt,pprs
real,dimension(:,:,:,:),allocatable :: vold2,vold1,tsold1
real,dimension(:,:,:), allocatable :: shearheat
contains

! Initiate array used in thermalpressurization
subroutine ini_thermpres
use m_globals, only : nm,ifn,dt,tpsubdt,tplz,tpdz,tini,pini
use m_fieldio
integer :: i1(3),j,k,l

! initialization diffusion distance array
nz = floor( tplz / tpdz ) + 1
allocate (z(nz))

do j = 1, nz
    z = ( j - 1 ) * tpdz
end do

subnt = floor(dt/tpsubdt)

! initialization of diffusion temperature and pore pressure array
if ( ifn /= 0 ) then
    i1 = nm
    i1(ifn) = 1
else
    i1 = 0
end if
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

! initialization
tmpt = 0.0
pprs = 0.0
vold2 = 0.0
vold1 = 0.0
tsold1 = 0.0
shearheat = 0.0

!set up initial value in shear zone 
do j = 1, nz+2
    tmpt(:,:,:,j) = tini(:,:,:)
    pprs(:,:,:,j) = pini(:,:,:)
end do

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
end subroutine

! second derivative
subroutine ddz2(field)
    real,intent(inout) :: field(:)
    real,dimension(:),allocatable :: f
    integer :: i,n

    n = size(field)
    allocate(f(n))
    f = 0.0

    do i = 2, n-1
        f(i) = field(i+1)-2*field(i)+field(i-1)
    end do

    field = f

    deallocate(f)
end subroutine

! solve thermal pressurization equation
subroutine update_thermpres
use m_globals
real,allocatable,dimension(:) :: dtmpt,dtmpp
integer :: j,k,l,i1(3),subit

! initialization of diffusion temperature and pore pressure array
if ( ifn /= 0 ) then
    i1 = nm
    i1(ifn) = 1
else
    i1 = 0
end if

allocate(dtmpt(nz+2))
allocate(dtmpp(nz+2))
! loop over subfault patch

do l = 1, i1(3)
    do k = 1, i1(2)
        do j = 1, i1(1)
            !update boundary condition
            tmpt(j,k,l,1) = tmpt(j,k,l,3)
            tmpt(j,k,l,nz+2) = tmpt(j,k,l,nz)
            pprs(j,k,l,1) = pprs(j,k,l,3)
            pprs(j,k,l,nz+2) = pprs(j,k,l,nz)

            ! loop over substep in thermal pressurization
            if (tpw(j,k,l)<1e-3) then ! slip on plane
                do subit = 1, subnt
                    !update temperature
                    dtmpt = tmpt(j,k,l,:)
                    call ddz2(dtmpt)
                    dtmpt = dtmpt/(tpdz*tpdz)*ath(j,k,l) + shearheat(j,k,l)/ &
                           (rhoc(j,k,l)*tpdz)
                    ! Tnew = Told + dT*dt_sub
                    tmpt(j,k,l,:) = tmpt(j,k,l,:) + dtmpt*tpsubdt
    
                    !update pore pressure
                    dtmpp = pprs(j,k,l,:)
                    call ddz2(dtmpp)
                    dtmpp = dtmpp/(tpdz*tpdz)*ahy(j,k,l) + tplam(j,k,l)*dtmpt
                    ! Pnew = Pold + dP*dt_sub
                    pprs(j,k,l,:) = pprs(j,k,l,:) + dtmpp*tpsubdt
                end do

            else ! Gaussian shear zone
                do subit = 1, subnt
                    !update temperature
                    dtmpt = tmpt(j,k,l,:)
                    call ddz2(dtmpt)
                    dtmpt = dtmpt/(tpdz*tpdz)*ath(j,k,l) + shearheat(j,k,l)* &
                           exp(-z*z/(2*tpw(j,k,l)*tpw(j,k,l)))/&
                            (rhoc(j,k,l)*tpw(j,k,l)*sqrt(2*pi))
                    ! Tnew = Told + dT*dt_sub
                    tmpt(j,k,l,:) = tmpt(j,k,l,:) + dtmpt*tpsubdt
    
                    !update pore pressure
                    dtmpp = pprs(j,k,l,:)
                    call ddz2(dtmpp)
                    dtmpp = dtmpp/(tpdz*tpdz)*ahy(j,k,l) + tplam(j,k,l)*dtmpt
                    ! Pnew = Pold + dP*dt_sub
                    pprs(j,k,l,:) = pprs(j,k,l,:) + dtmpp*tpsubdt
                end do
            end if


            ! store temperature and pore pressure on fault
            temp(j,k,l) = tmpt(j,k,l,2)
            porep(j,k,l) = pprs(j,k,l,2)
        end do 
    end do
end do
deallocate(dtmpt)
deallocate(dtmpp)
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

