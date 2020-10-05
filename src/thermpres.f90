! Thermal pressurization
module m_thermpres
implicit none
integer :: subnt
real,parameter:: mintplz = 100
real,dimension(:),allocatable :: z
real,dimension(:,:,:,:),allocatable :: tmpt,pprs
real,dimension(:,:,:,:),allocatable :: vold2,vold1,tsold1
real,dimension(:,:,:), allocatable :: shearheat
real,dimension(:,:,:,:),allocatable :: shearheat_history
real,allocatable,dimension(:) :: omega,dtmpt,dtmpp
contains

! Initiate array used in thermalpressurization
subroutine ini_thermpres
use m_globals
integer :: i1(3),j,k,l

! initialization of diffusion temperature and pore pressure array
if ( ifn /= 0 ) then
    i1 = nm
    i1(ifn) = 1
else
    return
end if

! initialization diffusion distance array
! tpnz = floor( tplz / tpdz ) + 1


j = i1(1)
k = i1(2)
l = i1(3)
! allocate previous slip velocity and shear traction
allocate( &
    vold2(j,k,l,3),  &
    vold1(j,k,l,3),  &
    tsold1(j,k,l,3)  )

! allocate heat production array
allocate( &
    shearheat(j,k,l))

vold2 = 0.0
vold1 = 0.0
tsold1 = 0.0
shearheat = 0.0

if (tp_method == 'fd') then
    tplz = max(mintplz,sqrt(4*(nt-1)*dt*max(ath,ahy))*1.2)
    tpdz = tplz/(tpnz - 1)
    allocate (z(tpnz))


    if (any(tpsubdt > 0.25 * tpdz * tpdz/max(ath,ahy))) then
         write(0,*) 'Explosion: tpsubdt is larger than the critical length)'
    end if

    subnt = floor(dt/tpsubdt)



    ! allocate temperature and pore pressure in diffusion distance
    allocate( &
        tmpt(j,k,l,tpnz+2),  &
        pprs(j,k,l,tpnz+2)   )

    allocate(omega(tpnz))
    allocate(dtmpt(tpnz))
    allocate(dtmpp(tpnz))

    ! initialization
    tmpt = 0.0
    pprs = 0.0
    omega = 0.0
    dtmpt = 0.0
    dtmpp = 0.0

    !set up initial value in shear zone 
    do j = 1, tpnz+2
        tmpt(:,:,:,j) = tini(:,:,:)
        pprs(:,:,:,j) = pini(:,:,:)
    end do

else if (tp_method == 'Green') then
    allocate(shearheat_history(j,k,l,nt))
end if

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
subroutine compute_shearheat(it,tp_method)
integer,intent(in) :: it
character(*), intent(in) :: tp_method
    shearheat = (sum(vold2 * tsold1,4) + sum(vold1 * tsold1,4))/2.

    ! closer results relative to TPV105-2D
    ! shearheat = sum(vold2 * tsold1,4)
    ! write(0,*) shearheat(151,2,1)
    if (tp_method == 'Green') shearheat_history(:,:,:,it) = shearheat(:,:,:)


end subroutine

subroutine init_vw
use m_globals
    vw = pi * ath * ((Tempw - tini)/(tau_c/rhoc))**2/AspD*1e-3
end subroutine


subroutine update_vw
use m_globals
    vw = pi * ath * ((Tempw - temp)/(tau_c/rhoc))**2/AspD*1e-3
end subroutine


subroutine update_thermpres
use m_globals
if (tp_method == 'fd') then
    call update_thermpres_fd
else if (tp_method == 'Green') then
    call update_thermpres_green
else
    stop 'Wrong thermal pressurization method'
end if
end subroutine update_thermpres

function green_th0(t)
use m_globals
real,intent(in) :: t
real :: green_th0(size(ath,1),size(ath,2),size(ath,3))

if (t<0) stop '(TP-Green_th0) t cannot be negative'
green_th0 = 1./(rhoc * 2 * sqrt(pi*(ath*t+0.5*tpw*tpw)))
end function

function green_hy0(t)
use m_globals
real,intent(in) :: t
real :: green_hy0(size(ahy,1),size(ahy,2),size(ahy,3))
if (t<0) stop '(TP-Green_hy0) t cannot be negative'
green_hy0 = tplam/(rhoc * 2 * sqrt(pi) * (ahy-ath)) * &
            (ahy/sqrt(ahy*t+0.5*tpw*tpw)-&
             ath/sqrt(ath*t+0.5*tpw*tpw))
end function

subroutine update_thermpres_green
use m_globals
integer :: i 

temp = tini
porep = pini
do i = 1, it
    temp(:,:,:) = temp(:,:,:) + dt * green_th0(tm - (i-1)*dt) * &
                              shearheat_history(:,:,:,i)
    porep(:,:,:) = porep(:,:,:) + dt * green_hy0(tm - (i-1)*dt) * &
                              shearheat_history(:,:,:,i)
end do 

end subroutine

! FD solve thermal pressurization equation
subroutine update_thermpres_fd
use m_globals
integer :: j,k,l,m,i1(3),i2(3),subit

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

            do m = 1, tpnz
                z(m)  = ( m - 1 ) * tpdz(j,k,l)
            end do

            if (tpw(j,k,l)<1e-5) then ! slip on plane
                omega = 0
                omega(1) = shearheat(j,k,l)/tpdz(j,k,l)
            else
                omega = shearheat(j,k,l) * &
                       exp(-z*z/(2*tpw(j,k,l)*tpw(j,k,l)))/&
                        (tpw(j,k,l)*sqrt(2*pi))
            end if

            ! loop over substep in thermal pressurization
            do subit = 1, subnt

                !update boundary condition
                tmpt(j,k,l,1) = tmpt(j,k,l,3)
                tmpt(j,k,l,tpnz+2) = tmpt(j,k,l,tpnz)
                pprs(j,k,l,1) = pprs(j,k,l,3)
                pprs(j,k,l,tpnz+2) = pprs(j,k,l,tpnz)

                ! diffusion
                dtmpt = ath(j,k,l)*(tmpt(j,k,l,3:tpnz+2)-2*tmpt(j,k,l,2:tpnz+1) + &
                        tmpt(j,k,l,1:tpnz))/(tpdz(j,k,l)*tpdz(j,k,l))+&
                        omega/rhoc(j,k,l) 
                dtmpp = ahy(j,k,l)*(pprs(j,k,l,3:tpnz+2)-2*pprs(j,k,l,2:tpnz+1) + &
                        pprs(j,k,l,1:tpnz))/(tpdz(j,k,l)*tpdz(j,k,l))+&
                        tplam(j,k,l)*dtmpt

                ! undrained, adiabatic
                ! dtmpt = omega/rhoc(j,k,l)
                ! dtmpp = tplam(j,k,l)*dtmpt

                tmpt(j,k,l,2:tpnz+1) = tmpt(j,k,l,2:tpnz+1) + dtmpt*tpsubdt
                pprs(j,k,l,2:tpnz+1) = pprs(j,k,l,2:tpnz+1) + dtmpp*tpsubdt

            end do

            ! store temperature and pore pressure on fault
            temp(j,k,l) = tmpt(j,k,l,2)
            porep(j,k,l) = pprs(j,k,l,2)
        end do 
    end do
end do

end subroutine update_thermpres_fd


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

