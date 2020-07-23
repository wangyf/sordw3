! Rupture boundary condition
module m_rupture
implicit none
real :: lastvalue
contains

! Rupture initialization
subroutine rupture_init
use m_globals
use m_collective
use m_surfnormals
use m_inivolstress
use m_util
use m_fieldio
use m_stats
use m_thermpres, only : ini_thermpres
real :: xhypo(3), xi(3), w  !, rr
integer :: i1(3), i2(3), i, j, k, l

if ( ifn /= 0 ) then
    i1 = i1core
    i2 = i2core
    i1(ifn) = irup
    i2(ifn) = irup
    call nodenormals( nhat, w1, dx, i1, i2, ifn )
    area = sign( 1, faultnormal ) * sqrt( sum( nhat * nhat, 4 ) )
 
    t0 = 0.
    lastvalue = 0.0
end if 

if ( ivols == 'yes' ) call inivolstress
if ( ifn == 0 )  return

if ( master ) write( 0, * ) 'Rupture initialization'

! I/O
t1 = 0.0
t2 = 0.0
t3 = 0.0
f5 = 0.0

if ( friction == 'slipweakening' ) then
    co = 0.0
    mus = 0.0
    mud = 0.0
    dc = 0.0
    call fieldio2d( '<>', 'co',  co          )
    call fieldio2d( '<>', 'mus', mus         )
    call fieldio2d( '<>', 'mud', mud         )
    call fieldio2d( '<>', 'dc',  dc          )   
end if

if ( friction == 'rateandstate' .or. &
     friction == 'thermalpressurization') then
    af = 0.0 
    bf = 0.0
    v0 = 0.0
    f0 = 0.0
    ll = 0.0
    fw = 0.0
    vw = 0.0
    psi = 0.0

    call fieldio2d( '<>', 'af',  af          )
    call fieldio2d( '<>', 'bf',  bf          )
    call fieldio2d( '<>', 'v0',  v0          )
    call fieldio2d( '<>', 'f0',  f0          )
    call fieldio2d( '<>', 'll',  ll          )
    call fieldio2d( '<>', 'fw',  fw          )
    call fieldio2d( '<>', 'vw',  vw          )


    if( friction == 'thermalpressurization') then
        ath = 0.0
        ahy = 0.0
        rhoc = 0.0
        tplam = 0.0
        tpw = 0.0
        temp = 0.0
        porep = 0.0

    call fieldio2d( '<>', 'ath',    ath     )
    call fieldio2d( '<>', 'ahy',    ahy     )
    call fieldio2d( '<>', 'rhoc',   rhoc    )
    call fieldio2d( '<>', 'tplam',  tplam   )
    call fieldio2d( '<>', 'tpw',    tpw     )
    call fieldio2d( '<>', 'tini',   tini    )! initial temperature
    call fieldio2d( '<>', 'pini',   pini    )! initial pore pressure
    end if
end if

if ( pcdep == 'yes' ) then
    lpc = 0.0
    call fieldio2d( '<>', 'lpc', lpc         )
end if

call fieldio2d( '<>', 's11', t1(:,:,:,1) )
call fieldio2d( '<>', 's22', t1(:,:,:,2) )
call fieldio2d( '<>', 's33', t1(:,:,:,3) )
call fieldio2d( '<>', 's23', t2(:,:,:,1) )
call fieldio2d( '<>', 's31', t2(:,:,:,2) )
call fieldio2d( '<>', 's12', t2(:,:,:,3) )
call fieldio2d( '<',  'ts',  t3(:,:,:,1) ) !here only input ts td and tn
call fieldio2d( '<',  'td',  t3(:,:,:,2) )
call fieldio2d( '<',  'tn',  t3(:,:,:,3) )

!! Normal traction check
!i1 = maxloc( t3(:,:,:,3) )
!rr = t3(i1(1),i1(2),i1(3),3)
!i1(ifn) = irup
!i1 = i1 + nnoff
!if ( rr > 0.0 ) write( 0, * ) 'warning: positive normal traction: ', rr, i1

! Lock fault in PML region
i1 = i1pml + 1
i2 = i2pml - 1
call set_halo( co, 1e20, i1, i2 )

! Normal vectors
!i1 = i1core
!i2 = i2core
!i1(ifn) = irup
!i2(ifn) = irup
!call nodenormals( nhat, w1, dx, i1, i2, ifn )
!area = sign( 1, faultnormal ) * sqrt( sum( nhat * nhat, 4 ) )


! output area of each node on fault
f1 = area
call fieldio2d( '>', 'area', f1(:,:,:) )

call invert( f1 )
do i = 1, 3
    nhat(:,:,:,i) = nhat(:,:,:,i) * f1
end do
call fieldio2d( '>', 'nhat1', nhat(:,:,:,1) )
call fieldio2d( '>', 'nhat2', nhat(:,:,:,2) )
call fieldio2d( '>', 'nhat3', nhat(:,:,:,3) )

do i = 1, 3
    j = modulo( i , 3 ) + 1
    k = modulo( i + 1, 3 ) + 1
    t0(:,:,:,i) = t0(:,:,:,i)   + &
    t1(:,:,:,i) * nhat(:,:,:,i) + &
    t2(:,:,:,j) * nhat(:,:,:,k) + &
    t2(:,:,:,k) * nhat(:,:,:,j)
end do

t2(:,:,:,1) = nhat(:,:,:,2) * slipvector(3) - nhat(:,:,:,3) * slipvector(2)
t2(:,:,:,2) = nhat(:,:,:,3) * slipvector(1) - nhat(:,:,:,1) * slipvector(3)
t2(:,:,:,3) = nhat(:,:,:,1) * slipvector(2) - nhat(:,:,:,2) * slipvector(1)
f1 = sqrt( sum( t2 * t2, 4 ) )
call invert( f1 )
do i = 1, 3
    t2(:,:,:,i) = t2(:,:,:,i) * f1
end do

t1(:,:,:,1) = t2(:,:,:,2) * nhat(:,:,:,3) - t2(:,:,:,3) * nhat(:,:,:,2)
t1(:,:,:,2) = t2(:,:,:,3) * nhat(:,:,:,1) - t2(:,:,:,1) * nhat(:,:,:,3)
t1(:,:,:,3) = t2(:,:,:,1) * nhat(:,:,:,2) - t2(:,:,:,2) * nhat(:,:,:,1)
f1 = sqrt( sum( t1 * t1, 4 ) )
call invert( f1 )
do i = 1, 3
    t1(:,:,:,i) = t1(:,:,:,i) * f1
end do

! export shear and normal stress from stress tensor
if (ivols=='yes' .and. maxval(abs(t3))<1e-3) then
    !ts
    t3(:,:,:,1) = sum(t0 * t1, 4)
    !td
    t3(:,:,:,2) = sum(t0 * t2, 4)
    !tn
    t3(:,:,:,3) = sum(t0 * nhat, 4)

    call fieldio2d( '>', 'ts',  t3(:,:,:,1) )
    call fieldio2d( '>', 'td',  t3(:,:,:,2) )
    call fieldio2d( '>', 'tn',  t3(:,:,:,3) )

    t3 = 0.
end if

do i = 1, 3
    t0(:,:,:,i) = t0(:,:,:,i) + &
    t3(:,:,:,1) * t1(:,:,:,i) + &
    t3(:,:,:,2) * t2(:,:,:,i) + &
    t3(:,:,:,3) * nhat(:,:,:,i)
end do

f1 = sum( t0 * nhat, 4 ) 
do i = 1, 3
    ts0(:,:,:,i) = t0(:,:,:,i) - f1 * nhat(:,:,:,i)
end do

if ( ( rcrit > 0.0 .and. vrup > 0.0 ) .or. ( rnucl > 0.0 ) ) then
    xhypo = 0.0
    xi = ihypo - nnoff
    i1 = floor( xi )
    if ( all( i1 >= 1 .and. i1 < nm ) ) then
        do l = i1(3), i1(3)+1
        do k = i1(2), i1(2)+1
        do j = i1(1), i1(1)+1
            w = (1.0-abs(xi(1)-j)) * (1.0-abs(xi(2)-k)) * (1.0-abs(xi(3)-l))
            do i = 1, 3
                xhypo(i) = xhypo(i) + w * w1(j,k,l,i)
            end do
        end do
        end do
        end do
    end if
    call rbroadcast1( xhypo, ip2root )
    do i = 1, 3
        select case( ifn )
        case ( 1 ); t2(1,:,:,i) = w1(irup,:,:,i) - xhypo(i)
        case ( 2 ); t2(:,1,:,i) = w1(:,irup,:,i) - xhypo(i)
        case ( 3 ); t2(:,:,1,i) = w1(:,:,irup,i) - xhypo(i)
        end select
    end do
    rhypo = sqrt( sum( t2 * t2, 4 ) )
end if

! Resample mu on to fault plane nodes for moment calculatioin
select case( ifn )
case ( 1 ); lamf(1,:,:) = lam(irup,:,:); muf(1,:,:) = mu(irup,:,:)
case ( 2 ); lamf(:,1,:) = lam(:,irup,:); muf(:,1,:) = mu(:,irup,:)
case ( 3 ); lamf(:,:,1) = lam(:,:,irup); muf(:,:,1) = mu(:,:,irup)
end select
call invert( lamf )
call invert( muf )
j = nm(1) - 1
k = nm(2) - 1
l = nm(3) - 1
if ( ifn /= 1 ) lamf(2:j,:,:) = 0.5 * (lamf(2:j,:,:) + lamf(1:j-1,:,:))
if ( ifn /= 2 ) lamf(:,2:k,:) = 0.5 * (lamf(:,2:k,:) + lamf(:,1:k-1,:))
if ( ifn /= 3 ) lamf(:,:,2:l) = 0.5 * (lamf(:,:,2:l) + lamf(:,:,1:l-1))
if ( ifn /= 1 ) muf(2:j,:,:) = 0.5 * (muf(2:j,:,:) + muf(1:j-1,:,:))
if ( ifn /= 2 ) muf(:,2:k,:) = 0.5 * (muf(:,2:k,:) + muf(:,1:k-1,:))
if ( ifn /= 3 ) muf(:,:,2:l) = 0.5 * (muf(:,:,2:l) + muf(:,:,1:l-1))
call invert( muf )

! Initial state, can be overwritten by read_checkpoint
psv   =  0.0
trup  =  1e9
tarr  =  0.0
efric =  0.0

! Halos
call scalar_swap_halo( co,    nhalo )
call scalar_swap_halo( area,  nhalo )
call scalar_swap_halo( rhypo, nhalo )
call vector_swap_halo( nhat,  nhalo )
call vector_swap_halo( t0,    nhalo )
call vector_swap_halo( ts0,   nhalo )

if ( friction == 'slipweakening' ) then
    call scalar_swap_halo( mus,   nhalo )
    call scalar_swap_halo( mud,   nhalo )
    call scalar_swap_halo( dc,    nhalo )
end if

if ( friction == 'rateandstate' .or. &
     friction == 'thermalpressurization') then
    call scalar_swap_halo( af,   nhalo )
    call scalar_swap_halo( bf,   nhalo )
    call scalar_swap_halo( v0,   nhalo )
    call scalar_swap_halo( f0,   nhalo )
    call scalar_swap_halo( ll,   nhalo )    
    call scalar_swap_halo( fw,   nhalo )
    call scalar_swap_halo( vw,   nhalo )

    if (friction == 'thermalpressurization') then
       call scalar_swap_halo( ath,     nhalo ) 
       call scalar_swap_halo( ahy,     nhalo ) 
       call scalar_swap_halo( rhoc,    nhalo ) 
       call scalar_swap_halo( tplam,   nhalo ) 
       call scalar_swap_halo( tpw,     nhalo ) 
       call scalar_swap_halo( temp,    nhalo ) 
       call scalar_swap_halo( porep,   nhalo ) 
    end if
end if

if ( pcdep == 'yes' ) call scalar_swap_halo( lpc,  nhalo )

! apply for thermal pressurization
if (friction == 'thermalpressurization') call ini_thermpres

end subroutine

!------------------------------------------------------------------------------!

! Rupture boundary condition
subroutine rupture
use m_globals
use m_collective
use m_bc
use m_util
use m_fieldio
use m_stats
use m_thermpres
integer ::  niter, nmax, i1(3), i2(3), i, j, k, l,         &
           j1, k1, l1, j2, k2, l2, j3, k3, l3, j4, k4, l4
real :: tol

if ( ifn == 0 ) return
if ( verb ) write( 0, * ) 'Rupture'

if ( friction == 'rateandstate' ) then
    nmax = 30
    tol = 1.e-3
end if

! Indices
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

if ( it == 1 ) then
    tp = 0.
else
    if ( tm < tmnucl ) then
        f2 = exp( (tm - tmnucl)**2/(tm*(tm -2*tmnucl)) )
    else
        f2 = 1.
    end if

    f1 = 0.
    where( rhypo < rnucl ) f1 = exp( rhypo**2/(rhypo**2-rnucl**2) ) * f2 * delts

    do i = 1, 3
        tp(:,:,:,i) = ts0(:,:,:,i) * f1
    end do
end if

if ( it == 1 ) then
    w2 = vv
    if ( maxval(abs(w2(:,:,:,1))) > 1.e-20 ) then
        vv(j3:j4,k3:k4,l3:l4,1) = w2(j3:j4,k3:k4,l3:l4,1)
        vv(j3:j4,k3:k4,l3:l4,2) = &
           - nhat(:,:,:,1)*nhat(:,:,:,2)/(1 - nhat(:,:,:,1)*nhat(:,:,:,1)) * w2(j3:j4,k3:k4,l3:l4,1)
        vv(j3:j4,k3:k4,l3:l4,3) = &
           - nhat(:,:,:,1)*nhat(:,:,:,3)/(1 - nhat(:,:,:,1)*nhat(:,:,:,1)) * w2(j3:j4,k3:k4,l3:l4,1)

        vv(j1:j2,k1:k2,l1:l2,1) = w2(j1:j2,k1:k2,l1:l2,1)
        vv(j1:j2,k1:k2,l1:l2,2) = &
           - nhat(:,:,:,1)*nhat(:,:,:,2)/(1 - nhat(:,:,:,1)*nhat(:,:,:,1)) * w2(j1:j2,k1:k2,l1:l2,1)
        vv(j1:j2,k1:k2,l1:l2,3) = &
           - nhat(:,:,:,1)*nhat(:,:,:,3)/(1 - nhat(:,:,:,1)*nhat(:,:,:,1)) * w2(j1:j2,k1:k2,l1:l2,1)
    end if
    if ( maxval(abs(w2(:,:,:,2))) > 1.e-20 ) then
        vv(j3:j4,k3:k4,l3:l4,1) = vv(j3:j4,k3:k4,l3:l4,1) &
           - nhat(:,:,:,1)*nhat(:,:,:,2)/(1 - nhat(:,:,:,2)*nhat(:,:,:,2)) * w2(j3:j4,k3:k4,l3:l4,2)
        vv(j3:j4,k3:k4,l3:l4,2) = vv(j3:j4,k3:k4,l3:l4,2) + w2(j3:j4,k3:k4,l3:l4,2)
        vv(j3:j4,k3:k4,l3:l4,3) = vv(j3:j4,k3:k4,l3:l4,2) &
           - nhat(:,:,:,2)*nhat(:,:,:,3)/(1 - nhat(:,:,:,2)*nhat(:,:,:,2)) * w2(j3:j4,k3:k4,l3:l4,2)

        vv(j1:j2,k1:k2,l1:l2,1) = vv(j1:j2,k1:k2,l1:l2,1) &
           - nhat(:,:,:,1)*nhat(:,:,:,2)/(1 - nhat(:,:,:,2)*nhat(:,:,:,2)) * w2(j1:j2,k1:k2,l1:l2,2)
        vv(j1:j2,k1:k2,l1:l2,2) = vv(j1:j2,k1:k2,l1:l2,2) + w2(j1:j2,k1:k2,l1:l2,2)
        vv(j1:j2,k1:k2,l1:l2,3) = vv(j1:j2,k1:k2,l1:l2,2) &
           - nhat(:,:,:,2)*nhat(:,:,:,3)/(1 - nhat(:,:,:,2)*nhat(:,:,:,2)) * w2(j1:j2,k1:k2,l1:l2,2)
    end if
    if ( maxval(abs(w2(:,:,:,3))) > 1.e-20 ) then
        vv(j3:j4,k3:k4,l3:l4,1) = vv(j3:j4,k3:k4,l3:l4,1) &
           - nhat(:,:,:,1)*nhat(:,:,:,3)/(1 - nhat(:,:,:,3)*nhat(:,:,:,3)) * w2(j3:j4,k3:k4,l3:l4,3)
        vv(j3:j4,k3:k4,l3:l4,2) = vv(j3:j4,k3:k4,l3:l4,2) &
           - nhat(:,:,:,2)*nhat(:,:,:,3)/(1 - nhat(:,:,:,3)*nhat(:,:,:,3)) * w2(j3:j4,k3:k4,l3:l4,3)
        vv(j3:j4,k3:k4,l3:l4,3) = vv(j3:j4,k3:k4,l3:l4,3) + w2(j3:j4,k3:k4,l3:l4,3)

        vv(j1:j2,k1:k2,l1:l2,1) = vv(j1:j2,k1:k2,l1:l2,1) &
           - nhat(:,:,:,1)*nhat(:,:,:,3)/(1 - nhat(:,:,:,3)*nhat(:,:,:,3)) * w2(j1:j2,k1:k2,l1:l2,3)
        vv(j1:j2,k1:k2,l1:l2,2) = vv(j1:j2,k1:k2,l1:l2,2) &
           - nhat(:,:,:,2)*nhat(:,:,:,3)/(1 - nhat(:,:,:,3)*nhat(:,:,:,3)) * w2(j1:j2,k1:k2,l1:l2,3)
        vv(j1:j2,k1:k2,l1:l2,3) = vv(j1:j2,k1:k2,l1:l2,3) + w2(j1:j2,k1:k2,l1:l2,3)
    end if

!    do i = 2, 3
!    vv(j3:j4,k3:k4,l3:l4,i) = -vv(j3:j4,k3:k4,l3:l4,1) * nhat(:,:,:,i) * &
!                              nhat(:,:,:,1)/(1 - nhat(:,:,:,1)*nhat(:,:,:,1))
!    vv(j1:j2,k1:k2,l1:l2,i) = -vv(j1:j2,k1:k2,l1:l2,1) * nhat(:,:,:,i) * &
!                              nhat(:,:,:,1)/(1 - nhat(:,:,:,1)*nhat(:,:,:,1))
!    end do 
end if
!---------------------------------------------------------------

! Trial traction for zero velocity and zero displacement
! parameter 'c' defined in (5c) in front of normal traction
f3 = area * ( mr(j1:j2,k1:k2,l1:l2) + mr(j3:j4,k3:k4,l3:l4) )
f1 = dt * dt * f3; 
call invert( f1 )

do i = 1, 3
    t1(:,:,:,i) = t0(:,:,:,i) + tp(:,:,:,i) + f1 * dt * &
        ( vv(j3:j4,k3:k4,l3:l4,i) &
        - vv(j1:j2,k1:k2,l1:l2,i) &
        + w1(j3:j4,k3:k4,l3:l4,i) * mr(j3:j4,k3:k4,l3:l4) * dt &
        - w1(j1:j2,k1:k2,l1:l2,i) * mr(j1:j2,k1:k2,l1:l2) * dt )
    t2(:,:,:,i) = t1(:,:,:,i) + f1 * &
        ( uu(j3:j4,k3:k4,l3:l4,i) - uu(j1:j2,k1:k2,l1:l2,i) )
end do

! Normal and Shear traction  [ZS]
tn = sum( t2 * nhat, 4 )
!if ( faultopening == 1 ) tn = min( 0.0, tn )
tn = min( 0.0, tn ) !trial normal stress

f1 = sum( t1 * nhat, 4 ) 
do i = 1, 3
    t3(:,:,:,i) = t1(:,:,:,i) - f1 * nhat(:,:,:,i)
end do
ts = sqrt( sum( t3 * t3, 4 ) ) !trial shear stress

! slip velocity from the previous step (delta_v (n-1/2))
do i = 1, 3     
    t2(:,:,:,i) = vv(j3:j4,k3:k4,l3:l4,i) - vv(j1:j2,k1:k2,l1:l2,i)
end do
if (friction == 'thermalpressurization') call record_svel_ts(t2,'svo1')

if ( friction == 'rateandstate' .or. &
     friction == 'thermalpressurization') then

    svold = sqrt( sum( t2 * t2, 4 ) ) !|delta_v(n-1/2)|
    ! call scalar_swap_halo( sv0,   nhalo )
    
    if ( it == 1 )  then
       
        !psi = af * log( 2.0 * v0 / svold * sinh( ts / (-tn) / af ) )
        psi = af * sngl(dlog( 2.0 * v0 / svold * sinh( dble(ts / (-tn) / af )) ))
        f1 = ts
        sv0 = svold
       
        if ( pcdep == 'yes' ) then 
            tnpc = tn 
            tnold = tn
        end if 
        
        if (friction == 'thermalpressurization') then
            call fieldio( '>', 'temp', temp         )  
            call fieldio( '>', 'porep', porep       ) 
        end if 
    else
        where ( svold < sv0 ) svold = sv0
       
        if ( pcdep == 'yes' ) then 
            tnpc = tnpc + dt * svold / lpc *( tnold - tnpc )
            tnold = tn
        end if
    
        !trail delta_v(n+1/2)
        do i = 1, 3     
            t1(:,:,:,i) = t3(:,:,:,i) * f3(:,:,:) * dt
        end do
        !trial V (n+1/2)
        svtrl = ts * f3 * dt  
    
        f1 = f0 - ( bf - af ) * log( svold / v0 ) 
        
        where ( svold < 1.e-5 )
            f2 = f1
        elsewhere
            f2 = fw + ( f1 - fw ) / ( 1.0 + (svold / vw )**8.0 )**(1.0/8.0)  
        end where
           
! warning in setups of Vini or af            
        if (any (f2 < 0)) then
            i1 = minloc(f2)
            i1(ifn) = -1
            write(0,*) 'f2 is negative at', i1
            !if (svold(i1(1),i1(2),i1(3)) < 1e-20) write(0,*) 'Reason: sliprate is assigned as 0.0'
            stop 'error in af or vini (af to large) or (vini to small)'
        end if  

        !f2 = af * ( log( 2 * v0 ) + log( sinh(f2/af) ) - log( svold ) ) 
        f2 = af * ( log( 2 * v0 ) + sngl(dlog( sinh(dble(f2/af))) ) - log( svold ) ) 

        f4 = exp( - svold * dt / ll )
        
        psi = psi * f4 + ( 1.0 - f4 ) * f2 
    
        f1 = 0.5 * svtrl / v0 * exp( psi / af ) 
        where ( f1 > 1.e6 )
            f1 = af * ( log(svtrl) - log(v0) ) + psi         
        elsewhere
            f1 = af * log( f1 + sqrt( f1*f1 + 1.0 ) )
        end where
    
        f2 = 0.5 * svold / v0 * exp( psi / af )  
        where ( f2 > 1.e6 )
            f2 = af * ( log(svold) - log(v0) ) + psi         
        elsewhere
            f2 = af * log( f2 + sqrt( f2*f2 + 1.0 ) )
        end where
        
        f1 = 0.5 * ( f1 + f2 ) 
    
        if ( pcdep == 'yes' ) then
            f4 = dt * f3 * (-tnpc)
        else
            f4 = dt * f3 * (-tn)
        end if
         
        f2 = psi + 0.82   
        where (f1 > f2)  f1 = f2
    
        i1 = i1node
        i2 = i2node
        i1(ifn) = 1
        i2(ifn) = 1
        delf = 1.0   
    
        do l = i1(3), i2(3)
        do k = i1(2), i2(2)
        do j = i1(1), i2(1)
    
        niter = 0
        
        do while ( abs(delf(j,k,l)) > tol .and. niter <= nmax ) 
    
            fun(j,k,l) = svtrl(j,k,l) - f4(j,k,l) * f1(j,k,l) - v0(j,k,l) * & 
                 ( exp((f1(j,k,l)-psi(j,k,l))/af(j,k,l)) - exp(-(f1(j,k,l)+psi(j,k,l))/af(j,k,l)) )
            dfun(j,k,l) = -f4(j,k,l) - ( exp((f1(j,k,l)-psi(j,k,l))/af(j,k,l)) + &
                 exp(-(f1(j,k,l)+psi(j,k,l))/af(j,k,l)) ) *V0(j,k,l) / af(j,k,l)
    
            delf(j,k,l) = fun(j,k,l) / dfun(j,k,l)
            f1(j,k,l) = f1(j,k,l) - delf(j,k,l)
    
            niter = niter + 1
    !        call set_halo( delf, -1.0, i1core, i2core) 
        end do
    
        end do
        end do
        end do
    
    
    
        !------------------------------------------  
        
        !thermal pressurization
        if (friction == 'thermalpressurization') then
        !V(n+1/2) v0 * (exp((f1-psi)/af) - exp(-(f1+psi)/af))
        !delta_v(n+1/2) = trial_delta_v(n+1/2)/trial_V(n+1/2)*
        ! V(n+1/2) in Steve's Notes
        !do i = 1, 3     
        !    t2(:,:,:,i) = 0.5*(t2(:,:,:,i)+t1(:,:,:,i)/svtrl(:,:,:)* &
        !        v0(:,:,:) * (exp((f1(:,:,:)-psi(:,:,:))/af(:,:,:)) - &
        !        exp(-(f1(:,:,:)+psi(:,:,:))/af(:,:,:))))
        !end do
        !t2 become delta_v(n) = (delta_v(n+1/2)+delta_v(n-1/2))/2
    
            if (verb) write(0,*) 'Thermpres'
            ! compute shear heat production
            call compute_shearheat
            ! solve thermal pressurization eqation set
            call update_thermpres
    
            if ( pcdep == 'yes' ) then 
                f1 = -min( 0.0, tnpc + porep ) * f1
            else
                f1 = -min( 0.0, tn + porep ) * f1
            end if  
    
            call fieldio( '>', 'temp', temp         )  
            call fieldio( '>', 'porep', porep       )  
    
        else
        !normal rate and state friction
            if ( pcdep == 'yes' ) then 
                f1 = -min( 0.0, tnpc ) * f1
            else
                f1 = -min( 0.0, tn ) * f1
            end if
        end if
    
        f2 = f1 / ts
        do i = 1, 3
            t3(:,:,:,i) = f2 * t3(:,:,:,i)
        end do
        ts = f1
    
    end if

end if

if ( friction == 'slipweakening' ) then

    if ( it > 1 ) then 
       ! Slip-weakening friction law
        f1 = mud
        where ( sl < dc ) f1 = f1 + (1.0 - sl / dc) * (mus - mud)
        f1 = -min( 0.0, tn ) * f1 + co
    
        ! Nucleation
        if ( rcrit > 0.0 .and. vrup > 0.0 ) then

            f2 = 1.0
            if ( trelax > 0.0 ) then
                f2 = min( (tm - rhypo / vrup) / trelax, 1.0 )
                f2 = (1.0 - f2) * ts + f2 * (-tn * mud + co)
            end if
    
            if ( rrelax > 0.0 ) then
                f2 = min( (tm*vrup - rhypo) / rrelax, 1.0 )
                f2 = (1.0 - f2) * ts + f2 * (-tn * mud + co)
            end if
    
            if (tslope > 0.0 ) then
                f2 = max(-tn*mud+co,-tn*mud+co+tslope*(rhypo-tm*vrup)*1e3)
            end if
    
            where ( rhypo < min( rcrit, tm * vrup ) .and. f2 < f1 ) f1 = f2
            
        end if
    
        ! Shear traction bounded by friction
        f2 = 1.0
        where ( ts > f1 ) f2 = f1 / ts
        do i = 1, 3
            t3(:,:,:,i) = f2 * t3(:,:,:,i)
        end do
        ts = min( ts, f1 )
    end if

end if

! Total traction
do i = 1, 3
    t1(:,:,:,i) = t3(:,:,:,i) + tn * nhat(:,:,:,i)
end do

! Update acceleration
do i = 1, 3
    f2 = area * ( t1(:,:,:,i) - t0(:,:,:,i)  - tp(:,:,:,i) )
    w1(j1:j2,k1:k2,l1:l2,i) = w1(j1:j2,k1:k2,l1:l2,i) + f2
    w1(j3:j4,k3:k4,l3:l4,i) = w1(j3:j4,k3:k4,l3:l4,i) - f2
end do
call vector_bc( w1, bc1, bc2, i1bc, i2bc )

if (friction == 'thermalpressurization') call record_svel_ts(t3,'tso1')
! Output
call fieldio( '>', 't1',  t1(:,:,:,1) )
call fieldio( '>', 't2',  t1(:,:,:,2) )
call fieldio( '>', 't3',  t1(:,:,:,3) )
call fieldio( '>', 'ts1', t3(:,:,:,1) )
call fieldio( '>', 'ts2', t3(:,:,:,2) )
call fieldio( '>', 'ts3', t3(:,:,:,3) )
call fieldio( '>', 'tsm', ts          )
call fieldio( '>', 'tnm', tn          )
call fieldio( '>', 'fr',  f1          )

if ( friction == 'rateandstate' ) then
    call fieldio( '>', 'psi', psi         )
end if

call set_halo( ts,      -1.0, i1core, i2core ); tsmax = maxval( ts ) 
call set_halo( tn,  huge(dt), i1core, i2core ); tnmin = minval( tn )
call set_halo( tn, -huge(dt), i1core, i2core ); tnmax = maxval( tn )
call set_halo( tn,       0.0, i1core, i2core )

! Slip acceleration
do i = 1, 3
    t2(:,:,:,i) = &
        w1(j3:j4,k3:k4,l3:l4,i) * mr(j3:j4,k3:k4,l3:l4) - &
        w1(j1:j2,k1:k2,l1:l2,i) * mr(j1:j2,k1:k2,l1:l2)
end do
f2 = sqrt( sum( t2 * t2, 4 ) )
call fieldio( '>', 'sa1', t2(:,:,:,1) )
call fieldio( '>', 'sa2', t2(:,:,:,2) )
call fieldio( '>', 'sa3', t2(:,:,:,3) )
call fieldio( '>', 'sam', f2          )
call set_halo( f2, -1.0, i1core, i2core )
samax = maxval( f2 )


! Friction + fracture energy
!sv(n+1/2) = sv(n-1/2) + dt * sa(n)
! ( sv(n+1/2) + sv(n-1/2) )/2 = sv(n-1/2) + 0.5 * dt * sa(n)
select case( ifn )
case ( 1 )
t2(1,:,:,:) = vv(irup+1,k3:k4,l3:l4,:) - vv(irup,k1:k2,l1:l2,:) + &
                 0.5 * dt * t2(1,k3:k4,l3:l4,:)
case ( 2 )
t2(:,1,:,:) = vv(j3:j4,irup+1,l3:l4,:) - vv(j1:j2,irup,l1:l2,:) + &
                 0.5 * dt * t2(j3:j4,1,l3:l4,:)
case ( 3 )
t2(:,:,1,:) = vv(j3:j4,k3:k4,irup+1,:) - vv(j1:j2,k1:k2,irup,:) + &
                 0.5 * dt * t2(j3:j4,k3:k4,1,:)
end select

f2 = sum( t1 * t2, 4 )   !ts * sv density
f5 = f5 + (f2 + f6) * dt / 2
f6 = f2
call fieldio( '>', 'erf', f5 )
f2 = f2 * area
call set_halo( f2, 0.0, i1core, i2core )

efric = efric + dt * (sum( f2 ) + lastvalue)/2 !use triangle integral method
lastvalue = sum( f2 )

! Strain energy
t2 = uu(j3:j4,k3:k4,l3:l4,:) - uu(j1:j2,k1:k2,l1:l2,:)
f2 = sum( (t0 + tp + t1) * t2, 4 )
call fieldio( '>', 'ere', f2 )
f2 = f2 * area
call set_halo( f2, 0.0, i1core, i2core )
estrain = 0.5 * sum( f2 )

eradiat = estrain - efric

! Stress drop
t2 = uu(j3:j4,k3:k4,l3:l4,:) - uu(j1:j2,k1:k2,l1:l2,:)
f2 = sum((t0 + tp - t1) * t2,4) * area
call set_halo( f2, 0.0, i1core, i2core)
strdropint = sum( f2 ) 
f2 = sqrt(sum(t2*t2,4))*area
call set_halo( f2, 0.0, i1core, i2core)
slipint = sum( f2 )

! Moment (negelcts opening lambda contribution)
f2 = muf * area * sqrt( sum( t2 * t2, 4 ) )
call set_halo( f2, 0.0, i1core, i2core )
moment = sum( f2 )


end subroutine

end module

