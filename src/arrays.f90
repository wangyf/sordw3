! Allocate arrays
module m_arrays
implicit none
contains

subroutine arrays
use m_globals
integer :: i1(3), i2(3), j, k, l, j1, k1, l1, j2, k2, l2

! 3d
j = nm(1)
k = nm(2)
l = nm(3)

! 3d vectors
allocate(          &
    vv(j,k,l,3),   &
    uu(j,k,l,3),   &
    w1(j,k,l,3),   &
    w2(j,k,l,3) )

if ( (eplasticity == 'plastic') .or. (ivols == 'yes') ) then
    allocate(          & 
        si1(j,k,l,3),  &
        si2(j,k,l,3),  &
        z1(j,k,l,3),   & 
        z2(j,k,l,3) )
end if

if ( eplasticity == 'plastic' ) then
    if ( master ) write( 0, * ) 'Elastoplastic Material'
    allocate(          &
        dep1(j,k,l,3), & 
        dep2(j,k,l,3), & 
        ep1(j,k,l,3),  &
        ep2(j,k,l,3) )
end if

! 3d scalars
allocate(         &
    vc(j,k,l),    &
    mr(j,k,l),    &
    lam(j,k,l),   &
    mu(j,k,l),    &
    gam(j,k,l),   &
!   qp(j,k,l),    &
!   qs(j,k,l),    &
    yy(j,k,l),    &
    s1(j,k,l),    &
    s2(j,k,l),    &
    fs1(j,k,l)     )

if ( eplasticity == 'plastic' ) then
    select case(plmodel)
    case('DP1')
        if ( master ) &
            write( 0, * ) 'Drucker Prager Plastic model (I)'
        allocate(         & 
            mco(j,k,l),   &
            phi(j,k,l),   &
            epm(j,k,l),   &
            mur(j,k,l),   &
            r1(j,k,l),    &
            r2(j,k,l),    &
            r3(j,k,l),    &
            r4(j,k,l),    &
            r5(j,k,l) )
    case('DP2')
        if ( master ) &
            write( 0, * ) 'Drucker Prager Plastic model (II)'
        allocate(         & 
            mco(j,k,l),   &
            phi(j,k,l),   &
            plb(j,k,l),   &
            plh(j,k,l),   &
            bk(j,k,l),    &
            mur(j,k,l),   &
            lambda(j,k,l),&
            gammap(j,k,l),&
            r1(j,k,l),    &
            r2(j,k,l))
    case default
        if ( master ) &
            write( 0, * ) 'Please choose a valid plasticity model'
        stop
    end select  
end if

! pml nodes
i1 = min( i2node, i1pml ) - i1node + 1
i2 = i2node - max( i1node, i2pml ) + 1
j1 = i1(1); j2 = i2(1)
k1 = i1(2); k2 = i2(2)
l1 = i1(3); l2 = i2(3)
allocate(         &
    p1(j1,k,l,3), &
    p2(j,k1,l,3), &
    p3(j,k,l1,3), &
    p4(j2,k,l,3), &
    p5(j,k2,l,3), &
    p6(j,k,l2,3)  )

! pml cells
i1 = min( i2cell, i1pml ) - i1cell + 1
i2 = i2cell - max( i1cell, i2pml - 1 ) + 1
j1 = i1(1); j2 = i2(1)
k1 = i1(2); k2 = i2(2)
l1 = i1(3); l2 = i2(3)
allocate(         &
    g1(j1,k,l,3), &
    g2(j,k1,l,3), &
    g3(j,k,l1,3), &
    g4(j2,k,l,3), &
    g5(j,k2,l,3), &
    g6(j,k,l2,3)  )

! PML damping
allocate( dn1(npml), dn2(npml), dc1(npml), dc2(npml) )

! Fault
if ( ifn /= 0 ) then
    i1 = nm
    i1(ifn) = 1
else
    return
end if

j = i1(1)
k = i1(2)
l = i1(3)

! Fault vectors
allocate(          &
    nhat(j,k,l,3), &
    t0(j,k,l,3),   &
    ts0(j,k,l,3),  &
    tp(j,k,l,3),   &
    t1(j,k,l,3),   &
    t2(j,k,l,3),   &
    t3(j,k,l,3)    )

! Fault scalars
if ( friction == 'slipweakening' ) then
    if ( master ) write( 0, * ) 'Slip-Weakening Friction'
    allocate(         & 
        mus(j,k,l),   &
        mud(j,k,l),   &
        dc(j,k,l) )
end if

if ( friction == 'rateandstate' .or. &
     friction == 'thermalpressurization' ) then
    
    if ( master .and. friction == 'rateandstate') &
            write( 0, * ) 'Rate-and-State Friction'
    allocate(         &
        af(j,k,l),    &
        bf(j,k,l),    &
        v0(j,k,l),    &
        f0(j,k,l),    &
        ll(j,k,l),    &
        fw(j,k,l),    &
        vw(j,k,l),    &
        psi(j,k,l),   &
        svtrl(j,k,l), &
        svold(j,k,l), &
        sv0(j,k,l),   &
        f4(j,k,l),    &
        fun(j,k,l),   &
        dfun(j,k,l),  &
        delf(j,k,l)   )

    if ( friction == 'thermalpressurization') then
            if (master) write( 0, * ) 'Thermal Pressurization Friction'
        allocate(     &
        ath(j,k,l),   &
        ahy(j,k,l),   &
        rhoc(j,k,l),  &
        tplam(j,k,l), &
        tpw(j,k,l),   &
        tini(j,k,l),  &
        pini(j,k,l),  &
        temp(j,k,l),  &
        porep(j,k,l)  )
    end if 

end if




allocate(         &
    co(j,k,l),    &
    area(j,k,l),  &
    rhypo(j,k,l), &
    lamf(j,k,l),  &
    muf(j,k,l),   &
    sl(j,k,l),    &
    psv(j,k,l),   &
    trup(j,k,l),  &
    tarr(j,k,l),  &
    tn(j,k,l),    &
    ts(j,k,l),    &
    f1(j,k,l),    &
    f2(j,k,l),    &
    f3(j,k,l),    &
    f5(j,k,l),    &
    f6(j,k,l) )

if ( pcdep == 'yes' ) then
    if ( master ) write( 0, * ) 'Regularized Response to Tn Variation'
    allocate(         & 
        lpc(j,k,l),   &
        tnold(j,k,l), &
        tnpc(j,k,l)   )
end if

end subroutine

end module

