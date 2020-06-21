! Global variables
module m_globals
implicit none

! Input parameters, see default-prm.py for documentation
integer, dimension(3) :: np3, nn, bc1, bc2, n1expand, n2expand
integer :: nt, itstats, itio, itcheck, itstop, npml, ppml, oplevel, mpin, &
    mpout, debug, faultopening, irup, faultnormal, nsource
real :: tm0, dt, dx(3), rho1, rho2, vp1, vp2, vs1, vs2, gam1, gam2, hourglass(2), &
    vdamp, rexpand, affine(9), gridnoise, ihypo(3), vpml, slipvector(3)
real :: period, source1(3), source2(3), strike,dip,rake,m0, &
        vrup, rcrit, trelax, rrelax, tslope, &
        svtol, rnucl, tmnucl, delts, tv, pleta,tpdz, tplz, tpsubdt
character(30) :: source, timefunction, eplasticity, plmodel, friction, pcdep, ivols

! Miscellaneous parameters
real, parameter :: pi = 3.14159265
real :: &
    mptimer,        & ! MPI timing
    iotimer,        & ! I/O timing
    tm                ! time
integer :: &
    it,             & ! current time step
    ifn,            & ! fault normal component=abs(faultnormal)
    ip,             & ! process rank
    ipid,           & ! processor ID
    np0               ! number of processes available
integer, dimension(3) :: &
    nl3,            & ! number of mesh nodes per process
    nm,             & ! size of local 3D arrays
    nhalo,          & ! number of ghost nodes
    ip3,            & ! 3D process rank
    ip3root,        & ! 3D root process rank
    ip2root,        & ! 2D root process rank
    i1bc, i2bc,     & ! model boundary
    i1pml, i2pml,   & ! PML boundary
    i1core, i2core, & ! core region
    i1node, i2node, & ! node region
    i1cell, i2cell, & ! cell region
    nnoff             ! offset between local and global indices
logical :: &
    sync,           & ! synchronize processes
    verb,           & ! print messages
    master            ! master process flag
character(256) :: &
    str               ! string for storing file names

! 1D dynamic arrays
real, allocatable, dimension(:) :: &
    dx1, dx2, dx3,  & ! x, y, z rectangular element size
    dn1,            & ! pml node damping -2*d     / (2+d*dt)
    dn2,            & ! pml node damping  2       / (2+d*dt)
    dc1,            & ! pml cell damping (2-d*dt) / (2+d*dt)
    dc2               ! pml cell damping  2*dt    / (2+d*dt)

! PML state
real, allocatable, dimension(:,:,:,:) :: &
    n1, n2, n3,     & ! surface normal near boundary
    n4, n5, n6,     & ! surface normal far boundary
    p1, p2, p3,     & ! pml momentum near side
    p4, p5, p6,     & ! pml momentum far side
    g1, g2, g3,     & ! pml gradient near side
    g4, g5, g6        ! pml gradient far side

! B matrix
real, allocatable, dimension(:,:,:,:,:) :: bb

! Volume fields
real, allocatable, target, dimension(:,:,:) :: &
    vc,             & ! cell volume
    mr,             & ! mass ratio
    lam, mu,        & ! Lame parameters
    gam,            & ! viscosity
!    qp, qs,         & ! anelastic coefficients
    yy,             & ! hourglass constant
    s1, s2,fs1,     &  ! temporary storage
    mco,            & 
    phi,            &
    epm,            &
    plb,            & !plasticity dilation
    plh,            & !plasticity hardening moduli
    mur,            & ! shear moduli (PL)
    bk,             & ! bulk moduli (PL)
    lambda,gammap,  & ! DP2
    r1, r2, r3,     &
    r4, r5

real, allocatable, target, dimension(:,:,:,:) :: &
    xx,             & ! node locations
    vv,             & ! velocity
    uu,             & ! displacement
    w1, w2,         &
    dep1, dep2,     &
    ep1, ep2,       &
    z1, z2,         &
    si1, si2         


! Fault surface fields
real, allocatable, target, dimension(:,:,:) :: &
    co,             & ! cohesion
    area,           & ! fault element area
    rhypo,          & ! radius to hypocenter
    lamf, muf,      & ! moduli at the fault nodes
    sl,             & ! slip path length
    psv,            & ! peak slip velocity
    trup,           & ! rupture time
    tarr,           & ! arrest time
    tn, ts,         &
    tnpc, tnold,    &
    mus, mud,       &
    dc,             &
    af, bf, v0, f0, &
    ll, fw, vw,     &
    psi,            &
    lpc,            &
    ath, ahy, rhoc, &
    tplam, tpw,     &
    tini, pini,     &
    temp,porep,     &
    svtrl, svold,   &
    sv0,            &
    f1, f2, f3,     &
    f4, f5,         &
    fun, dfun, delf    

real, allocatable, target, dimension(:,:,:,:) :: &
    nhat,           & ! fault surface normals
    t0, ts0,        & 
    tp,             &
    t1, t2, t3       
end module

