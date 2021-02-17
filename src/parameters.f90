! Read model parameters
module m_parameters
implicit none
contains

subroutine read_parameters
use m_globals
use m_fieldio
integer :: ios, i
character(12) :: key
character(1) :: op
character(256) :: line

! initiate node and cell list
call ini_nodecelllist

! I/O pointers
allocate( io0 )
io => io0
io%next => io0
io%field = 'head'

open( 1, file='parameters.py', status='old' )

doline: do

! Read line
read( 1, '(a)', iostat=ios ) line
if ( ios /= 0 ) exit doline

! Strip comments and punctuation
str = line
i = scan( str, '#' )
if ( i > 0 ) str(i:) = ' '
do
    i = scan( str, "()[]{}'," )
    if ( i == 0 ) exit
    str(i:i) = ' '
end do

! Read key val pair
if ( str == '' ) cycle doline
read( str, *, iostat=ios ) key

! Select input key
select case( key )
case( 'fieldio', '' )
case( 'nn' );           read( str, *, iostat=ios ) key, op, nn
case( 'nt' );           read( str, *, iostat=ios ) key, op, nt
case( 'dx' );           read( str, *, iostat=ios ) key, op, dx
case( 'dt' );           read( str, *, iostat=ios ) key, op, dt
case( 'tm0' );          read( str, *, iostat=ios ) key, op, tm0
case( 'affine' );       read( str, *, iostat=ios ) key, op, affine
case( 'n1expand' );     read( str, *, iostat=ios ) key, op, n1expand
case( 'n2expand' );     read( str, *, iostat=ios ) key, op, n2expand
case( 'rexpand' );      read( str, *, iostat=ios ) key, op, rexpand
case( 'gridnoise' );    read( str, *, iostat=ios ) key, op, gridnoise
case( 'oplevel' );      read( str, *, iostat=ios ) key, op, oplevel
case( 'rho1' );         read( str, *, iostat=ios ) key, op, rho1
case( 'rho2' );         read( str, *, iostat=ios ) key, op, rho2
case( 'vp1' );          read( str, *, iostat=ios ) key, op, vp1
case( 'vp2' );          read( str, *, iostat=ios ) key, op, vp2
case( 'vs1' );          read( str, *, iostat=ios ) key, op, vs1
case( 'vs2' );          read( str, *, iostat=ios ) key, op, vs2
case( 'gam1' );         read( str, *, iostat=ios ) key, op, gam1
case( 'gam2' );         read( str, *, iostat=ios ) key, op, gam2
case( 'vdamp' );        read( str, *, iostat=ios ) key, op, vdamp
case( 'hourglass' );    read( str, *, iostat=ios ) key, op, hourglass
case( 'bc1' );          read( str, *, iostat=ios ) key, op, bc1
case( 'bc2' );          read( str, *, iostat=ios ) key, op, bc2
case( 'npml' );         read( str, *, iostat=ios ) key, op, npml
case( 'i1pml' );        read( str, *, iostat=ios ) key, op, i1pml
case( 'i2pml' );        read( str, *, iostat=ios ) key, op, i2pml
case( 'ppml' );         read( str, *, iostat=ios ) key, op, ppml
case( 'vpml' );         read( str, *, iostat=ios ) key, op, vpml
case( 'ihypo' );        read( str, *, iostat=ios ) key, op, ihypo
case( 'source' );       read( str, *, iostat=ios ) key, op, source
case( 'timefunction' ); read( str, *, iostat=ios ) key, op, timefunction
case( 'period' );       read( str, *, iostat=ios ) key, op, period
case( 'strike' );       read( str, *, iostat=ios ) key, op, strike
case( 'dip' );          read( str, *, iostat=ios ) key, op, dip
case( 'rake' );         read( str, *, iostat=ios ) key, op, rake
case( 'm0' );           read( str, *, iostat=ios ) key, op, m0
case( 'source1' );      read( str, *, iostat=ios ) key, op, source1
case( 'source2' );      read( str, *, iostat=ios ) key, op, source2
case( 'nsource' );      read( str, *, iostat=ios ) key, op, nsource
case( 'ivols' );        read( str, *, iostat=ios ) key, op, ivols
case( 'eplasticity' );  read( str, *, iostat=ios ) key, op, eplasticity
case( 'plmodel' );      read( str, *, iostat=ios ) key, op, plmodel
case( 'pleta' );        read( str, *, iostat=ios ) key, op, pleta    
case( 'tv' );           read( str, *, iostat=ios ) key, op, tv
case( 'tpnz' );         read( str, *, iostat=ios ) key, op, tpnz
case( 'tpsubdt' );      read( str, *, iostat=ios ) key, op, tpsubdt
case( 'tp_method');     read( str, *, iostat=ios ) key, op, tp_method
case( 'tp_vw');         read( str, *, iostat=ios ) key, op, tp_vw
case( 'friction' );     read( str, *, iostat=ios ) key, op, friction
case( 'pcdep' );        read( str, *, iostat=ios ) key, op, pcdep
case( 'faultnormal' );  read( str, *, iostat=ios ) key, op, faultnormal
case( 'slipvector' );   read( str, *, iostat=ios ) key, op, slipvector
case( 'faultopening' ); read( str, *, iostat=ios ) key, op, faultopening
case( 'vrup' );         read( str, *, iostat=ios ) key, op, vrup
case( 'rcrit' );        read( str, *, iostat=ios ) key, op, rcrit
case( 'trelax' );       read( str, *, iostat=ios ) key, op, trelax
case( 'rrelax' );       read( str, *, iostat=ios ) key, op, rrelax
case( 'tslope' );       read( str, *, iostat=ios ) key, op, tslope
case( 'rnucl' );        read( str, *, iostat=ios ) key, op, rnucl
case( 'tmnucl' );       read( str, *, iostat=ios ) key, op, tmnucl
case( 'delts' );        read( str, *, iostat=ios ) key, op, delts
case( 'svtol' );        read( str, *, iostat=ios ) key, op, svtol
case( 'np3' );          read( str, *, iostat=ios ) key, op, np3
case( 'itstats' );      read( str, *, iostat=ios ) key, op, itstats
case( 'itio' );         read( str, *, iostat=ios ) key, op, itio
case( 'itcheck' );      read( str, *, iostat=ios ) key, op, itcheck
case( 'itstop' );       read( str, *, iostat=ios ) key, op, itstop
case( 'debug' );        read( str, *, iostat=ios ) key, op, debug
case( 'mpin' );         read( str, *, iostat=ios ) key, op, mpin
case( 'mpout' );        read( str, *, iostat=ios ) key, op, mpout
case default
    select case( key(1:1) )
    case( '=', '+' )
        call pappend
        io%ib = -1
        !XXXread( str, *, iostat=ios ) io%mode, io%nc
        read( str, *, iostat=ios ) io%mode, io%nc, io%tfunc, &
            io%period, io%x1, io%x2, io%nb, io%ii, io%filename, &
            io%val, io%field
        if (any(nodelist==io%field)) then
            io%nodecell='nod'
        else if (any(celllist==io%field)) then
            io%nodecell='cel'
        else
            write(0,*) 'Unknown field for node/cell ',io%field
            stop
        end if
    case default; ios = 1
    end select
end select

! Error check
if ( ios /= 0 ) then
    if ( master ) write( 0, * ) 'bad input: ', trim( line )
    stop
end if

end do doline

close( 1 )

end subroutine


subroutine ini_nodecelllist
use m_globals
nodelist(1)     = 'x1'
nodelist(2)     = 'x2'
nodelist(3)     = 'x3'
nodelist(4)     = 'f1'
nodelist(5)     = 'f2'
nodelist(6)     = 'f3'
nodelist(7)     = 'a1'
nodelist(8)     = 'a2'
nodelist(9)     = 'a3'
nodelist(10)    = 'am2'
nodelist(11)    = 'v1'
nodelist(12)    = 'v2'
nodelist(13)    = 'v3'
nodelist(14)    = 'vm2'
nodelist(15)    = 'u1'
nodelist(16)    = 'u2'
nodelist(17)    = 'u3'
nodelist(18)    = 'um2'
nodelist(19)    = 'co'
nodelist(20)    = 'mus'
nodelist(21)    = 'mud'
nodelist(22)    = 'dc'
nodelist(23)    = 'af'
nodelist(24)    = 'bf'
nodelist(25)    = 'v0'
nodelist(26)    = 'f0'
nodelist(27)    = 'll'
nodelist(28)    = 'fw'
nodelist(29)    = 'vw'
nodelist(30)    = 'psi'
nodelist(31)    = 'ath'
nodelist(32)    = 'ahy'
nodelist(33)    = 'rhoc'
nodelist(34)    = 'tplam'
nodelist(35)    = 'tpw'
nodelist(36)    = 'tini'
nodelist(37)    = 'pini'
nodelist(38)    = 'tempw'
nodelist(39)    = 'tau_c'
nodelist(40)    = 'aspD'
nodelist(41)    = 'lpc'
nodelist(42)    = 'ts'
nodelist(43)    = 'td'
nodelist(44)    = 'tn'
nodelist(45)    = 'area'
nodelist(46)    = 'nhat1'
nodelist(47)    = 'nhat2'
nodelist(48)    = 'nhat3'
nodelist(49)    = 'temp'
nodelist(50)    = 'porep'
nodelist(51)    = 'vwv'
nodelist(52)    = 't1'
nodelist(53)    = 't2'
nodelist(54)    = 't3'
nodelist(55)    = 'ts1'
nodelist(56)    = 'ts2'
nodelist(57)    = 'ts3'
nodelist(58)    = 'tsm'
nodelist(59)    = 'tnm'
nodelist(60)    = 'tneff'
nodelist(61)    = 'fr'
nodelist(62)    = 'sa1'
nodelist(63)    = 'sa2'
nodelist(64)    = 'sa3'
nodelist(65)    = 'sam'
nodelist(66)    = 'sv1'
nodelist(67)    = 'sv2'
nodelist(68)    = 'sv3'
nodelist(69)    = 'svm'
nodelist(70)    = 'psv'
nodelist(71)    = 'su1'
nodelist(72)    = 'su2'
nodelist(73)    = 'su3'
nodelist(74)    = 'sum'
nodelist(75)    = 'sl'
nodelist(76)    = 'erf'
nodelist(77)    = 'ere'
nodelist(78)    = 'eft'
nodelist(79)    = 'trup'
nodelist(80)    = 'tarr'


celllist(1)     = 'c1'
celllist(2)     = 'c2'
celllist(3)     = 'c3'
celllist(4)     = 'vc'
celllist(5)     = 'a11'
celllist(6)     = 'a22'
celllist(7)     = 'a33'
celllist(8)     = 'a23'
celllist(9)     = 'a31'
celllist(10)    = 'a12'
celllist(11)    = 'rho'
celllist(12)    = 'vp'
celllist(13)    = 'vs'
celllist(14)    = 'gam'
celllist(15)    = 'mco'
celllist(16)    = 'phi'
celllist(17)    = 'plb'
celllist(18)    = 'plh'
celllist(19)    = 'mu'
celllist(20)    = 'lam'
celllist(21)    = 'yy'
celllist(22)    = 'bk'
celllist(23)    = 'plcls'
celllist(24)    = 'epm'
celllist(25)    = 'gammap'
celllist(26)    = 's11'
celllist(27)    = 's22'
celllist(28)    = 's33'
celllist(29)    = 's23'
celllist(30)    = 's31'
celllist(31)    = 's12'
celllist(32)    = 'e11'
celllist(33)    = 'e22'
celllist(34)    = 'e33'
celllist(35)    = 'e23'
celllist(36)    = 'e31'
celllist(37)    = 'e12'
celllist(38)    = 'w11'
celllist(39)    = 'w22'
celllist(40)    = 'w33'
celllist(41)    = 'w23'
celllist(42)    = 'w31'
celllist(43)    = 'w12'
celllist(44)    = 'wm2'
celllist(45)    = 'mr11'
celllist(46)    = 'mr22'
celllist(47)    = 'mr33'
celllist(48)    = 'mr23'
celllist(49)    = 'mr31'
celllist(50)    = 'mr12'
end subroutine

end module

