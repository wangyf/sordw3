! Collect statistics
module m_stats
real :: &
    amax, vmax, umax, wmax, &
    samax, svmax, sumax, slmax, &
    tsmax, tnmin, tnmax, tarrmax, &
    efric, efrac, estrain, moment, strdropint,&
    eradiat,slipint

contains

! Write statistics
subroutine stats
use m_globals
use m_collective
use m_util
logical, save :: init = .true., dofault = .false.
integer, save :: fh(20), j = 0
integer :: m, o, i
real :: rr
real, save, allocatable, dimension(:,:) :: &
    vstats, fstats, estats, gvstats, gfstats, gestats

! Start timer
if ( verb ) write( 0, * ) 'Statistics'

! Allocate buffers
if ( init ) then
    init = .false.
    if ( faultnormal /= 0 ) then
        i = abs( faultnormal )
        if ( ip3(i) == ip3root(i) ) dofault = .true.
    end if
    allocate( vstats(4,itio), fstats(8,itio), estats(7,itio), &
        gvstats(4,itio), gfstats(8,itio), gestats(7,itio) )
    vstats = 0.0
    fstats = 0.0
    estats = 0.0
    gvstats = 0.0
    gfstats = 0.0
    gestats = 0.0
    fh = -1
    if ( mpout /= 0 ) fh = file_null
end if

! Buffer stats
if (debug ==-1 .and.it==1) then
    write( str, "( a,i6.6,a )" ) 'debug/max_', ipid, '.py'
    open( 666, file=str, status='replace' )
end if

if ( modulo( it, itstats ) == 0 ) then
    j = j + 1
    vstats(1,j) = amax
    vstats(2,j) = vmax
    vstats(3,j) = umax
    vstats(4,j) = wmax

    
    if (debug == -1) then
        write( 666, "(I06,ES20.10,ES20.10,ES20.10,ES20.10)" ) it,amax,vmax,umax,wmax
    end if
    

    rr = maxval( vstats )
    if ( rr /= rr .or. rr > huge( rr ) ) then
        write(0,*) ipid
        stop 'NaN/Inf!'
    end if
    if ( dofault ) then
        fstats(1,j) = samax
        fstats(2,j) = svmax
        fstats(3,j) = sumax
        fstats(4,j) = slmax
        fstats(5,j) = tsmax
        fstats(6,j) = -tnmin
        fstats(7,j) = tnmax
        fstats(8,j) = tarrmax
        estats(1,j) = efric
        estats(2,j) = efrac
        estats(3,j) = estrain
        estats(4,j) = moment
        estats(5,j) = eradiat
        estats(6,j) = strdropint
        estats(7,j) = slipint
    end if
end if
if (debug == -1 .and. it == nt) close(666)

! Write stats
if ( j > 0 .and. ( modulo( it, itio ) == 0 .or. it == nt ) ) then
    rr = timer( 2 )
    call rreduce2( gvstats, vstats, 'max', ip3root )
    if ( dofault ) then
        call rreduce2( gfstats, fstats, 'max', ip2root )
        call rreduce2( gestats, estats, 'sum', ip2root )
    end if
    if (sync) call barrier
    mptimer = mptimer + timer( 2 )
    if ( master ) then
        m = nt / itstats
        o = it / itstats - j
        gvstats = sqrt( gvstats )
        call rio1( fh(1), gvstats(1,:j), 'w', 'stats/amax', m, o, mpout, verb )
        call rio1( fh(2), gvstats(2,:j), 'w', 'stats/vmax', m, o, mpout, verb )
        call rio1( fh(3), gvstats(3,:j), 'w', 'stats/umax', m, o, mpout, verb )
        call rio1( fh(4), gvstats(4,:j), 'w', 'stats/wmax', m, o, mpout, verb )
        if ( dofault ) then
            gfstats(6,:j) = -gfstats(6,:j)
            call rio1( fh(5),  gfstats(1,:j), 'w', 'stats/samax',   m, o, mpout, verb )
            call rio1( fh(6),  gfstats(2,:j), 'w', 'stats/svmax',   m, o, mpout, verb )
            call rio1( fh(7),  gfstats(3,:j), 'w', 'stats/sumax',   m, o, mpout, verb )
            call rio1( fh(8),  gfstats(4,:j), 'w', 'stats/slmax',   m, o, mpout, verb )
            call rio1( fh(9),  gfstats(5,:j), 'w', 'stats/tsmax',   m, o, mpout, verb )
            call rio1( fh(10), gfstats(6,:j), 'w', 'stats/tnmin',   m, o, mpout, verb )
            call rio1( fh(11), gfstats(7,:j), 'w', 'stats/tnmax',   m, o, mpout, verb )
            call rio1( fh(12), gfstats(8,:j), 'w', 'stats/tarrmax', m, o, mpout, verb )
            call rio1( fh(13), gestats(1,:j), 'w', 'stats/efric',   m, o, mpout, verb )
            call rio1( fh(14), gestats(2,:j), 'w', 'stats/efrac',   m, o, mpout, verb )
            call rio1( fh(15), gestats(3,:j), 'w', 'stats/estrain', m, o, mpout, verb )
            call rio1( fh(16), gestats(4,:j), 'w', 'stats/moment',  m, o, mpout, verb )
            call rio1( fh(17), gestats(5,:j), 'w', 'stats/eradiat',  m, o, mpout, verb )
            call rio1( fh(18), gestats(6,:j), 'w', 'stats/strdropint',  m, o, mpout, verb )  
            call rio1( fh(19), gestats(7,:j), 'w', 'stats/slipint',  m, o, mpout, verb )          
            do i = 1, j
                if ( gestats(4,i) > 0.0 ) then
                    gestats(4,i) = ( log10( gestats(4,i) ) - 9.05 ) / 1.5
                else
                    gestats(4,i) = -999
                end if
            end do
            call rio1( fh(20), gestats(4,:j), 'w', 'stats/mw',      m, o, mpout, verb )
        end if
    end if
    j = 0
    if (sync) call barrier
    iotimer = iotimer + timer( 2 )
end if

if ( it == nt .and. dofault) then
        estats(1,1) = efric
        estats(2,1) = efrac
        estats(3,1) = estrain
        estats(4,1) = moment
        estats(5,1) = eradiat
        estats(6,1) = strdropint
        estats(7,1) = slipint
        call rreduce2( gestats, estats, 'sum', ip2root )
    if (master) then
      if(debug > 1)  then 
      write(0,'(A,ES20.10,A)') 'Strain Energy is ',gestats(3,1),' Nm'
      write(0,'(A,ES20.10,A)') 'Frictional+Fracture Energy is ',gestats(1,1),' Nm'
      write(0,'(A,ES20.10,A)') 'Fracture Energy is ',gestats(2,1),' Nm'
      write(0,'(A,ES20.10,A)') 'Radiation Energy is ',gestats(5,1),' Nm'
      write(0,'(A,ES20.10,A)') 'Moment is ',gestats(4,1), ' Nm'
      write(0,'(A,F10.6,A)') 'Moment magnitude is ',( log10( gestats(4,1) ) - 9.05 ) / 1.5
      write(0,'(A,F10.6,A)') 'Energy stress drop is', gestats(6,1)/gestats(7,1)/1e6,' MPa'
      end if

      open( 1, file='stats/energetics.py', status='replace' )
      write(1,'(A)') 'Energetics Statistics'
      write(1,'(A)') 
      write(1,'(A,ES20.10,A)') 'Strain Energy is ',gestats(3,1),' Nm'
      write(1,'(A,ES20.10,A)') 'Frictional+Fracture Energy is ',gestats(1,1),' Nm'
      write(1,'(A,ES20.10,A)') 'Fracture Energy is ',gestats(2,1),' Nm'
      write(1,'(A,ES20.10,A)') 'Radiation Energy is ',gestats(5,1),' Nm'
      write(1,'(A,ES20.10,A)') 'Moment is ',gestats(4,1), ' Nm'
      write(1,'(A,F10.6,A)') 'Moment magnitude is ',( log10( gestats(4,1) ) - 9.05 ) / 1.5
      write(1,'(A,F10.6,A)') 'Energy stress drop is', gestats(6,1)/gestats(7,1)/1e6,' MPa'
      close(1)
    end if
end if


end subroutine

end module

