#!/usr/bin/env python
"""
Source utilities
"""
import os, sys, urllib, gzip, sordw3
import numpy as np

def scsn_mts( eventid ):
    """
    Retrieve Southern California Seismic Network Moment Tensor Solutions.
    """
    url = 'http://www.data.scec.org/MomentTensor/solutions/web_%s/ci%s_MT.html' % (eventid, eventid)
    url = 'http://www.data.scec.org/MomentTensor/solutions/%s/' % eventid
    url = 'http://www.data.scec.org/MomentTensor/showMT.php?evid=%s' % eventid
    text = urllib.urlopen( url )
    event = {}
    clvd = [[[], [], []], [[], [], []]]
    dc   = [[[], [], []], [[], [], []]]
    for line in text.readlines():
        line = line.strip()
        if ':' in line and line[0] != ' ':
            f = line.split( ':', 1 )
            k = f[0].strip()
            if k in ('Origin Time', 'Stations', 'Quality Factor'):
                event[k] = f[1].strip()
            elif k in ('Event ID', 'Number of Stations used'):
                event[k] = int( f[1] )
            elif k in ('Magnitude', 'Depth (km)', 'Latitude', 'Longitude', 'Moment Magnitude'):
                event[k] = float( f[1] )
            elif k == 'Best Fitting Double Couple and CLVD Solution':
                tensor = clvd
            elif k == 'Best Fitting Double Couple Solution':
                tensor = dc
        elif line:
            f = line.split()
            if f[0] == 'Mxx':
                tensor[0][0] = float( f[1] )
            elif f[0] == 'Myy':
                tensor[0][1] = float( f[1] )
            elif f[0] == 'Mzz':
                tensor[0][2] = float( f[1] )
            elif f[0] == 'Myz':
                tensor[1][0] = float( f[1] )
            elif f[0] == 'Mxz':
                tensor[1][1] = float( f[1] )
            elif f[0] == 'Mxy':
                tensor[1][2] = float( f[1] )
    event['double-couple-clvd'] = np.array( clvd )
    event['double-couple'] = np.array( dc )
    return event

def magarea( A ):
    """
    Various earthquake magnitude area relations.
    """
    if type( A ) in (float, int):
        Mw = 3.98 + np.log10( A )
        if A > 537.0:
            Mw = 3.08 + 4.0 / 3.0 * np.log10( A )
    else:
        A = np.array( A ) 
        i = A > 537.0
        Mw = 3.98 + np.log10( A )
        Mw[i] = 3.08 + 4.0 / 3.0 * np.log10( A )
    Mw = dict(
        Hanks2008 = Mw,
        EllsworthB2003 = 4.2 + np.log10( A ),
        Somerville2006 = 3.87 + 1.05 * np.log10( A ),
        Wells1994 = 3.98 + 1.02 * np.log10( A ),
    )
    return Mw

def src_write( history, nt, dt, t0, xi, w1, w2=None, path='' ):
    """
    Write SORD input for moment or potency source.
    """
    path = os.path.join( os.path.expanduser( path ), 'src_' )
    np.array( history, 'f' ).tofile( path + 'history' )
    np.array( nt,      'f' ).tofile( path + 'nt'  )
    np.array( dt,      'f' ).tofile( path + 'dt'  )
    np.array( t0,      'f' ).tofile( path + 't0'  )
    np.array( xi[0],   'f' ).tofile( path + 'xi1' )
    np.array( xi[1],   'f' ).tofile( path + 'xi2' )
    np.array( xi[2],   'f' ).tofile( path + 'xi3' )
    if not w2:
        np.array( w1[0], 'f' ).tofile( path + 'w11' )
        np.array( w1[1], 'f' ).tofile( path + 'w12' )
        np.array( w1[2], 'f' ).tofile( path + 'w13' )
    else:
        np.array( w1[0], 'f' ).tofile( path + 'w11' )
        np.array( w1[1], 'f' ).tofile( path + 'w22' )
        np.array( w1[2], 'f' ).tofile( path + 'w33' )
        np.array( w2[0], 'f' ).tofile( path + 'w23' )
        np.array( w2[1], 'f' ).tofile( path + 'w31' )
        np.array( w2[2], 'f' ).tofile( path + 'w12' )
    return

def fsp_read(filename, path=None, mks=True):
    """
    Reader for fsp data format in SRCMOD database
    http://equake-rc.info/SRCMOD/fileformats/fsp/
    """
    fd = filename
    if not isinstance(fd,io.IOBase):
        fd = open( os.path.expanduser( fd ) ) 

    meta={}

    line = fd.readline()
    while line:
        line = fd.readline()
        k = line.split()
        if k[1] == 'Event':
            meta['Event']=line[line.index(':')+2:]




def srf_read( filename, path=None, mks=True ):
    """
    Reader for Graves Standard Rupture Format (SRF).

    If path is specified, write binary files. Otherwise, just return header.

    Utilities for Graves Standard Rupture Format (SRF).

    Standard Rupture Format (SRF) is a file format defined by 
    Robert Graves (USGS) intended to facilitate exchange of rupture 
    descriptions between scientists.
    SRF is documented at http://hypocenter.usc.edu/research/SRF/srf-v2.0_rev1.pdf

    Modified by Yongfei Wang, Jan 2020
    """
    fd = filename
    if not isinstance(fd,io.IOBase):
        if fd.split('.')[-1] == 'gz':
            fd = gzip.open( os.path.expanduser( fd ), 'r' )
        else:
            fd = open( os.path.expanduser( fd ) )

    # Header block
    meta = {}
    meta['version'] = fd.readline().split()[0]
    k = fd.readline().split()
    if k[0] == 'PLANE':
        meta['nsegments'] = int( k[1] )
        k = fd.readline().split() + fd.readline().split()
        if len( k ) != 11:
            sys.exit( 'error reading %s' % filename )
        meta['nsource2']   = int(   k[2] ), int(   k[3]  )  #subfault numbers 
        meta['topcenter']  = float( k[0] ), float( k[1]  ), float( k[8] )
        meta['plane']      = float( k[6] ), float( k[7]  )  #plane strike and dip
        meta['length']     = float( k[4] ), float( k[5]  )  #total plane size
        meta['hypocenter'] = float( k[9] ), float( k[10] )  #hypocenter location
        if mks:
            meta['length']     = tuple( 1000 * x for x in meta['length'] )
            meta['hypocenter'] = tuple( 1000 * x for x in meta['hypocenter'] )
        k = fd.readline().split()
    if k[0] != 'POINTS':
        sys.exit( 'error reading %s' % filename )
    meta['nsource'] = int( k[1] )
    if not path:
        return meta

    # Data block
    path = os.path.expanduser( path ) + os.sep
    if path not in '.' and not os.path.isdir( path ):
        os.makedirs( path )
    n = meta['nsource']
    lon   = np.empty( n )
    lat   = np.empty( n )
    dep   = np.empty( n )
    stk   = np.empty( n )
    dip   = np.empty( n )
    vs    = np.empty( n )
    den   = np.empty( n )
    rake  = np.empty( n )
    area  = np.empty( n )
    t0    = np.empty( n )
    dt    = np.empty( n )
    slip1 = np.empty( n )
    slip2 = np.empty( n )
    slip3 = np.empty( n )
    nt1   = np.empty( n, 'i' )
    nt2   = np.empty( n, 'i' )
    nt3   = np.empty( n, 'i' )
    fd1 = open( path + 'sv1', 'wb' )
    fd2 = open( path + 'sv2', 'wb' )
    fd3 = open( path + 'sv3', 'wb' )
    for i in range( n ):
        k = fd.readline().split() + fd.readline().split()
        if len( k ) != 17:
            sys.exit( 'error reading %s %s' % ( filename, i ) )
        lon[i]   = float( k[0] )
        lat[i]   = float( k[1] )
        dep[i]   = float( k[2] )
        stk[i]   = float( k[3] )
        dip[i]   = float( k[4] )
        vs[i]    = float( k[8] )
        den[i]   = float( k[9] )
        rake[i]  = float( k[10] )
        area[i]  = float( k[5] )
        t0[i]    = float( k[6] )
        dt[i]    = float( k[7] )
        slip1[i] = float( k[11] )
        slip2[i] = float( k[13] )
        slip3[i] = float( k[15] )
        nt1[i]   = int( k[12] )
        nt2[i]   = int( k[14] )
        nt3[i]   = int( k[16] )
        sv = []
        n = np.cumsum([ nt1[i], nt2[i], nt3[i] ])
        while len( sv ) < n[-1]:
            sv += fd.readline().split()
        if len( sv ) != n[-1]:
            sys.exit( 'error reading %s %s' % ( filename, i ) )
        sv1 = np.array( [ float(f) for f in sv[:n[0]]     ] )
        sv2 = np.array( [ float(f) for f in sv[n[0]:n[1]] ] )
        sv3 = np.array( [ float(f) for f in sv[n[1]:]     ] )
        if mks:
            sv1 = 0.01 * sv1
            sv2 = 0.01 * sv2
            sv3 = 0.01 * sv3
        np.array( sv1, 'f' ).tofile( fd1 )
        np.array( sv2, 'f' ).tofile( fd2 )
        np.array( sv3, 'f' ).tofile( fd3 )
    fd1.close()
    fd2.close()
    fd3.close()
    if mks:
        dep = 1000.0 * dep
        area = 0.0001 * area
        slip1 = 0.01 * slip1
        slip2 = 0.01 * slip2
        slip3 = 0.01 * slip3
    np.array( nt1,   'i' ).tofile( path + 'nt1'   )
    np.array( nt2,   'i' ).tofile( path + 'nt2'   )
    np.array( nt3,   'i' ).tofile( path + 'nt3'   )
    np.array( dt,    'f' ).tofile( path + 'dt'    )
    np.array( t0,    'f' ).tofile( path + 't0'    )
    np.array( area,  'f' ).tofile( path + 'area'  )
    np.array( lon,   'f' ).tofile( path + 'lon'   )
    np.array( lat,   'f' ).tofile( path + 'lat'   )
    np.array( dep,   'f' ).tofile( path + 'dep'   )
    np.array( stk,   'f' ).tofile( path + 'stk'   )
    np.array( dip,   'f' ).tofile( path + 'dip'   )
    np.array( rake,  'f' ).tofile( path + 'rake'  )
    np.array( vs,    'f' ).tofile( path + 'vs'    )
    np.array( den,   'f' ).tofile( path + 'den'   )
    np.array( slip1, 'f' ).tofile( path + 'slip1' )
    np.array( slip2, 'f' ).tofile( path + 'slip2' )
    np.array( slip3, 'f' ).tofile( path + 'slip3' )

    # Write meta data total area, average slip and scalar potency
    meta['area'] = area.sum()
    meta['potency'] = np.sqrt(
        ( area * slip1 ).sum() ** 2 +
        ( area * slip2 ).sum() ** 2 +
        ( area * slip3 ).sum() ** 2 )
    meta['slip'] = meta['potency'] / meta['area']
    meta['dtype'] = np.dtype( 'f' ).str
    sordw3.util.save( path + 'meta.py', meta )
    return meta

def srf2potency( path, proj, dx=None ):
    """
    Convert SRF to potency tensor source and write SORD input files.
    """
    import coord

    # Read meta data
    path = os.path.expanduser( path ) + os.sep
    meta = {}
    # exec open( path + 'meta.py' ) in meta     # python 2.7
    exec(open(path + 'meta.py').read() in meta) # python 3
    dtype = meta['dtype'] #'<f4'

    #if dx is not assigned
    if dx == None:
        dx = np.array([meta['length'][0]/meta['nsource2'][0],
                       meta['length'][1]/meta['nsource2'][1],
                       meta['length'][0]/meta['nsource2'][0]])

    # Read data
    nt1  = np.fromfile( path + 'nt1',  'i' )
    nt2  = np.fromfile( path + 'nt2',  'i' )
    nt3  = np.fromfile( path + 'nt3',  'i' )
    dt   = np.fromfile( path + 'dt',   dtype )
    t0   = np.fromfile( path + 't0',   dtype )
    x    = np.fromfile( path + 'lon',  dtype )
    y    = np.fromfile( path + 'lat',  dtype )
    z    = np.fromfile( path + 'dep',  dtype )
    stk  = np.fromfile( path + 'stk',  dtype )
    dip  = np.fromfile( path + 'dip',  dtype )
    rake = np.fromfile( path + 'rake', dtype )
    area = np.fromfile( path + 'area', dtype )

    # Time
    nt = np.array( [nt1, nt2, nt3] )
    ii = nt > 0
    nsource = nt[ii].size
    nt[ii].tofile( path + 'src_nt' )
    dt[None].repeat(3,0)[ii].tofile( path + 'src_dt' )
    t0[None].repeat(3,0)[ii].tofile( path + 'src_t0' )

    # Time history
    fd1 = open( path + 'sv1' )
    fd2 = open( path + 'sv2' )
    fd3 = open( path + 'sv3' )
    fd  = open( path + 'src_history', 'wb' ) #slip velocity time series
    for i in range( dt.size ):
        np.fromfile(fd1, dtype, nt1[i]).tofile( fd )
        #np.cumsum( dt[i] * np.fromfile(fd1, dtype, nt1[i]) ).tofile( fd )
    for i in range( dt.size ):
        np.fromfile(fd2, dtype, nt2[i]).tofile( fd )
        #np.cumsum( dt[i] * np.fromfile(fd2, dtype, nt2[i]) ).tofile( fd )
    for i in range( dt.size ):
        np.fromfile(fd3, dtype, nt3[i]).tofile( fd )
        #np.cumsum( dt[i] * np.fromfile(fd3, dtype, nt3[i]) ).tofile( fd )
    fd1.close()
    fd2.close()
    fd3.close()
    fd.close()

    # Coordinates
    rot = coord.rotation( x, y, proj )[1]
    x, y = proj( x, y )
    x = np.array( x / dx[0] + 1.0, dtype ) #can be fraction
    y = np.array( y / dx[1] + 1.0, dtype ) #can be fraction
    z = np.array( z / dx[2] + 1.0, dtype ) #can be fraction
    x[None].repeat(3,0)[ii].tofile( path + 'src_xi1' )
    y[None].repeat(3,0)[ii].tofile( path + 'src_xi2' )
    z[None].repeat(3,0)[ii].tofile( path + 'src_xi3' )

    # Strike, dip, and normal vectors
    R = area * coord.slipvectors( stk + rot, dip, rake )

    # Tensor components (problematic?)
    stk, dip, nrm = np.array( coord.source_potency_tensors( R ), dtype )
    #assume slip only along rake direction (i.e., only slip1 exists)
    stk[0,ii].tofile( path + 'src_w23')
    stk[1,ii].tofile( path + 'src_w31')
    stk[2,ii].tofile( path + 'src_w12')
    stk[3,ii].tofile( path + 'src_w11')
    stk[4,ii].tofile( path + 'src_w22')
    stk[5,ii].tofile( path + 'src_w33')

    # w = np.zeros_like( stk )
    # w[0] = stk[0]; w[1] = dip[0]; w[ii].tofile( path + 'src_w23' )
    # w[0] = stk[1]; w[1] = dip[1]; w[ii].tofile( path + 'src_w31' )
    # w[0] = stk[2]; w[1] = dip[2]; w[ii].tofile( path + 'src_w12' )
    # w = np.zeros_like( nrm )
    # w[2] = nrm[0]; w[ii].tofile( path + 'src_w11' )
    # w[2] = nrm[1]; w[ii].tofile( path + 'src_w22' )
    # w[2] = nrm[2]; w[ii].tofile( path + 'src_w33' )

    return nsource


def srf2momrate( path, proj, dx, dt, nt ):
    """
    Convert SRF to moment rate and write Olsen AWM input file.
    """
    import coord

    # Read meta data
    path = os.path.expanduser( path ) + os.sep
    meta = {}
    # exec open( path + 'meta.py' ) in meta     # python 2.7
    exec(open(path + 'meta.py').read() in meta) # python 3
    dtype = meta['dtype']

    # Read data
    nt1  = np.fromfile( path + 'nt1',  'i' )
    nt2  = np.fromfile( path + 'nt2',  'i' )
    nt3  = np.fromfile( path + 'nt3',  'i' )
    dt0  = np.fromfile( path + 'dt',   dtype )
    t0   = np.fromfile( path + 't0',   dtype )
    x    = np.fromfile( path + 'lon',  dtype )
    y    = np.fromfile( path + 'lat',  dtype )
    z    = np.fromfile( path + 'dep',  dtype )
    stk  = np.fromfile( path + 'stk',  dtype )
    dip  = np.fromfile( path + 'dip',  dtype )
    rake = np.fromfile( path + 'rake', dtype )
    area = np.fromfile( path + 'area', dtype )
    mu   = np.fromfile( path + 'mu',   dtype )
    lam  = np.fromfile( path + 'lam',  dtype )

    # Coordinates
    rot = coord.rotation( x, y, proj )[1]
    x, y = proj( x, y )
    jj = int( x / dx[0] + 1.5 )
    kk = int( y / dx[1] + 1.5 )
    ll = int( z / dx[2] + 1.5 )

    # Moment tensor components
    R = area * coord.slipvectors( stk + rot, dip, rake )
    stk, dip, nrm = coord.source_tensors( R )
    stk = stk * mu
    dip = dip * mu
    nrm = nrm * lam

    # Time history
    t = dt * np.arange( nt )
    fd1 = open( path + 'sv1' )
    fd2 = open( path + 'sv2' )
    fd3 = open( path + 'sv3' )
    fd  = open( path + 'momrate', 'wb' )
    for i in range( dt.size ):
        sv1 = np.fromfile( fd1, dtype, nt1[i] )
        sv2 = np.fromfile( fd2, dtype, nt2[i] )
        sv3 = np.fromfile( fd3, dtype, nt3[i] )
        sv1 = coord.interp( t0[i], dt0[i], sv1, t )
        sv2 = coord.interp( t0[i], dt0[i], sv2, t )
        sv3 = coord.interp( t0[i], dt0[i], sv3, t )
        np.array( [jj[i], kk[i], ll[i]], 'i' ).tofile( fd )
        np.array( [
            nrm[0,i] * sv3,
            nrm[1,i] * sv3,
            nrm[2,i] * sv3,
            stk[0,i] * sv1, + dip[0,i] * sv2,
            stk[1,i] * sv1, + dip[1,i] * sv2,
            stk[2,i] * sv1, + dip[2,i] * sv2,
        ], 'f' ).tofile( fd )
    fd1.close()
    fd2.close()
    fd3.close()
    fd.close()

    return dt.size

def dsample( f, d ):
    """
    Downsample 2d array.
    """
    if not d:
        return f
    n = f.shape
    n = n[0] / d, n[1] / d
    g = np.zeros( n )
    for j in range( d ):
        for k in range( d ):
            g = g + f[j::d,k::d]
    g = g / (d * d)
    return g

def srf2coulomb( path, proj, dx, dest=None, scut=0 ):
    """
    Convert SRF to Coulomb input file.
    """
    import coord

    if dest == None:
        dest = os.path.join( path, 'coulomb-' )

    # Meta data
    path = os.path.expanduser( path ) + os.sep
    meta = {}
    # exec open( path + 'meta.py' ) in meta     # python 2.7
    exec(open(path + 'meta.py').read() in meta) # python 3
    dtype = meta['dtype']

    # Read files
    x    = np.fromfile( path + 'lon',   dtype )
    y    = np.fromfile( path + 'lat',   dtype )
    z    = np.fromfile( path + 'dep',   dtype )
    stk  = np.fromfile( path + 'stk',   dtype )
    dip  = np.fromfile( path + 'dip',   dtype )
    rake = np.fromfile( path + 'rake',  dtype )
    s1   = np.fromfile( path + 'slip1', dtype )
    s2   = np.fromfile( path + 'slip2', dtype )

    # Slip components
    s = np.sin( np.pi / 180.0 * rake )
    c = np.cos( np.pi / 180.0 * rake )
    r1 = -c * s1 + s * s2
    r2 =  s * s1 + c * s2

    # Coordinates
    rot = coord.rotation( x, y, proj )[1]
    x, y = proj( x, y )
    x *= 0.001
    y *= 0.001
    z *= 0.001
    delta = 0.0005 * meta['dx']
    dx = delta * np.sin( np.pi / 180.0 * (stk + rot) )
    dy = delta * np.cos( np.pi / 180.0 * (stk + rot) )
    dz = delta * np.sin( np.pi / 180.0 * dip )
    x1, x2 = x - dx, x + dx
    y1, y2 = y - dy, y + dy
    z1, z2 = z - dz, z + dz

    # Source file
    i = (s1**2 + s2**2) > (np.sign( scut ) * scut**2)
    c = np.array( [x1[i], y1[i], x2[i], y2[i], r1[i], r2[i], dip[i], z1[i], z2[i]] ).T
    fd = open( dest + 'source.inp', 'w' )
    fd.write( coulomb_header % meta )
    np.savetxt( fd, c, coulomb_fmt )
    fd.write( coulomb_footer )
    fd.close()

    # Receiver file
    s1.fill( 0.0 )
    c = np.array( [x1, y1, x2, y2, s1, s1, dip, z1, z2] ).T
    fd = open( dest + 'receiver.inp', 'w' )
    fd.write( coulomb_header % meta )
    np.savetxt( fd, c, coulomb_fmt )
    fd.write( coulomb_footer )
    fd.close()

    return

coulomb_fmt = '  1' + 4*' %10.4f' + ' 100' + 5*' %10.4f' + '    Fault 1'

coulomb_header = """\
header line 1
header line 2
#reg1=  0  #reg2=  0  #fixed=  %(nsource)s  sym=  1
 PR1=       0.250     PR2=       0.250   DEPTH=      12.209
  E1=     8.000e+005   E2=     8.000e+005
XSYM=       .000     YSYM=       .000
FRIC=          0.400
S1DR=         19.000 S1DP=         -0.010 S1IN=        100.000 S1GD=          0.000
S2DR=         89.990 S2DP=         89.990 S2IN=         30.000 S2GD=          0.000
S3DR=        109.000 S3DP=         -0.010 S3IN=          0.000 S3GD=          0.000

  #   X-start    Y-start     X-fin      Y-fin   Kode  rt.lat    reverse   dip angle     top      bot
xxx xxxxxxxxxx xxxxxxxxxx xxxxxxxxxx xxxxxxxxxx xxx xxxxxxxxxx xxxxxxxxxx xxxxxxxxxx xxxxxxxxxx xxxxxxxxxx
"""

coulomb_footer = """
   Grid Parameters
  1  ----------------------------  Start-x =     -100.0
  2  ----------------------------  Start-y =        0.0
  3  --------------------------   Finish-x =      500.0
  4  --------------------------   Finish-y =      400.0
  5  ------------------------  x-increment =        5.0
  6  ------------------------  y-increment =        5.0
     Size Parameters
  1  --------------------------  Plot size =        2.0
  2  --------------  Shade/Color increment =        1.0
  3  ------  Exaggeration for disp.& dist. =    10000.0
  
     Cross section default
  1  ----------------------------  Start-x =     -126.4
  2  ----------------------------  Start-y =     -124.6
  3  --------------------------   Finish-x =       40.0
  4  --------------------------   Finish-y =       40.0
  5  ------------------  Distant-increment =        1.0
  6  ----------------------------  Z-depth =       30.0
  7  ------------------------  Z-increment =        1.0
     Map info
  1  ---------------------------- min. lon =     -128.0
  2  ---------------------------- max. lon =     -123.0
  3  ---------------------------- zero lon =     -125.0
  4  ---------------------------- min. lat =       39.5
  5  ---------------------------- max. lat =       42.5
  6  ---------------------------- zero lat =       40.0
"""



class Segment:
    def __init__(self,colname,iseg,length,width,dx,dz,d2t,tlat,tlon,hyposeg,\
                      hypoX,hypoZ,nsbfs,ncol):
        self.colname = colname
        self.iseg = iseg
        self.len = length
        self.wid = width
        self.dx = dx
        self.dz = dz
        self.depth2top = d2t
        self.toplat = tlat
        self.toplon = tlon
        self.hyposeg = hyposeg
        self.hypoX = hypoX
        self.hypoZ = hypoZ
        self.nsbfs = nsbfs

        self.slip = np.empty((nsbfs,ncol))

def fsp_read(filename):
    """
    Reader for fsp data format in SRCMOD database
    http://equake-rc.info/SRCMOD/fileformats/fsp/
    """
    fd = filename
    if not isinstance(fd,io.IOBase):
        fd = open( os.path.expanduser( fd ) ) 

    meta={}

    line = fd.readline()

    nsubfault = 0
    while line:
        line = fd.readline()
        k = line.split()

        if len(k) < 3:
            continue 

        if k[0]=='%' and k[1] == 'Event':
            meta['Event']=line[line.index(':')+2:].replace('\t','').replace('\n','')
        if k[0]=='%' and k[1] == 'EventTAG:':
            meta['EvtTAG']=k[2].replace(':','') #the filename is meta['EvtTAG']+'.fsp'
        if k[0]=='%' and k[1] == 'Loc':
            meta['Lat'] = float(k[5])
            meta['Lon'] = float(k[8])
            meta['Dep'] = float(k[11]) #km
        if k[0]=='%' and k[1] == 'Size':
            meta['Len']= float(k[5]) #km
            meta['Wid']= float(k[9]) #km
            meta['Mw']= float(k[13])
            meta['M0']= float(k[16]) #Nm
        if k[0]=='%' and k[1] == 'Mech':
            meta['Strike'] = float(k[5])
            meta['Dip'] = float(k[8])   
            meta['Rake'] = float(k[11]) 
            meta['Htop'] = float(k[14]) #km
        if k[0]=='%' and k[1] == 'Rupt':
            meta['Hypx'] = float(k[5])
            meta['Hypz'] = float(k[9])
            meta['Avvr'] = float(k[17]) #km/s
        if k[0]=='%' and k[1] == 'Invs':
            if k[3] == 'Nx':
                meta['Nx'] = int(k[5])
                meta['Nz'] = int(k[8])
            if k[3] == 'Dx':
                meta['Dx'] = float(k[5])
                meta['Dz'] = float(k[9])
        if k[0]=='%' and k[1]=='SOURCE':
            line = fd.readline()
            k = line.split()
            if k[1] == 'Nsbfs':
                meta['Planes'] = 'Single'
                meta['Nsbfs'] = int(k[3])
            elif k[1]=='X,Y,Z':
                meta['Planes'] = 'Multi'
                meta['mpdata'] = []

        if 'Planes' in meta.keys():
            if meta['Planes'] == 'Single':
                if k[0]=='%' and k[1]=='LAT' and k[2]=='LON':
                    meta['colname']= k[1:]
                    meta['spdata']=np.empty((meta['Nsbfs'],len(k)-1))

                    line = fd.readline()

                    for i in range(meta['Nsbfs']):
                        line = fd.readline()
                        k = line.split()
                        meta['spdata'][i,:] = np.array([float(ik) for ik in k])


            if meta['Planes'] == 'Multi' and k[1] == 'SEGMENT':
                iseg = int(k[3].replace(':',''))
                strike = float(k[6])
                dip = float(k[10])

                line = fd.readline()
                k = line.split()
                length = float(k[3])
                width =  float(k[7])

                line = fd.readline()
                k = line.split()
                if k[1] == 'Dx':
                    dx = float(k[3])
                    dz = float(k[7])

                    line = fd.readline()
                    k = line.split()
                    z2top = float(k[6])
                else:
                    dx = meta['Dx']
                    dz = meta['Dz']
                    z2top = float(k[6])

                line = fd.readline()
                line = fd.readline()
                k = line.split()
                tlat =  float(k[3])
                tlon =  float(k[7]) 

                line = fd.readline()
                k = line.split()
                hyposeg = int(k[5])
                hypox =   float(k[10])
                hypoz =   float(k[15])

                line = fd.readline()
                k = line.split()
                nsbfs = int(k[3])

                line = fd.readline()
                line = fd.readline()
                k = line.split()
                colname = k[1:]

                seg = Segment(colname,iseg,length,width,\
                            dx,dz,z2top,tlat,tlon,hyposeg,\
                          hypox,hypoz,nsbfs,len(colname))


                line = fd.readline()

                for i in range(nsbfs):
                    line = fd.readline()
                    k = line.split()
                    seg.slip[i,:] = np.array([float(ik) for ik in k])

                meta['mpdata'] = meta['mpdata']+ [seg,] 
    return meta

def loadallfsp(path):
    database=[]
    for file in os.listdir(path):
        if file.endswith(".fsp"):
            fname = os.path.join(path, file)
            print('Loading',fname)
            database = database + [fsp_read(fname),]
    return database

def command_line():
    """
    Process command line options.
    """
    import pprint
    for f in sys.argv[1:]:
        print( f )
        pprint.pprint( srf_read( f, mks=True ) )
    
if __name__ == '__main__':
    command_line()
 
