#!/usr/bin/env python
"""
Mapping data utilities
"""
import os, urllib, gzip, zipfile, sordw3
import numpy as np
try:
    from cStringIO import StringIO ## for Python 2
except ImportError:
    from io import StringIO ## for Python 3

repo = os.path.expanduser( '~/mapdata' )


def readbin(file,shape,inputdtype=None):
    fd = open( file ,'rb')
    if inputdtype == None:
        inputdtype = np.dtype( 'f8' ).str #real*4
    matrix = np.fromfile(fd, inputdtype).reshape(shape).astype(np.dtype( 'f8' ).str)
    print('Read size check',file,matrix.shape)
    fd.close()
    return matrix

def readbinseg(file,shape,offset=0,count=-1,inputdtype=None):
    if inputdtype == None:
        inputdtype = np.dtype( 'f8' ).str #real*4
    matrix = np.fromfile(fd, inputdtype,offset=offset,count=count).reshape(shape).astype(np.dtype( 'f8' ).str)
    print('Read size check',file,matrix.shape)
    fd.close()
    return matrix

def writebin(file,matrix,outdtype=None):
    fd = open( file ,'wb')
    if dtype==None: 
        dtype=np.dtype( 'f8' ).str #'<f4'
    print('Write size check',file,matrix.shape)
    matrix.astype(outdtype).tofile( fd )
    fd.close()
    return

def tsurf( path ):
    """
    Read GOCAD (http://www.gocad.org) trigulated surface "Tsurf" files.
    """
    fh = open( path )
    tsurf = []
    for line in fh.readlines():
        f = line.split()
        if line.startswith( 'GOCAD TSurf' ):
            tface, vrtx, trgl, border, bstone, name, color = [], [], [], [], [], None, None
        elif f[0] in ('VRTX', 'PVRTX'):
            vrtx += [[float(f[2]), float(f[3]), float(f[4])]]
        elif f[0] in ('ATOM', 'PATOM'):
            i = int( f[2] ) - 1
            vrtx += [ vrtx[i] ]
        elif f[0] == 'TRGL':
            trgl += [[int(f[1]) - 1, int(f[2]) - 1, int(f[3]) - 1]]
        elif f[0] == 'BORDER':
            border += [[int(f[2]) - 1, int(f[3]) - 1]]
        elif f[0] == 'BSTONE':
            bstone += [int(f[1]) - 1]
        elif f[0] == 'TFACE':
            if trgl != []:
                tface += [ np.array( trgl, 'i' ).T ]
            trgl = []
        elif f[0] == 'END':
            vrtx   = np.array( vrtx, 'f' ).T
            border = np.array( border, 'i' ).T
            bstone = np.array( bstone, 'i' ).T
            tface += [ np.array( trgl, 'i' ).T ]
            tsurf += [[vrtx, tface, border, bstone, name, color]]
        elif line.startswith( 'name:' ):
            name = line.split( ':', 1 )[1].strip()
        elif line.startswith( '*solid*color:' ):
            f = line.split( ':' )[1].split()
            color = float(f[0]), float(f[1]), float(f[2])
    return tsurf

def etopo1( indices=None, downsample=1 ):
    """
    Download ETOPO1 Global Relief Model.
    http://www.ngdc.noaa.gov/mgg/global/global.html
    """
    filename = os.path.join( repo, 'etopo%02d-ice.f32' % downsample )
    if not os.path.exists( filename ):
        if not os.path.exists( repo ):
            os.makedirs( repo )
        url = 'ftp://ftp.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/ice_surface/grid_registered/binary/etopo1_ice_g_i2.zip'
        f = os.path.join( repo, os.path.basename( url ) )
        if not os.path.exists( f ):
            print( 'Retrieving %s' % url )
            urllib.urlretrieve( url, f )
        print( 'Creating %s' % filename )
        z = zipfile.ZipFile( f, 'r' ).read( 'etopo1_ice_g_i2.bin' )
        z = np.fromstring( z, '<i2' ).reshape( [21601, 10801] )
        z = np.array( z, 'f' )
        if downsample > 1:
            z = sordw3.coord.downsample_sphere( z, downsample )
        open( filename, 'wb' ).write( z )
    if indices != None:
        shape = (21601 - 1) / downsample + 1, (10801 - 1) / downsample + 1
        return sordw3.util.ndread( filename, shape, indices, 'f' )
    else:
        return

def globe( indices=None ):
    """
    Global Land One-km Base Elevation Digital Elevation Model.
    http://www.ngdc.noaa.gov/mgg/topo/globe.html
    """
    filename = os.path.join( repo, 'globe30.i16' )
    if not os.path.exists( filename ):
        if not os.path.exists( repo ):
            os.makedirs( repo )
        print( 'Building %s' % filename )
        n = 90 * 60 * 2
        url = 'http://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/%s10g.gz'
        tiles = 'abcd', 'efgh', 'ijkl', 'mnop'
        fd = open( filename, 'wb' )
        for j in range( len( tiles ) ):
            row = []
            for k in range( len( tiles[j] ) ):
                u = url % tiles[j][k]
                f = os.path.join( repo, os.path.basename( u ) )
                if not os.path.exists( f ):
                    print( 'Retrieving %s' % u )
                    urllib.urlretrieve( u, f )
                z = gzip.open( f, mode='rb' ).read()
                z = np.fromstring( z, '<i2' ).reshape( [-1, n] )
                row += [z]
            row = np.hstack( row )
            row.tofile( fd )
        fd.close()
        del( z, row )
    if indices != None:
        shape = 43200, 21600
        return sordw3.util.ndread( filename, shape, indices, '<i2' )
    else:
        return

def topo( extent, scale=1.0 ):
    """
    Extrat merged GLOBE/ETOPO1 digital elvation model for given region.
    """
    o = 0.25
    lon, lat = extent
    j = int( lon[0] * 60 + 10801 - o ), int( np.ceil( lon[1] * 60 + 10801 + o ) )
    k = int( -lat[1] * 60 + 5401 - o ), int( np.ceil( -lat[0] * 60 + 5401 + o ) )
    z = etopo1( [j, k], 1 )
    j = 2 * j[0] - 1, 2 * j[1] - 2
    k = 2 * k[0] - 1, 2 * k[1] - 2
    n = j[1] - j[0] + 1, k[1] - k[0] + 1
    z *= 0.0625
    z1 = np.empty( n, z.dtype )
    z1[0::2,0::2] = 9 * z[:-1,:-1] + 3 * z[:-1,1:] + 3 * z[1:,:-1] +     z[1:,1:]
    z1[0::2,1::2] = 3 * z[:-1,:-1] + 9 * z[:-1,1:] +     z[1:,:-1] + 3 * z[1:,1:]
    z1[1::2,0::2] = 3 * z[:-1,:-1] +     z[:-1,1:] + 9 * z[1:,:-1] + 3 * z[1:,1:]
    z1[1::2,1::2] =     z[:-1,:-1] + 3 * z[:-1,1:] + 3 * z[1:,:-1] + 9 * z[1:,1:]
    z = globe( [j, k] )
    i = z != -500
    z1[i] = z[i]
    z = z1
    z *= scale
    lon = (j[0] - 21600.5) / 120, (j[1] - 21600.5) / 120
    lat = (10800.5 - k[1]) / 120, (10800.5 - k[0]) / 120
    return z[:,::-1], (lon, lat)

def us_place_names( kind=None, extent=None ):
    """
    USGS place name database.
    """
    url = 'http://geonames.usgs.gov/docs/stategaz/US_CONCISE.zip'
    filename = os.path.join( repo, os.path.basename( url ) )
    if not os.path.exists( filename ):
        if not os.path.exists( repo ):
            os.makedirs( repo )
        print( 'Downloading %s' % url )
        urllib.urlretrieve( url, filename )
    data = zipfile.ZipFile( filename ).read( 'US_CONCISE.txt' )
    data = StringIO( data )
    name = np.genfromtxt( data, delimiter='|', skip_header=1, usecols=(1,), dtype='S64' )
    data.reset()
    kind_ = np.genfromtxt( data, delimiter='|', skip_header=1, usecols=(2,), dtype='S64' )
    data.reset()
    lat, lon, elev = np.genfromtxt( data, delimiter='|', skip_header=1, usecols=(9,10,15) ).T
    if kind != None:
        i = kind == kind_
        lon = lon[i]
        lat = lat[i]
        elev = elev[i]
        name = name[i]
    if extent != None:
        x, y = extent
        i = (lon >= x[0]) & (lon <= x[1]) & (lat >= y[0]) & (lat <= y[1])
        lon = lon[i]
        lat = lat[i]
        elev = elev[i]
        name = name[i]
    return (lon, lat, elev, name)

def mapdata( kind='coastlines', resolution='high', extent=None, min_area=0.0, min_level=0, max_level=4, delta=None, clip=1 ):
    """
    Reader for the Global Self-consistent, Hierarchical, High-resolution Shoreline
    database (GSHHS) by Wessel and Smith.  WGS-84 ellipsoid.

    kind: 'coastlines', 'rivers', 'borders'
    resolution: 'crude', 'low', 'intermediate', 'high', 'full'
    extent: (min_lon, max_lon), (min_lat, max_lat)

    Reference:
    Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
    High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996.
    http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
    http://www.soest.hawaii.edu/wessel/gshhs/index.html
    """
    nh = 11
    url = 'http://www.ngdc.noaa.gov/mgg/shorelines/data/gshhs/version2.0/gshhs_2.0.zip'
    filename = os.path.join( repo, os.path.basename( url ) )
    kind = dict(c='gshhs', r='wdb_rivers', b='wdb_borders')[kind[0]]
    member = 'gshhs/%s_%s.b' % (kind, resolution[0])
    if kind != 'gshhs':
        min_area = 0.0
    if extent != None:
        lon, lat = extent
        lon = lon[0] % 360, lon[1] % 360
        extent = lon, lat
    if not os.path.exists( filename ):
        if not os.path.exists( repo ):
            os.makedirs( repo )
        print( 'Downloading %s' % url )
        urllib.urlretrieve( url, filename )
    data = np.fromstring( zipfile.ZipFile( filename ).read( member ), '>i' )
    xx = []
    yy = []
    ii = 0
    nkeep = 0
    ntotal = 0
    while ii < data.size:
        ntotal += 1
        hdr = data[ii:ii+nh]
        n = hdr[1]
        ii += nh + 2 * n
        level = hdr[2:3].view( 'i1' )[3]
        if level > max_level:
            break
        if level < min_level:
            continue
        area = hdr[7] * 0.1
        if area < min_area:
            continue
        if extent != None:
            west, east, south, north = hdr[3:7] * 1e-6
            west, east, south, north = hdr[3:7] * 1e-6
            if east < lon[0] or west > lon[1] or north < lat[0] or south > lat[1]:
                continue
        nkeep += 1
        x, y = 1e-6 * np.array( data[ii-2*n:ii].reshape(n, 2).T, 'f' )
        if extent != None and clip != 0:
            if delta:
                x, y = clipdata( x, y, extent, 1 )[:2]
                x, y = densify( x, y, delta )
            x, y = clipdata( x, y, extent, clip )[:2]
        elif delta:
            x, y = densify( x, y, delta )
        xx += [ x, [np.nan] ]
        yy += [ y, [np.nan] ]
    print('%s, resolution: %s, selected %s of %s' % (member, resolution, nkeep, ntotal))
    if nkeep:
        xx = np.concatenate( xx )[:-1]
        yy = np.concatenate( yy )[:-1]
    return np.array( [xx, yy], 'f' )

def clipdata( x, y, extent, lines=1 ):
    """
    Clip data outside extent.
    
    x, y : data coordinates
    extent : (xmin, xmax), (ymin, ymax)
    lines : 0 = points, assume no connectivity.
            1 = line segments, include one extra point past the boundary.
           -1 = line segments, do not include extra point past the boundary.
    """
    x, y = np.array( [x, y] )
    x1, x2 = extent[0]
    y1, y2 = extent[1]
    i = (x >= x1) & (x <= x2) & (y >= y1) & (y <= y2)
    if lines:
        if lines > 0:
            i[:-1] = i[:-1] | i[1:]
            i[1:] = i[:-1] | i[1:]
        x[~i] = np.nan
        y[~i] = np.nan
        i[1:] = i[:-1] | i[1:]
    return x[i], y[i], i

def densify( x, y, delta ):
    """
    Piecewise up-sample line segments with spacing delta.
    """
    x, y = np.array( [x, y] )
    dx = np.diff( x )
    dy = np.diff( y )
    r = np.sqrt( dx * dx + dy * dy )
    xx = [[x[0]]]
    yy = [[y[0]]]
    for i in range( r.size ):
        if r[i] > delta:
            ri = np.arange( delta, r[i], delta )
            xx += [np.interp( ri, [0.0, r[i]], x[i:i+2] )]
            yy += [np.interp( ri, [0.0, r[i]], y[i:i+2] )]
        xx += [[x[i+1]]]
        yy += [[y[i+1]]]
    xx = np.concatenate( xx )
    yy = np.concatenate( yy )
    return np.array( [xx, yy] )

def engdahlcat( path='engdahl-centennial-cat.f32', fields=['lon', 'lat', 'depth', 'mag'] ):
    """
    Engdahl Centennial Earthquake Catalog to binary file.
    http://earthquake.usgs.gov/research/data/centennial.php
    """
    if not os.path.exists( path ):
        fmt = [
            6, ('icat',   'S6'),
            1, ('asol',   'S1'),
            5, ('isol',   'S5'),
            4, ('year',   'i4'),
            3, ('month',  'i4'),
            3, ('day',    'i4'),
            4, ('hour',   'i4'),
            3, ('minute', 'i4'),
            6, ('second', 'f4'),
            9, ('lat',    'f4'),
            8, ('lon',    'f4'),
            6, ('depth',  'f4'),
            4, ('greg',   'i4'),
            4, ('ntel',   'i4'),
            4, ('mag',    'f4'),
            3, ('msc',    'S3'),
            6, ('mdo',    'S6'),
        ]
        url = 'http://earthquake.usgs.gov/research/data/centennial.cat'
        url = urllib.urlopen( url )
        data = np.genfromtxt( url, dtype=fmt[1::2], delimiter=fmt[0::2] )
        out = []
        for f in fields:
            out += [data[:][f]]
        np.array( out, 'f' ).T.tofile( path )
    else:
        out = np.fromfile( path, 'f' ).reshape( (-1,4) ).T
    return out

def upsample( f ):
    n = list( f.shape )
    n[:2] = [ n[0] * 2 - 1, n[1] * 2 - 1 ]
    g = np.empty( n, f.dtype )
    g[0::2,0::2] = f
    g[0::2,1::2] = 0.5 * (f[:,:-1] + f[:,1:])
    g[1::2,0::2] = 0.5 * (f[:-1,:] + f[1:,:])
    g[1::2,1::2] = 0.25 * (f[:-1,:-1] + f[1:,1:] + f[:-1,1:] + f[1:,:-1])
    return g

def downsample( f, d ):
    n = f.shape
    n = (n[0] + 1) / d, (n[1] + 1) / d
    g = np.zeros( n, f.dtype )
    for k in range( d ):
        for j in range( d ):
            g += f[j::d,k::d]
    g *= 1.0 / (d * d)
    return g

def downsample_sphere( f, d ):
    """
    Down-sample node-registered spherical surface with averaging.

    The indices of the 2D array f are, respectively, longitude and latitude.
    d is the decimation interval which should be odd to preserve nodal
    registration.
    """
    n = f.shape
    i = np.arange( d ) - (d - 1) / 2
    jj = np.arange( 0, n[0], d )
    kk = np.arange( 0, n[1], d )
    nn = jj.size, kk.size
    g = np.zeros( nn, f.dtype )
    jj, kk = np.ix_( jj, kk )
    for dk in i:
        k = n[1] - 1 - abs( n[1] - 1 - abs( dk + kk ) )
        for dj in i:
            j = (jj + dj) % n[0]
            g = g + f[j,k]
    g[:,0] = g[:,0].mean()
    g[:,-1] = g[:,-1].mean()
    g *= 1.0 / (d * d)
    return g


def write_cvws(filename,header,*args):
    """
    Standard format for SCEC/USGS rupture dynamics verification project

    # This is the file header:
    # problem=TPV105
    # author=A.Modeler
    # date=2011/01/31
    # code=MyCode
    # code_version=3.7
    # element_size=100 m
    # time_step=0.005
    # num_time_steps=2400
    # location= on fault, 9 km along strike, 7.5km down-dip
    # Column #1 = time (s)
    # Column #2 = horizontal slip (m)
    # Column #3 = horizontal slip rate (m/s)
    # Column #4 = horizontal shear stress (MPa)
    # Column #5 = vertical slip (m)
    # Column #6 = vertical slip rate (m/s)
    # Column #7 = vertical shear stress (MPa)
    # Column #8 = effective normal stress (MPa)
    # Column #9 = state variable psi (dimensionless)
    # Column #10 = temperature (K)
    # Column #11 = pore pressure (MPa)
    #
    # The line below lists the names of the data fields:
    # (Although rendered as two lines on this printed page, it must be one
    # single line in the actual file.)
    t h-slip h-slip-rate h-shear-stress v-slip v-slip-rate v-shear-stress
     n-stress psi temperature pressure
    #
    # Here is the time-series data.
    # There should be 11 numbers on each line, but this page is not wide enough
    # to show 11 numbers on a line, so we only show the first five.
    0.000000E+00 0.000000E+00 0.000000E+00 7.000000E+01 0.000000E+00 ...
    5.000000E-03 0.000000E+00 0.000000E+00 7.104040E+01 0.000000E+00 ...
    1.000000E-02 0.000000E+00 0.000000E+00 7.239080E+01 0.000000E+00 ...
    1.500000E-02 0.000000E+00 0.000000E+00 7.349000E+01 0.000000E+00 ...
    2.000000E-02 0.000000E+00 0.000000E+00 7.440870E+01 0.000000E+00 ...
    2.500000E-02 0.000000E+00 0.000000E+00 7.598240E+01 0.000000E+00 ...



    # Example new contour-plot file.
    #
    # This is the file header:
    # problem=TPV105
    # author=A.Modeler
    # date=2011/01/31
    # code=MyCode
    # code_version=3.7
    # element_size=100 m
    # Column #1 = horizontal coordinate, distance along strike (m)
    # Column #2 = vertical coordinate, distance down-dip (m)
    # Column #3 = rupture time (s)
    #
    # The line below lists the names of the data fields.
    # It indicates that the first column contains the horizontal
    # coordinate (j), the second column contains the vertical
    # coordinate (k), and the third column contains the time (t).
    j k t
    #
    # Here is the rupture history
    6.000000E+02 7.000000E+03 3.100000E-02
    6.000000E+02 7.100000E+03 4.900000E-02
    6.000000E+02 7.200000E+03 6.700000E-02
    7.000000E+02 7.000000E+03 1.230000E-01
    7.000000E+02 7.100000E+03 1.350000E-01
    7.000000E+02 7.2
    """
    fd=open( filename, 'w' )
    fd.write( header )

    ncol = len(args)
    nt = len(args[0])
    # print("# {0} {1}".format(ncol,nt))
    for it in range(nt):
        for icol in range(ncol):
            if icol == ncol-1:
                fd.write('{0:12.6e} \n'.format(args[icol][it]))
            else:
                fd.write('{0:12.6e} '.format(args[icol][it]))

    fd.close()

    return


if __name__ == '__main__':
    import doctest
    doctest.testmod()

