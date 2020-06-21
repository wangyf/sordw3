#!/usr/bin/env python
"""
PEER Lifelines program task 1A01, Problem LOH.1

Layer over a halfspace model with buried double-couple source.
http://peer.berkeley.edu/lifelines/lifelines_pre_2006/lifelines_princ_invest_y-7.html#day
http://www-rohan.sdsu.edu/~steveday/BASINS/Final_Report_1A01.pdf
"""
import sord

rundir = '~/run/loh1'
np3 = 1, 16, 1
#np3 = 1, 2, 1
dx = 50.0, 50.0, 50.0
dt = 0.004
L = 8000.0, 10000.0, 6000.0
T = 9.0
nn = [ int( x / dx[0] + 20.5 ) for x in L  ]
nt =   int( T / dt +  1.5 )

# near side boundary conditions:
# anti-mirror symmetry at the near x and y boundaries
# free surface at the near z boundary
bc1 = -2, -2, 0

# far side boundary conditions:
# PML absorbing boundaries at x, y and z boundaries
bc2 = 10, 10, 10

# source
ihypo = 1.5, 1.5, 41.5       # hypocenter indices
source = 'moment'            # specify moment source
timefunction = 'brune'       # Brune pulse source time function
period = 0.1                 # source characteristic time
source1 = 0.0, 0.0, 0.0      # moment tensor comp M_xx, M_yy, M_zz
source2 = 0.0, 0.0, 1e18     # moment tensor comp M_yz, M_zx, M_xy

# material properties
hourglass = 1.0, 2.0                   # hourglass stiffness and viscosity
l = 1.5, 1000.0 / dx[2] + 0.5
fieldio = [
    ( '=', 'rho', [], 2700.0 ),
    ( '=', 'vp',  [], 6000.0 ),
    ( '=', 'vs',  [], 3464.0 ),
    ( '=', 'gam', [],    0.0 ),
    ( '=', 'rho', [(),(),l,()], 2600.0 ),
    ( '=', 'vp',  [(),(),l,()], 4000.0 ),
    ( '=', 'vs',  [(),(),l,()], 2000.0 ),
]

# receivers
for i in range(10):
    j = ihypo[0] + 600.0 * (i + 1) / delta[0]
    k = ihypo[1] + 800.0 * (i + 1) / delta[1]
    fieldio += [
        ('=w', 'v1', [j,k,1,()], 'p%s-v1.bin' % i),
        ('=w', 'v2', [j,k,1,()], 'p%s-v2.bin' % i),
        ('=w', 'v3', [j,k,1,()], 'p%s-v3.bin' % i),
    ]

# run job
if __name__ == '__main__':
    sord.run( locals() )

