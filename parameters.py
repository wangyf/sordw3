#!/usr/bin/env python
"""
Default simulation parameters

Spatial difference operator level:

  0: Auto pick 2 or 6
  1: Mesh with constant spacing dx
  2: Rectangular mesh
  3: Parallelepiped mesh
  4: One-point quadrature
  5: Exactly integrated elements
  6: Saved operators, nearly as fast as 2, but doubles the memory usage
"""

# I/O and code execution parameters
np3 = 1, 1, 1			# number of processors in j k l
mpin = 1			# input:  0=separate files, 1=MPI-IO, -1=non-collective MPI-IO
mpout = 1			# output: 0=separate files, 1=MPI-IO, -1=non-collective MPI-IO
itstats = 10			# interval for calculating statistics
itio = 50			# interval for writing i/o buffers
itcheck = 0			# interval for check-pointing (0=off)
itstop = 0			# for testing check-pointing, simulates code crash
debug = 0			# >0 verbose, >1 sync, >2 mpi vars, >3 I/O

# Wave model parameters
nn = 41, 41, 42			# number of nodes in j, k, l (double nodes counted)
nt = 41				# number of time steps
dx = 100.0, 100.0, 100.0	# spatial step length
dt = 0.0075			# time step length
tm0 = 0.0			# initial time
affine = (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0) # grid transformation
gridnoise = 0.0			# random noise added to mesh, assumes planar fault
oplevel = 0			# spatial difference operator level
vdamp = -1.0			# Vs dependent damping (e.g., 400)
hourglass = 1.0, 2.0		# hourglass stiffness (1) and viscosity (2)
fieldio = [			# field I/O
    ('=', 'rho', [], 2670.0),	# density
    ('=', 'vp',  [], 6000.0),	# P-wave speed
    ('=', 'vs',  [], 3464.0),	# S-wave speed
    ('=', 'gam', [],    0.0),	# viscosity
    ('=', 'mco', [],    0.0),   # material cohesion
]
rho1 = -1.0			# min density
rho2 = -1.0			# max density
vp1 = -1.0			# min P-wave speed
vp2 = -1.0			# max P-wave speed
vs1 = -1.0			# min S-wave speed
vs2 = -1.0			# max S-wave speed
gam1 = -1.0			# min viscosity
gam2 = 0.8			# max viscosity
npml = 10			# number of PML damping nodes
ppml = 2			# PML exponend, 1-4. Generally 2 is best.
vpml = -1.0			# PML damping velocity, <0 default to min, max V_s harmonic mean
bc1 = 0, 0, 0			# boundary condition - near side
bc2 = 0, 0, 0			# boundary condition - far side
ihypo = 0, 0, 0			# hypocenter indices (with fractional values), 0 = center
rexpand = -1.06			# grid expansion ratio
n1expand = 0, 0, 0		# number of grid expansion nodes - near side
n2expand = 0, 0, 0		# number of grid expansion nodes - far side

# Dynamic rupture parameters
faultnormal = 0			# normal direction to fault plane (0=no fault)
faultopening = 0		# 0=not allowed, 1=allowed
slipvector = 1.0, 0.0, 0.0	# shear traction direction for ts1
svtol = 0.001			# slip velocity considered rupturing

# nucleation by gradually overstressing
vrup = -1.0                     # nucleation rupture velocity, negative = no nucleation
rcrit = 0.0                     # nucleation critical radius
trelax = 0                      # nucleation relaxation time
rrelax = 0                      # nucleation relaxation radius
tslope = 0                      # nucleation traction slope (MPa/km)
rnucl = 0.0                     # nucleation radius/half-width for Ts perturbation
tmnucl = 1.0                    # duration time for Ts pertubation. if negative, perturb at t=0
delts = 0.0                     # peak amplitude increase for Ts perturbation

# friction type
friction = 'slipweakening'      # or 'rateandstate', 'thermalpressurization'
tpnz    = -1                    # (TP) number of grid within diffusion zone
tpsubdt = -1                    # (TP) sub dt < 0.25 * tpdz^2/max(alpha_th,alpha_hy)
tp_method = 'Green'             # (TP) method for thermal pressurization: Green or fd
tp_vw = 'no'                    # (TP) update vw by in-situ temperature yes or no

# Prakash-Clifton gradual response to change of normal traction
pcdep = 'no'                    # or 'yes'  

# elastic or plastic
eplasticity = 'elastic'         # or 'plastic' 
tv = 0.0144                     # viscoplastic relaxation time Tv = dx/Vs = 50./3464.
plmodel = 'none'                # 'DP1: Drucker Prager 1', 'DP2'
pleta = 1e8                     # (DP2) Drucker-prager viscosity (0.1GR_0/C_s) Pa*s

# initial volume stress input 
ivols = 'no'

# Finite source parameters
source = 'potency'		# 'moment', 'potency', 'force', or 'none'
nsource = 0			# number of sub-faults

# Point source parameters
source1 = 0.0, 0.0, 0.0		# normal components
source2 = 0.0, 0.0, 0.0		# shear components
strike = -90.             # 
dip = -90.
rake = -0.
m0 = -1.e16                # N.m (Mw4.6)
timefunction = 'none'		# time function, see util.f90 for details.
period = 10 * dt		# dominant period

# Placeholders
i1pml = None
i2pml = None

