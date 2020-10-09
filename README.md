### sordw3
## Support Operator Rupture Dynamics


[Introduction and examples](https://wangyf.github.io/software/)


> # Requirments:
> - Python3
> - Fortran 90 (later) compiler 
> - MPI library


## Installation:

Requirements:	 MPI (Fortran), python3 (with authorization to write)  

## Step 1: Download the latest SORD 
## git clone https://github.com/wangyf/sordw3

## Step 2: python -m sordw3.configure local
	Note: if errors reported, please modify compiler in conf/local/conf.py

A successful message is like: 
dtype = '<f4'
email = 'yongfeiwang'
fortran_flags = {'g': ('-fbounds-check', '-ffpe-trap=invalid,zero,overflow', '-g', '-fcheck=all', '-cpp', '-fdefault-real-8', '-Ddoubleprecision', '-fimplicit-none', '-Wall -Wno-maybe-uninitialized', '-o'), 't': ('-fbounds-check', '-ffpe-trap=invalid,zero,overflow', '-fcheck=all', '-cpp', '-fdefault-real-8', '-Ddoubleprecision', '-fimplicit-none', '-Wall -Wno-maybe-uninitialized', '-o'), 'p': ('-O', '-pg', '-fcheck=all', '-cpp', '-fdefault-real-8', '-Ddoubleprecision', '-fimplicit-none', '-Wall -Wno-maybe-uninitialized', '-o'), 'O': ('-O3', '-fcheck=all', '-cpp', '-fdefault-real-8', '-Ddoubleprecision', '-fimplicit-none', '-Wall -Wno-maybe-uninitialized', '-o')}
fortran_mpi = ('mpif90',)
fortran_serial = ('gfortran',)
host = 'Yongfei-Wang-SCEC.local'
hosts = ('',)
infiles = ()
itbuff = 10
login = ''
machine = 'local'
maxcores = 56
maxnodes = 500
maxram = 30000
maxtime = 0
mode = None
optimize = 'O'
os_ = posix.uname_result(sysname='Darwin', nodename='Yongfei-Wang-SCEC.local', release='18.7.0', version='Darwin Kernel Version 18.7.0: Mon Feb 10 21:08:45 PST 2020; root:xnu-4903.278.28~1/RELEASE_X86_64', machine='x86_64')
post = ''
pre = ''
prepare = True
queue = None
rate = 1000000.0
run = False
rundir = 'run'
user = 'yongfeiwang'

## Step 3: python -m sordw3.setup
This step is compiling the executable. If successful, sord-sO and sord-mO will be stored in sordw3/bin/

A successful message is:
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c globals.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c diffcn.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c diffnc.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c hourglass.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c bc.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c surfnormals.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c util.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c frio.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c mpi.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c arrays.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c fieldio.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c stats.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c parameters.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c setup.f90
setup.f90:26:10:
 
 ip3root = (ihypo - 1.0) / nl3
          1
Warning: Possible change of value in conversion from REAL(8) to INTEGER(4) at (1) [-Wconversion]
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c gridgen.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c material.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c source.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c inivolstress.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c thermpres.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c rupture.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c resample.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c checkpoint.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c timestep.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c plastic.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c stress.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c acceleration.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized -c sord.f90
mpif90 -O3 -fcheck=all -cpp -fdefault-real-8 -Ddoubleprecision -fimplicit-none -Wall -Wno-maybe-uninitialized globals.o diffcn.o diffnc.o hourglass.o bc.o surfnormals.o util.o frio.o mpi.o arrays.o fieldio.o stats.o parameters.o setup.o gridgen.o material.o source.o inivolstress.o thermpres.o rupture.o resample.o checkpoint.o timestep.o plastic.o stress.o acceleration.o sord.o -o ../bin/sord-mO

## Step 4: python -m sordw3.setup install 
	If needed, you can reinstall SORD by “python -m sordw3.setup reinstall ”

If without any errors popping up, then congratulations, your SORD is ready to use.


