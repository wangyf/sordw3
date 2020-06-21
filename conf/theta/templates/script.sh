#!/bin/bash
#COBALT -t %(walltime)s
#COBALT -n %(nodes)s
#COBALT -q default 
#COBALT --attrs mcdram=cache:numa=quad 
#COBALT -A SeismicHazard_2 


# Launch MPI code... 
cd %(rundir)r

echo "$( date ): %(name)s started" >> log
%(pre)s
export MPICH_MPIIO_HINTS='*:romio_cb_read=disable'
/usr/bin/time -p aprun -n %(np)s -N 64  %(bin)s
%(post)s
echo "$( date ): %(name)s finished" >> log
