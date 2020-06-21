#!/bin/bash

#SBATCH --job-name=%(name)s           # Job name
#SBATCH --ntasks=%(np)s
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4G
#SBATCH --time=%(walltime)s       # Run time (hh:mm:ss)

# Launch MPI code... 

cd %(rundir)r

echo "$( date ): %(name)s started" >> log
%(pre)s
/bin/time -p srun %(bin)s >> log
%(post)s
echo "$( date ): %(name)s finished" >> log
