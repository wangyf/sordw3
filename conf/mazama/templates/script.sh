#!/bin/bash
#SBATCH --job-name %(name)s           # Job name
#SBATCH --output=%(name)s.o       # Name of stdout output file
#SBATCH --error=%(name)s.e       # Name of stderr error file
#SBATCH --ntasks=%(np)s             # Total # of mpi tasks
#SBATCH --cpus-per-task=2
#SBATCH --partition=beroza
#SBATCH --mem-per-cpu=32G
#SBATCH --time=%(walltime)s       # Run time (hh:mm:ss)
#SBATCH --mail-type=all       # Send email at begin and end of job
#SBATCH --mail-user=schu3@stanford.edu

# Launch MPI code... 

cd %(rundir)r
module purge
module load intel/19
module load openmpi_3/
echo "$( date ): %(name)s started" >> log
%(pre)s
/bin/time -p srun %(bin)s
%(post)s
echo "$( date ): %(name)s finished" >> log
