#!/bin/bash
#----------------------------------------------------
# Sample Slurm job script
#   for TACC Frontera CLX nodes
#
#   *** MPI Job in Normal Queue ***
# 
# Last revised: 20 May 2019
#
# Notes:
#
#   -- Launch this script by executing
#      "sbatch clx.mpi.slurm" on a Frontera login node.
#
#   -- Use ibrun to launch MPI codes on TACC systems.
#      Do NOT use mpirun or mpiexec.
#
#   -- Max recommended MPI ranks per CLX node: 56
#      (start small, increase gradually).
#
#   -- If you're running out of memory, try running
#      fewer tasks per node to give each task more memory.
#
#
#  Queue Name	Max Nodes per Job	         Max Duration   Max Jobs	Max Nodes	Charge Rate
#  flex	        128 nodes (7,168 cores)	         48 hrs	        50 jobs	        6400 nodes	.8 SU
#  development	40 nodes  (2,240 cores)           2 hrs	         1 job	        40 nodes	1 Service Unit (SU)
#  normal	512 nodes (28,672 cores)	 48 hrs	        50 jobs	        1024 nodes	1 SU
#  large	513-2048 nodes(114,688 cores)	 48 hrs	        5 jobs	        2048 nodes	1 SU
#  rtx	        22 nodes	                 48 hrs	        5 jobs	        22 nodes	3 SUs
#  rtx-dev	2 nodes	                          2 hrs	        2 jobs         	1	        3 SUs
#
#
#
#
#----------------------------------------------------

#SBATCH -J %(name)s           # Job name
#SBATCH -o %(name)s.o       # Name of stdout output file
#SBATCH -e %(name)s.e       # Name of stderr error file
#SBATCH -p normal             # Queue (partition) name
#SBATCH -N %(nodes)s          # Total # of nodes 
#SBATCH -n %(np)s             # Total # of mpi tasks
#SBATCH -t %(walltime)s       # Run time (hh:mm:ss)
#SBATCH --mail-type=all       # Send email at begin and end of job
#SBATCH -A EAR20006         # Project/Allocation name (req'd if you have more than 1)
#SBATCH --mail-user=yongfeiw@usc.edu

# Launch MPI code... 

cd %(rundir)r

echo "$( date ): %(name)s started" >> log
%(pre)s
/bin/time -p ibrun %(bin)s
%(post)s
echo "$( date ): %(name)s finished" >> log
