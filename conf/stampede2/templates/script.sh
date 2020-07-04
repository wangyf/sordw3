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
#  development	KNL cache-quadrant	16 nodes
#  (1,088 cores)*	2 hrs	1*	0.8 Service Unit (SU)
#  normal	KNL cache-quadrant	256 nodes
#  (17,408 cores)*	48 hrs	50*	0.8 SU
#  large**	KNL cache-quadrant	2048 nodes
#  (139,264 cores)*	48 hrs	5*	0.8 SU
#  long	KNL cache-quadrant	32 nodes
#  (2,176 cores)*	120 hrs	2*	0.8 SU
#  flat-quadrant	KNL flat-quadrant	32 nodes
#  (2,176 cores)*	48 hrs	5*	0.8 SU
#  skx-dev	SKX	4 nodes
#  (192 cores)*	2 hrs	1*	1 SU
#  skx-normal	SKX	128 nodes
#  (6,144 cores)*	48 hrs	25*	1 SU
#  skx-large**	SKX	868 nodes
#  (41,664 cores)*	48 hrs	3*	1 SU
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
#SBATCH -A TG-MCA03S012         # Project/Allocation name (req'd if you have more than 1)
#SBATCH --mail-user=yongfeiw@usc.edu

# Launch MPI code... 

cd %(rundir)r

echo "$( date ): %(name)s started" >> log
%(pre)s
/bin/time -p ibrun %(bin)s
%(post)s
echo "$( date ): %(name)s finished" >> log
