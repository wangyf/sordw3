#!/bin/bash

#PBS -A jmz 
#PBS -N %(name)s
#PBS -l nodes=%(nodes)s:ppn=32
#PBS -l walltime=%(walltime)s
#PBS -e stderr
#PBS -o stdout
#PBS -m abe

cd %(rundir)r

echo "$( date ): %(name)s started" >> log
%(pre)s
/usr/bin/time -p aprun -n %(np)s -N 16 -d 2 %(bin)s
%(post)s
echo "$( date ): %(name)s finished" >> log

