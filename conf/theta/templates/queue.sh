#!/bin/bash -e

cd %(rundir)r

echo "$( date ): %(name)s queued with ID: $( qsub -q default -t %(walltime)s -A SeismicHazard_2 script.sh )" >> log

