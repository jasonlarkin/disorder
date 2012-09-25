#!/bin/bash
cd $PBS_O_WORKDIR
module load openmpi-psm-gcc

RUNPATH=/home/jason/disorder/lj/ald/m1/20x
EXEPATH=/home/jason/LD_300

mpirun -np `cat $PBS_NODEFILE | wc -l` $EXEPATH/joe_mpic++ $RUNPATH/ald.in
