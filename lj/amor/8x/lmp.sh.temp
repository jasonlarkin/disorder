##/opt/open-mpi/tcp-gnu41/bin/mpirun -np 4 $EXEPATH/lmp_generic < $RUNPATH/LMP_TEMP

#!/bin/bash
cd $PBS_O_WORKDIR
module load openmpi-psm-gcc

RUNPATH=runpath
EXEPATH=/home/jason/lammps/lammps-2Nov10/src

mpirun -np `cat $PBS_NODEFILE | wc -l` $EXEPATH/lmp_generic < $RUNPATH/LMP_TEMP 

