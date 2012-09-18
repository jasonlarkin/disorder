##/opt/open-mpi/tcp-gnu41/bin/mpirun -np 4 $EXEPATH/lmp_generic < $RUNPATH/lmp.in.sed.1

#!/bin/bash
cd $PBS_O_WORKDIR
module load openmpi-psm-gcc

RUNPATH=/home/jason/disorder/lj/amor/4x/XCORR_AF/220/2_10K_relax/time
EXEPATH=/home/jason/lammps/lammps-2Nov10/src

mpirun -np `cat $PBS_NODEFILE | wc -l` $EXEPATH/lmp_generic < $RUNPATH/lmp.in.timex0

