#!/bin/sh
#PBS -l nodes=1:ppn=1:node
### Merge stderr with stdout
#PBS -j oe
### Queue name
#PBS -q default
###Job name
#PBS -N NMD_1_1
### Declare job-non-rerunable
#PBS -r n
#PBS -V
# This job's working directory
echo Job ID: $PBS_JOBID
echo Working directory is $PBS_O_WORKDIR cd $PBS_O_WORKDIR echo Running on host `hostname` echo Time is `date` echo Directory is `pwd` echo This job runs on the following processors:
echo `cat $PBS_NODEFILE`



RUNPATH=/home/jason/lammps/LJ/4x
EXEPATH=/home/jason/matlab/matlab_R2010a/bin

cd $RUNPATH

/opt/open-mpi/tcp-gnu41/bin/mpirun -np 1 $EXEPATH/matlab -nodesktop < $RUNPATH/NMD_1_1.m

