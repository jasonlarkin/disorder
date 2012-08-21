#!/bin/sh
#PBS -l nodes=1:ppn=10
### Merge stderr with stdout
#PBS -j oe
### Queue name
#PBS -q default
###Job name
#PBS -N lmp_10x
### Declare job-non-rerunable
#PBS -r n
#PBS -V
# This job's working directory
echo Job ID: $PBS_JOBID
echo Working directory is $PBS_O_WORKDIR cd $PBS_O_WORKDIR echo Running on host `hostname` echo Time is `date` echo Directory is `pwd` echo This job runs on the following processors:
echo `cat $PBS_NODEFILE`



RUNPATH=/home/jason/lammps/LJ/amorphous/rect/40x4x4
EXEPATH=/home/jason/lammps/lammps-30Nov10/src

cd $RUNPATH

/opt/open-mpi/tcp-gnu41/bin/mpirun -np 10 $EXEPATH/lmp_openmpi < $RUNPATH/in.LJ

