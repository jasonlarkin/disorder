#!/bin/sh

qsub -l walltime=2:00:00 -l nodes=1:ppn=1 lmp1.sh
