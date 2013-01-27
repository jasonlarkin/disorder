#!/bin/sh

qsub -l walltime=72:00:00 -l nodes=1:ppn=1 gulp.sh
