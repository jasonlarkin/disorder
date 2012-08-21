#!/bin/sh

qsub -l cput=48:00:00 -l nodes=1:ppn=32 matlab.sh

