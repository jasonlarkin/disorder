FIRST=`qsub -l walltime=24:00:00,nodes=1,mem=2gb job1.sh`
echo $FIRST
SECOND=`qsub -W depend=afterok:$FIRST -l walltime=24:00:00,nodes=1,mem=2gb job2.sh`
echo $SECOND
