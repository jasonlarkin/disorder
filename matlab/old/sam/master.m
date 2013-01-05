%system('qsub -l walltime=24:00:00 -l nodes=1:ppn=1 slave.sh');
system('qsub -l walltime=24:00:00 -l nodes=1:ppn=1 lmp1.sh');

while exist('dump_1_1.vel') == 0

end

B=magic(100);
dlmwrite('master.dat',B);
