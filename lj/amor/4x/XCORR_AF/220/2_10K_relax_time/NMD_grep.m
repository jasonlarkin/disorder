
NMD=load('./NMD.mat');

%GREP LAMMPS DUMP----------------------------------------------------------    

for iseed=1:NMD.NUM_SEEDS
	for ifft=1:NMD.NUM_FFTS

    cmd = ['grep -A ' int2str(NMD.NUM_ATOMS) ...
        ' "ITEM: ATOMS xu yu zu " dump_' int2str(iseed) '_' int2str(ifft) '.pos > pos.pos'];
    system(cmd);
    cmd = ['grep -v "ITEM: ATOMS xu yu zu " pos.pos > pos2.pos'];
    system(cmd);
    cmd = ['rm dump_' int2str(iseed) '_' int2str(ifft) '.pos'];
    system(cmd);
    cmd = ['mv pos2.pos dump_' int2str(iseed) '_' int2str(ifft) '.pos'];
    system(cmd);


    cmd = ['grep -A ' int2str(NMD.NUM_ATOMS) ...
        ' "ITEM: ATOMS vx vy vz " dump_' int2str(iseed) '_' int2str(ifft) '.vel > vel.vel'];
    system(cmd);
    cmd = ['grep -v "ITEM: ATOMS vx vy vz " vel.vel > vel2.vel'];
    system(cmd);
    cmd = ['rm dump_' int2str(iseed) '_' int2str(ifft) '.vel'];
    system(cmd);
    cmd = ['mv vel2.vel dump_' int2str(iseed) '_' int2str(ifft) '.vel'];
    system(cmd);


    end
end

