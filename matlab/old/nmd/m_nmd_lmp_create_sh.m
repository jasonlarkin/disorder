
function nmd_alloy_lmp_create_sh( NMD )


%for NMD.seed.alloy=1:size(NMD.seed.alloy,2)

    str.cmd = ['mkdir -p ./' int2str(NMD.seed.alloy) '/NMD'];
    system(str.cmd);

    str.cmd = ['mkdir -p ./' int2str(NMD.seed.alloy)];
    system(str.cmd);
    
    str.cmd = ['cp ./lmp_submit.sh.temp ./' int2str(NMD.seed.alloy) '/lmp_submit.sh'];
    system(str.cmd);    


%loops over initial seeds
    for iseed=1:size(NMD.seed.initial,2)
        
%lmp_ISEED.sh------------------------------------------------------        
        str.orig = 'lmp.sh.temp';
        str.change = ['lmp' int2str(iseed) '.sh'];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'runpath';
        str.change = strcat(NMD.str.main,'/',int2str(NMD.seed.alloy));
        str.temp = strcat('-e ''s|',str.orig,'|',str.change);
        str.cmd2 = [str.temp '|g'' '];
        str.orig = 'LMP.TEMP';
        str.change = ['lmp.in.sed.' int2str(iseed)];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'lmp_temp';
        str.change = ['lmp' int2str(iseed)];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    
    str.cmd5 = ['lmp.sh.temp > ./' int2str(NMD.seed.alloy) '/lmp' int2str(iseed) '.sh'];
    
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5];
    
        system(str.cmd);
               

        
 	%lmp_submit.sh------------------------------------------------------------- 
    output =...
        ['qsub -l walltime=' int2str(NMD.walltime.lammps)...
        ':00:00 -l nodes=1:ppn=' int2str(NMD.cpu.lammps)...
        ' lmp' int2str(iseed) '.sh'];
    
        
    
    str.write = strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/lmp_submit.sh');
    dlmwrite(str.write,output,'-append','delimiter','');

    end 
%end



end
