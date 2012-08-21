
function nmd_alloy_lmp_create_in( NMD )





%lmp.in.sed.iseed
        str.orig = 'IN.X0';
        str.change = ['lmp.in.x0.' int2str(NMD.seed.alloy)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LMP_TEMP';
        str.change = ['lmp.in.sed.' int2str(iseed)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'IX0';
        str.change = [int2str(NMD.seed.alloy)];
        str.cmd3 = ['-e ''s/\' str.orig '>/' str.change '/g'' '];
        str.orig = 'ISEED_TMP';
        str.change = [int2str(iseed)];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'SEED_TMP';
        str.change = [int2str(iseed) int2str(iseed) int2str(iseed)...
            int2str(iseed) int2str(iseed)];
        str.cmd5 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'T_STEP';
        str.change = [num2str(NMD.t_step)];
        str.cmd6 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'T_FFT';
        str.change = [num2str(NMD.t_fft)];
        str.cmd7 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'T_TOTAL';
        str.change = [num2str(NMD.t_total)];
        str.cmd8 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
        str.cmd9 =...
            ['lmp.in.sed.temp > ./' int2str(NMD.seed.alloy) '/lmp.in.sed.' int2str(iseed)];
        
        str.cmd =...
            ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5...
            str.cmd6 str.cmd7 str.cmd8 str.cmd9 ];       
        system(str.cmd);


end
