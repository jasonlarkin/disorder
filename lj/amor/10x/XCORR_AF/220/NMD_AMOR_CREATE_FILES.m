%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%INPUT
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

NMD.LJ.eps = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
NMD.LJ.sigma = 3.4E-10;                 %Angstroms 3.4E-10 meters
NMD.LJ.mass = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
NMD.LJ.tau = sqrt((NMD.LJ.mass*(NMD.LJ.sigma^2))/NMD.LJ.eps);
NMD.constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
NMD.constant.hbar = 1.054E-34;                %J/s
NMD.constant.i = sqrt(-1);
NMD.constant.c = 29979245800.00019;      %cm/s
NMD.constant.s2ps = 1E-12;
NMD.constant.ang2m = 1E-10;
NMD.constant.eV2J = 1.60217646E-19;

%--------------------------------------------------------------------------
    [tmp,NMD.str.main]=system('pwd');
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
    NMD.alloy_conc = 0.0;
%--------------------------------------------------------------------------
    NMD.m(1) = 1.0; NMD.m(2) = 3.0; NMD.NUM_ATOMS_TYPE = 1;
%--------------------------------------------------------------------------

NMD.walltime.lammps = 8; NMD.cpu.lammps = 8; 
NMD.walltime.matlab = 12; NMD.cpu.matlab = 8; NMD.mem.matlab = 16;

%--------------------------------------------------------------------------
    NMD.Nx = 10; NMD.Ny = 10; NMD.Nz = 10;
%--------------------------------------------------------------------------
    NMD.seed.initial = 1:5;
    NMD.seed.alloy = 1;
%--------------------------------------------------------------------------

%SED PARAMETERS------------------------------------------------------------    

%ISEED---------------------------------------------------------------------
    NMD.NUM_SEEDS = size(NMD.seed.initial,2);
%--------------------------------------------------------------------------   

%---IKSLICE----------------------------------------------------------------
    NMD.NUM_MODESLICES = 100;
%--------------------------------------------------------------------------   

%TIMES---------------------------------------------------------------------
    NMD.t_total = 2^20; NMD.t_fft = 2^20; NMD.t_step = 2^5; NMD.dt = 0.002;
    NMD.NUM_TSTEPS = NMD.t_fft/NMD.t_step; 
%-------------------------------------------------------------------------- 

%IFFT----------------------------------------------------------------------
    NMD.NUM_FFTS = NMD.t_total/NMD.t_fft;
%-------------------------------------------------------------------------- 

%FREQS---------------------------------------------------------------------
    NMD.w_step = 2*pi/(NMD.t_fft*NMD.dt); 
    NMD.w_max = 2*pi/(NMD.t_step*NMD.dt*2);
%-------------------------------------------------------------------------- 

%--------------------------------------------------------------------------

%read data
dummy =...
    dlmread(...
    strcat(NMD.str.main,'/x0.data'));
   
    NMD.param = dummy(1,:);
    NMD.x0(:,1) = dummy(2:size(dummy,1),1);
    NMD.x0(:,2) = dummy(2:size(dummy,1),2);
    NMD.x0(:,3) = dummy(2:size(dummy,1),3);
    NMD.x0(:,4) = dummy(2:size(dummy,1),4);
    NMD.x0(:,5) = dummy(2:size(dummy,1),5);
    
    NMD.mass = NMD.x0(:,2);
    
%replace double alloy
    I = find(NMD.mass(:,1) ==1); NMD.mass(I) = NMD.mass(1);
    I = find(NMD.mass(:,1) ==2); NMD.mass(I) = NMD.mass(2);
    
    NMD.NUM_ATOMS_UCELL = NMD.param(1);
    
    NMD.NUM_MODES = NMD.param(1)*3; NMD.NUM_ATOMS = size(NMD.x0,1);
    
    NMD.NUM_UCELL_COPIES=NMD.NUM_ATOMS/NMD.NUM_ATOMS_UCELL;
    
    NMD.Lx = NMD.param(3); NMD.Ly = NMD.param(4); NMD.Lz = NMD.param(5);
    NMD.VOLUME = NMD.Lx*NMD.Ly*NMD.Lz;
    NMD.alat = NMD.Lx/NMD.Nx;
    
    NMD.freq =...
    dlmread(...
    strcat(NMD.str.main,'/AF_freq_',int2str(NMD.seed.alloy),'.dat')); 

%set gamma
NMD.kpt.NUM_KPTS = 1;
NMD.kpt.cart(1,:) = [0 0 0];

NMD.kptmaster = NMD.kpt.cart;
NMD.NUM_KPTS = size(NMD.kptmaster(:,1:3),1);
NMD.kptmaster_index = 1:NMD.NUM_KPTS;

    
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%INPUT
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%LAMMPS
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------

%for NMD.seed.alloy=1:size(NMD.seed.alloy,2)
    str.cmd = ['mkdir -p ./' int2str(NMD.seed.alloy) '/work/NMD'];
    system(str.cmd);

    str.cmd = ['mkdir -p ./' int2str(NMD.seed.alloy)];
    system(str.cmd);
    
    str.cmd = ['cp ./lmp_submit.sh.temp ./' int2str(NMD.seed.alloy) '/work/lmp_submit.sh'];
    system(str.cmd);    
%output lammps    
        str.orig = 'NUM_ATOMS';
        str.change = [int2str(NMD.NUM_ATOMS)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
        if NMD.alloy_conc == 0.0
        str.orig = 'NUM_ATOMS_TYPE';
        str.change = [int2str(NMD.NUM_ATOMS_TYPE)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        else
        str.orig = 'NUM_ATOMS_TYPE';
        str.change = [int2str(NMD.NUM_ATOMS_TYPE)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        end
        
        str.orig = 'LX';
        str.change = [num2str( NMD.Lx )];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LY';
        str.change = [num2str( NMD.Lx )];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LZ';
        str.change = [num2str( NMD.Lx )];
        str.cmd5 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'ATOM_MASS_1';
        str.change = ['1 ' num2str(NMD.m(1))];
        str.cmd6 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
    if NMD.alloy_conc == 0.0
        str.mass2 ='';
    else
    str.orig = 'ATOM_MASS_2';
    str.change = ['2 ' num2str(NMD.m(2))];
    str.mass2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    end
        
    str.cmd8 =...
        ['lmp.in.x0.temp > ./' int2str(NMD.seed.alloy)...
        '/work/lmp.in.x0.' int2str(NMD.seed.alloy)];
        
        str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5...
            str.cmd6 str.mass2 str.cmd8];
        
    system(str.cmd);
        
output = [NMD.x0(:,1:5)];
str.write=...
    strcat(...
    NMD.str.main,['/' int2str(NMD.seed.alloy) '/work/lmp.in.x0.' int2str(NMD.seed.alloy)]);
dlmwrite(str.write,output,'-append','delimiter','\t');

        
%loops over initial seeds
    for iseed=1:size(NMD.seed.initial,2)
        
%lmp_ISEED.sh------------------------------------------------------        
        str.orig = 'lmp.sh.temp';
        str.change = ['lmp' int2str(iseed) '.sh'];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'runpath';
        str.change = strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/work');
        str.temp = strcat('-e ''s|',str.orig,'|',str.change);
        str.cmd2 = [str.temp '|g'' '];
        str.orig = 'LMP.TEMP';
        str.change = ['lmp.in.sed.' int2str(iseed)];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'lmp_temp';
        str.change = ['lmp' int2str(iseed)];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    
    str.cmd5 = ['lmp.sh.temp > ./' int2str(NMD.seed.alloy) '/work/lmp' int2str(iseed) '.sh'];
    
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5];
    
        system(str.cmd);
               
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
            int2str(iseed)];
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
            ['lmp.in.sed.temp > ./' int2str(NMD.seed.alloy) '/work/lmp.in.sed.' int2str(iseed)];
        
        str.cmd =...
            ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5...
            str.cmd6 str.cmd7 str.cmd8 str.cmd9 ];       
        system(str.cmd);
        
 	%lmp_submit.sh------------------------------------------------------------- 
    output =...
        ['qsub -l walltime=' int2str(NMD.walltime.lammps)...
        ':00:00 -l nodes=1:ppn=' int2str(NMD.cpu.lammps)...
        ' lmp' int2str(iseed) '.sh'];
    
    str.write = strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/work/lmp_submit.sh');
    dlmwrite(str.write,output,'-append','delimiter','');

    end 
%end

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%LAMMPS
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------



%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%MATLAB
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------


    
%CREATE PROGRAM FILES------------------------------------------------------   

%KPT LISTS-----------------------------------------------------------------

NMD.modemaster = 1:NMD.NUM_MODES;
slice_length = size(NMD.modemaster,2)/NMD.NUM_MODESLICES;

% remainder_length = size(NMD.kptlist,1) - slice_length*(NMD.NUM_KSLICE-1);
for imslice = 1:NMD.NUM_MODESLICES
    NMD.modelist(:,imslice) =...
        NMD.modemaster( (imslice-1)*slice_length+1:(imslice)*slice_length);
end

%MAKES JOB FILES-----------------------------------------------------------

system(...
    strcat('cp ./NMD_submit.sh.temp ./',int2str(NMD.seed.alloy),'/work/NMD_submit.sh'));

    for imode = 1:NMD.NUM_MODESLICES
%NMD_ISEED_IKSLICE.sh------------------------------------------------------        
        str.orig = 'NMD_temp';
        str.change = ['NMD_' int2str(imode)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'runpath';
        str.change = strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/work');
        str.temp = strcat('-e ''s|',str.orig,'|',str.change);
        str.cmd2 = [str.temp '|g'' '];
        str.orig = 'NMD_TEMP.m';
        str.change = ['NMD_' int2str(imode) '.m'];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.cmd4 =...
        ['NMD.sh.temp > ./' int2str(NMD.seed.alloy) '/work/NMD_' int2str(imode) '.sh'];
    
    
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4];
        system(str.cmd);
%NMD_ISEED_IKSLICE.m-------------------------------------------------------        
        str.orig = 'runpath';
        str.change = strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/work');
        str.temp = strcat('-e ''s|',str.orig,'|',str.change);
        str.cmd1 = [str.temp '|g'' '];
        str.orig = 'IMSLICE';
        str.change = [int2str(imode)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.cmd3 =...
            ['NMD.m.temp > ./' int2str(NMD.seed.alloy) '/work/NMD_' int2str(imode) '.m'];
        str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3];
        system(str.cmd);
%NMD_submit.sh------------------------------------------------------------- 
    output =...
        ['qsub -l walltime=' int2str(NMD.walltime.matlab)...
        ':00:00,nodes=' int2str(NMD.cpu.matlab)...
        ',mem=' int2str(NMD.mem.matlab)...
        'gb NMD_' int2str(imode) '.sh'];
    
    str.write = strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/work/NMD_submit.sh');
    dlmwrite(str.write,output,'-append','delimiter','');

    end
    

system(strcat('cp ./NMD_grep.m ./',int2str(NMD.seed.alloy),'/work/NMD_grep.m'));

system(strcat('cp ./NMD_SEED.m ./',int2str(NMD.seed.alloy),'/work/NMD_SEED.m'));

%SAVE NMD structure--------------------------------------------------------  

save(strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/work/NMD.mat'), '-struct', 'NMD');


% str(1,:) = 'qdel 515042'; cnt=2;
% for i=515042:515121
% str(cnt,:) = ['qdel ' int2str(i)]; cnt = cnt+1;
% end
