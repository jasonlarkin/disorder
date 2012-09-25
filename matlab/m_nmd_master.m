
%--------------------------------------------------------------------------
%STRUCTURE-----------------------------------------------------------------    
%--------------------------------------------------------------------------  
    [tmp,NMD.str.main]=system('pwd');
    NMD.gulp.str.main = NMD.str.main;
%--------------------------------------------------------------------------
    NMD.system(1).str = 'LJ'; NMD.system(2).str = 'alloy';
%--------------------------------------------------------------------------
    NMD.x0.alloy.conc = 0.0; NMD.x0.alloy.seed = 1;
%--------------------------------------------------------------------------
    NMD.x0.mass(1) = 1.0; NMD.x0.mass(2) = 3.0; NMD.x0.NUM_ATOMS_TYPE = 2;
    NMD.x0.alloy.vm =...
        ( (1-NMD.x0.alloy.conc)*NMD.x0.mass(1) +...
        NMD.x0.alloy.conc*NMD.x0.mass(2) );
    NMD.x0.alat = [1.55 1.55 1.55] ;
%--------------------------------------------------------------------------

NMD.env = 'gilgamesh'

NMD = load_env( NMD )

NMD.lmp.path = '/home/jason/lammps';
NMD.lmp.exe = 'lmp_generic';
	NMD.lmp.walltime = 24; 
    NMD.lmp.cpu = 1; 
    NMD.lmp.mem = 2;

NMD.matlab.path = '/opt/mcgaugheygroup/matlab_R2011a/bin';
NMD.matlab.lib = '/opt/mcgaugheygroup/matlab_R2011a/work';
NMD.gulp.matlab.lib = '/home/jason/Documents/MATLAB/nmd';
NMD.matlab.exe = 'matlab'
	NMD.matlab.walltime = 24; 
    NMD.matlab.cpu = 1; 
    NMD.matlab.mem = 2;
    
NMD.gulp.path = '/home/jason/gulp/Src';
NMD.gulp.exe = 'gulp';
    NMD.gulp.walltime = 24; 
    NMD.gulp.cpu = 1; 
    NMD.gulp.mem = 2;
%--------------------------------------------------------------------------
    NMD.x0.Nx = 4; NMD.x0.Ny = 4; NMD.x0.Nz = 4;
%--------------------------------------------------------------------------
    NMD.seed.initial = 1:10;
    NMD.seed.NUM_SEEDS = size(NMD.seed.initial,2);
%--------------------------------------------------------------------------
%SED PARAMETERS------------------------------------------------------------    
%--------------------------------------------------------------------------  
    NMD.type(1).str ='SED'; NMD.type(1).str ='XCORR';
    
    NMD.type(2).str ='DISP'; NMD.type(2).str ='GAMMA';
%--------------------------------------------------------------------------   
    NMD.NUM_KSLICES = 8;
%--------------------------------------------------------------------------   
    NMD.t_total = 2^19; NMD.t_fft = 2^19; NMD.t_step = 2^5; NMD.dt = 0.002;
%--------------------------------------------------------------------------   

NMD = m_nmd_structure_create( NMD );
save('NMD_1.mat')
pause
%LD: GULP LD
gulp_disp( NMD.gulp, NMD.x0 );
pause



NMD = m_nmd_gulp_create_jobfiles(NMD);
save('NMD_2.mat')
%MD: LAMMPS
NMD = load('NMD_2.mat');
m_nmd_lmp_create_jobfiles(NMD);
save('NMD_3.mat')
%NMD: MATLAB
NMD.eigvec =...
    dlmread(strcat(NMD.str.main,'/eigvec.dat'));
NMD.freq =...
    dlmread(strcat(NMD.str.main,'/freq.dat'));
NMD.vel =...
    dlmread(strcat(NMD.str.main,'/vel.dat'));

NMD = load('NMD_3.mat');
NMD = nmd_matlab_create(NMD);
save('NMD_FINISHED.mat')









