
%--------------------------------------------------------------------------  
%--------------------------------------------------------------------------  
    [tmp,AF.str.main]=system('pwd');
%--------------------------------------------------------------------------

AF.matlab.path = '/opt/mcgaugheygroup/matlab_R2011a/bin';
AF.matlab.lib = '/opt/mcgaugheygroup/matlab_R2011a/work';
AF.gulp.matlab.lib = '/home/jason/Documents/MATLAB/nmd';
AF.matlab.exe = 'matlab';

AF.matlab.walltime = 24; 
    AF.matlab.cpu = 1; 
    AF.matlab.mem = 2;
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
AF.x0.Nx = 2;       AF.x0.Ny = 2;       AF.x0.Nz = 2;
AF.x0.luc.Nx = 8;   AF.x0.luc.Ny = 8;   AF.x0.luc.Nz = 8;
AF.x0.luc.rand.seed = 1;
AF.x0.luc.type='random';
%--------------------------------------------------------------------------

AF.x0.alat = [1.55 1.55 1.55];

AF.mass = [1 3];


%steps

AF.x0 = m_af_create_structure( AF );

% AF.x0 = load(strcat(AF.str.main,'/x0.mat');

pause

AF = m_af_create_jobfiles( AF )
