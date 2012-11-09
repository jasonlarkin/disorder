%--------------------------------------------------------------------------
%input
%--------------------------------------------------------------------------
clear
%--------------------------------------------------------------------------
    nmd.str.main = '/home/jason/disorder2/si/alloy/0.05/20x/' ;
    nmd.str.matlab = '/home/jason/disorder/matlab/';
    nmd.str.gulp = 'gulp_disp_si_conv.tmp';
    nmd.str.lmp_in = 'lmp.in.x0.alloy.single.tmp';
    nmd.str.lmp_sed = 'lmp.in.sed.si.tmp';
    nmd.str.m_sed = 'm_nmd_sed_si.m.tmp';
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
    nmd.x0.Nx = 20; nmd.x0.Ny = 20; nmd.x0.Nz = 20;
%--------------------------------------------------------------------------
    nmd.x0.alloy_conc = 0.05;
%--------------------------------------------------------------------------
    nmd.x0.mass(1) = 1.0; nmd.x0.mass(2) = 2.6; 
    nmd.x0.NUM_ATOMS_TYPE = 1;
    nmd.x0.vm =...
        ( (1-nmd.x0.alloy_conc)*nmd.x0.mass(1) +...
        nmd.x0.alloy_conc*nmd.x0.mass(2) );
%--------------------------------------------------------------------------
    nmd.walltime.lammps = 2; 
    nmd.cpu.lammps = 1; 
    nmd.walltime.matlab = 2; 
    nmd.cpu.matlab = 1; 
    nmd.mem.matlab = 2;

%--------------------------------------------------------------------------
    nmd.x0.alloy_seed = 1;
    nmd.seed.initial = 1:10;
%--------------------------------------------------------------------------
    nmd.si = m_si;
%-------------------------------------------------------------------------- 
%sed parameters   
%-------------------------------------------------------------------------- 

%-------------------------------------------------------------------------- 
%ISEED
%-------------------------------------------------------------------------- 
    nmd.NUM_SEEDS = size(nmd.seed.initial,2);
%--------------------------------------------------------------------------   
%IKSLICE
%-------------------------------------------------------------------------- 
    nmd.NUM_KSLICES = 4;
%--------------------------------------------------------------------------   
%TIMES
%-------------------------------------------------------------------------- 
    nmd.t_total = 2^21; nmd.t_fft = 2^19; nmd.t_step = 2^5;nmd.dt = 0.0005;
    nmd.NUM_TSTEPS = nmd.t_fft/nmd.t_step; 
%-------------------------------------------------------------------------- 
%IFFT
%-------------------------------------------------------------------------- 
    nmd.NUM_FFTS = nmd.t_total/nmd.t_fft;
%-------------------------------------------------------------------------- 
%FREQS
%-------------------------------------------------------------------------- 
    nmd.w_step = 2*pi/(nmd.t_fft*nmd.dt); 
    nmd.w_max = 2*pi/(nmd.t_step*nmd.dt*2);
    nmd.NUM_OMEGAS = nmd.t_fft/(2*nmd.t_step); 
%-------------------------------------------------------------------------- 
    nmd.constant = m_constant;
%-------------------------------------------------------------------------- 
    alat = m_si_alat;
    nmd.x0.alat = alat(1,1);
%--------------------------------------------------------------------------
%structure
%--------------------------------------------------------------------------
[nmd.x0.direct nmd.x0.cart nmd.x0.latvec nmd.x0.latvec_rec] =...
    m_si_unit_cell_conv( nmd.x0.alat )
[nmd.x0.x nmd.x0.y nmd.x0.z] =...
    m_build_supercell(...
        nmd.x0.cart, (nmd.x0.alat)*nmd.x0.latvec,...
        nmd.x0.Nx, nmd.x0.Ny, nmd.x0.Nz );
nmd.x0.NUM_ATOMS = size(nmd.x0.x,1);
nmd.x0.NUM_ATOMS_UCELL = size(nmd.x0.cart,1);
%Define number of atoms 
nmd.x0.NUM_UCELL_COPIES=nmd.x0.NUM_ATOMS/nmd.x0.NUM_ATOMS_UCELL; 
nmd.x0.NUM_MODES = 3*nmd.x0.NUM_ATOMS_UCELL;
nmd.NUM_MODES = nmd.x0.NUM_MODES;

nmd.x0.Lx = nmd.x0.Nx*nmd.x0.alat;
nmd.x0.Ly = nmd.x0.Ny*nmd.x0.alat;
nmd.x0.Lz = nmd.x0.Nz*nmd.x0.alat;

nmd.x0.VOLUME = nmd.x0.Lx*nmd.x0.Ly*nmd.x0.Lz;

%randomize masses: only set up to do 2 species
[nmd.x0.id,nmd.x0.m] =...
    m_create_binaryalloy(...
    nmd.x0.mass, nmd.x0.NUM_ATOMS, nmd.x0.alloy_conc, nmd.x0.alloy_seed );
%output universal
m_x0_write(nmd.x0, nmd.str.main , 'x0.data');
 

%--------------------------------------------------------------------------
%GULP
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%kpt
%--------------------------------------------------------------------------
[nmd.kpt.cart nmd.kpt.integer] =...
    m_create_kptlist(...
    nmd.x0.latvec_rec , nmd.x0.Nx , nmd.x0.Ny , nmd.x0.Nz );
nmd.kpt.NUM_KPTS = size(nmd.kpt.cart,1);

nmd.kptmaster =...
    bsxfun(@times,nmd.kpt.cart,[nmd.x0.Nx nmd.x0.Ny nmd.x0.Nz]);
nmd.NUM_KPTS = size(nmd.kptmaster(:,1:3),1);
nmd.kptmaster_index = 1:nmd.NUM_KPTS;

%KPT LISTS-----------------------------------------------------------------
slice_length = size(nmd.kptmaster,1)/nmd.NUM_KSLICES;
% remainder_length = size(nmd.kptlist,1) - slice_length*(nmd.NUM_KSLICE-1);
for ikslice = 1:nmd.NUM_KSLICES
    nmd.kptlist(:,1:3,ikslice) =...
        nmd.kptmaster(...
        (ikslice-1)*slice_length+1:(ikslice)*slice_length,1:3);
    nmd.kpt_index(:,ikslice) =...
        nmd.kptmaster_index(...
        (ikslice-1)*slice_length+1:(ikslice)*slice_length);
end


if exist(...
        [nmd.str.main 'eigvec.dat'], 'file')~=0
    system(['rm -f ' nmd.str.main 'eigvec.dat']);
    system(['rm -f ' nmd.str.main 'freq.dat']);
    system(['rm -f ' nmd.str.main 'vel.dat']);
end    

for ikpt=1:size(nmd.kptmaster,1)
nmd.kptmaster(ikpt,:)       

nmd.kptmodelist( (ikpt-1)*nmd.NUM_MODES+1:(ikpt)*nmd.NUM_MODES,1:3) = ...
        repmat(nmd.kptmaster(ikpt,:),nmd.NUM_MODES,1);

kpt = nmd.kptmaster(ikpt,:)./[nmd.x0.Nx nmd.x0.Ny nmd.x0.Nz];

    eigvec =...
        m_gulp_eig_si(...
        kpt,nmd.x0.NUM_ATOMS_UCELL,nmd.si.mass*nmd.x0.vm,...
        nmd.str.main,nmd.str.matlab,nmd.str.gulp);
    freq =...
        m_gulp_freq_si(...
        kpt,nmd.x0.NUM_ATOMS_UCELL,nmd.si.mass*nmd.x0.vm,...
        nmd.str.main,nmd.str.matlab,nmd.str.gulp);
    vel =...
        m_gulp_vel_si(...
        kpt,nmd.x0.NUM_ATOMS_UCELL,nmd.si.mass*nmd.x0.vm,...
        nmd.str.main,nmd.str.matlab,nmd.str.gulp);
%Output formatted eigvec		
    str.write=...
        [nmd.str.main 'eigvec.dat'];
    dlmwrite(str.write,eigvec,'-append','delimiter',' ');
%Output formatted freqs
    str.write=...
        [nmd.str.main 'freq.dat'];
    dlmwrite(str.write,freq','-append','delimiter',' ');
%Output velocities
    str.write=...
        [nmd.str.main 'vel.dat'];
    dlmwrite(str.write,vel,'-append','delimiter',' ');
    
end

nmd.eigvec =...
    dlmread([nmd.str.main 'eigvec.dat']);
nmd.freq =...
    dlmread([nmd.str.main 'freq.dat']);
nmd.vel =...
    dlmread([nmd.str.main 'vel.dat']);
%--------------------------------------------------------------------------


%--------------------------------------------------------------------------
%LAMMPS
%--------------------------------------------------------------------------
str.cmd = ['mkdir -p ' nmd.str.main 'nmd'];
system(str.cmd);

str.cmd =...
    ['cp ' nmd.str.matlab 'lmp_submit.sh.tmp '...
    nmd.str.main 'lmp_submit.sh'];
system(str.cmd);    

orig(1).str = 'NUM_ATOMS';
change(1).str = int2str(nmd.x0.NUM_ATOMS);
orig(2).str = 'NUM_ATOM_TYPE';
change(2).str = int2str(nmd.x0.NUM_ATOMS_TYPE);
orig(3).str = 'LX';
change(3).str = num2str((nmd.x0.Nx)*nmd.x0.latvec(1,1)*nmd.x0.alat);
orig(4).str = 'LY';
change(4).str = num2str((nmd.x0.Nx)*nmd.x0.latvec(1,1)*nmd.x0.alat);
orig(5).str = 'LZ';
change(5).str = num2str((nmd.x0.Nx)*nmd.x0.latvec(1,1)*nmd.x0.alat);
orig(6).str = 'ATOM_MASS_1';
change(6).str = num2str(nmd.x0.mass(1)*nmd.si.mass);

m_change_file_strings(...
    [nmd.str.matlab nmd.str.lmp_in],...
    orig,...
    [nmd.str.main 'lmp.in.x0'],...
    change)
clear orig change
          
output = [nmd.x0.id nmd.x0.m nmd.x0.x nmd.x0.y nmd.x0.z];
str.write=...
    [nmd.str.main 'lmp.in.x0'];
dlmwrite(str.write,output,'-append','delimiter','\t');


for iseed=1:size(nmd.seed.initial,2)
%lmp_ISEED.sh------------------------------------------------------  
    orig(1).str = 'lmp.sh.tmp';
    change(1).str = ['lmp' int2str(iseed) '.sh'];
    orig(2).str = 'runpath';
    change(2).str = nmd.str.main(1:end-1);
    orig(3).str = 'LMP_TMP';
    change(3).str = ['lmp.in.sed.' int2str(iseed)];
    orig(4).str = 'lmp_tmp';
    change(4).str = ['lmp' int2str(iseed) '.sh'];
    
    m_change_file_strings(...
    [nmd.str.matlab 'lmp.sh.tmp'],...
    orig,...
    [nmd.str.main 'lmp' int2str(iseed) '.sh'],...
    change);
clear orig change
%lmp.in.sed.iseed
    orig(1).str = 'IN.X0';
    change(1).str = ['lmp.in.x0'];
    orig(2).str = 'LMP_TMP';
    change(2).str = ['lmp.in.sed.' int2str(iseed)];
    orig(3).str = 'IX0';
    change(3).str = [int2str(nmd.x0.alloy_seed)];
    orig(4).str = 'ISEED_TMP';
    change(4).str = [int2str(iseed)];
    orig(5).str = 'ISEED_TMP';
    change(5).str = [int2str(iseed)];
    orig(6).str = 'SEED_TMP';
    change(6).str = [int2str(iseed) int2str(iseed) int2str(iseed)...
        int2str(iseed) int2str(iseed)];
    orig(7).str = 'T_STEP';
    change(7).str = [num2str(nmd.t_step)];
    orig(8).str = 'T_FFT';
    change(8).str = [num2str(nmd.t_fft)];
    orig(9).str = 'T_TOTAL';
    change(9).str = [num2str(nmd.t_total)];
    
    
    
    m_change_file_strings(...
    [nmd.str.matlab nmd.str.lmp_sed],...
    orig,...
    [nmd.str.main 'lmp.in.sed.' int2str(iseed)],...
    change);
clear orig change
%lmp_submit.sh------------------------------------------------------------- 
output =...
    ['qsub -l walltime=' int2str(nmd.walltime.lammps)...
    ':00:00 -l nodes=1:ppn=' int2str(nmd.cpu.lammps)...
    ' lmp' int2str(iseed) '.sh'];
    
str.write = [nmd.str.main 'lmp_submit.sh'];
dlmwrite(str.write,output,'-append','delimiter','');
        
end
%--------------------------------------------------------------------------

system(['cp ' nmd.str.matlab 'Si.sw ' nmd.str.main 'Si.sw']);

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------


%--------------------------------------------------------------------------
%MATLAB
%-------------------------------------------------------------------------- 

system(...
    ['cp ' nmd.str.matlab 'nmd.submit.sh.tmp '...
    nmd.str.main 'nmd_submit.sh']);

for iseed=1:size(nmd.seed.initial,2)
    for ikslice = 1:nmd.NUM_KSLICES
%nmd_ISEED_IKSLICE.sh------------------------------------------------------        
    orig(1).str = 'runpath';
    change(1).str = strcat(nmd.str.main(1:end-1));
    orig(2).str = 'nmd_TMP.m';
    change(2).str = ['nmd_' int2str(iseed) '_' int2str(ikslice) '.m'];
    
    m_change_file_strings(...
    [nmd.str.matlab 'nmd.sh.tmp'],...
    orig,...
    [nmd.str.main 'nmd_' int2str(iseed) '_' int2str(ikslice) '.sh'],...
    change);
        
%nmd_ISEED_IKSLICE.m------------------------------------------------------- 
    orig(1).str = 'AAAAA';
    change(1).str = [int2str(iseed)];
    orig(2).str = 'IKSLICE';
    change(2).str = [int2str(ikslice)];
    
    m_change_file_strings(...
    [nmd.str.matlab nmd.str.m_sed],...
    orig,...
    [nmd.str.main 'nmd_' int2str(iseed) '_' int2str(ikslice) '.m'],...
    change);

%nmd_submit.sh------------------------------------------------------------- 
    output =...
        ['qsub -l walltime=' int2str(nmd.walltime.matlab)...
        ':00:00,nodes=' int2str(nmd.cpu.matlab)...
        ',mem=' int2str(nmd.mem.matlab)...
        'gb nmd_' int2str(iseed) '_' int2str(ikslice) '.sh'];
    
    str.write = strcat(nmd.str.main,'nmd_submit.sh');
    dlmwrite(str.write,output,'-append','delimiter','');

    end
end
%--------------------------------------------------------------------------
%nmd_grep.m-------------------------------------------------------        
system(...
    ['cp ' nmd.str.matlab 'm_nmd_grep_vel.m.tmp '...
    nmd.str.main 'nmd_grep.m']);
orig(1).str = 'runpath';
    change(1).str = strcat(nmd.str.main(1:end-1));
    orig(2).str = 'nmd_TMP.m';
    change(2).str = ['nmd_grep.m'];
    m_change_file_strings(...
    [nmd.str.matlab 'nmd.sh.tmp'],...
    orig,...
    [nmd.str.main 'nmd_grep.sh'],...
    change);
system(...
    ['cp ' nmd.str.matlab 'nmd.submit.sh.tmp '...
    nmd.str.main 'nmd_grep_submit.sh']);
output =...
        ['qsub -l walltime=1:00:00,nodes=' int2str(nmd.cpu.matlab)...
        ',mem=2gb nmd_grep.sh'];
str.write = strcat(nmd.str.main,'nmd_grep_submit.sh');
dlmwrite(str.write,output,'-append','delimiter','');

%--------------------------------------------------------------------------
%SAVE nmd structure--------------------------------------------------------  
save([nmd.str.main,'nmd.mat'], '-struct', 'nmd');
size(nmd.freq)