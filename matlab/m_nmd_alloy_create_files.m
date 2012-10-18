%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%INPUT
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------

    

%--------------------------------------------------------------------------
    [tmp,nmd.str.main]=system('pwd');
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
    nmd.alloy_conc = 0.0;
%--------------------------------------------------------------------------
    nmd.m(1) = 1.1; nmd.m(2) = 3.0; nmd.NUM_ATOMS_TYPE = 1;
    nmd.vm =...
        ( (1-nmd.alloy_conc)*nmd.m(1) + nmd.alloy_conc*nmd.m(2) );
%--------------------------------------------------------------------------
    nmd.walltime.lammps = 24; 
    nmd.cpu.lammps = 10; 
    nmd.walltime.matlab = 24; 
    nmd.cpu.matlab = 1; 
    nmd.mem.matlab = 4;
%--------------------------------------------------------------------------
    nmd.Nx = 10; nmd.Ny = 10; nmd.Nz = 10;
%--------------------------------------------------------------------------
    nmd.seed.alloy = 1;
    nmd.seed.initial = 1:10;
%--------------------------------------------------------------------------

%-------------------------------------------------------------------------- 
%SED PARAMETERS   
%-------------------------------------------------------------------------- 

%-------------------------------------------------------------------------- 
%ISEED
%-------------------------------------------------------------------------- 
    nmd.NUM_SEEDS = size(nmd.seed.initial,2);
%--------------------------------------------------------------------------   
%IKSLICE
%-------------------------------------------------------------------------- 
    nmd.NUM_KSLICES = 8;
%--------------------------------------------------------------------------   
%TIMES
%-------------------------------------------------------------------------- 
    nmd.t_total = 2^19; nmd.t_fft = 2^19; nmd.t_step = 2^5; nmd.dt = 0.002;
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
   
alat = m_lj_alat;
nmd.alat = alat(1,2);

%--------------------------------------------------------------------------



%Unit cell and lattice vectors
dummy = [   1.0 0.0  0.0 
            0.0  1.0 0.0 
            0.0  0.0  1.0
            0.0  0.0  0.0
            0.5  0.5  0.0
            0.5  0.0  0.5
            0.0  0.5  0.5];
    
%Define box size and conventional cell lattice parameters
    nmd.latvec(1,1) = dummy(1,1); nmd.latvec(1,2) = dummy(1,2); 
    nmd.latvec(1,3) = dummy(1,3);
    nmd.latvec(2,1) = dummy(2,1); nmd.latvec(2,2) = dummy(2,2); 
    nmd.latvec(2,3) = dummy(2,3);    
    nmd.latvec(3,1) = dummy(3,1); nmd.latvec(3,2) = dummy(3,2); 
    nmd.latvec(3,3) = dummy(3,3);
    
    nmd.latvec = nmd.alat*nmd.latvec;
    
nmd.latvec_rec = [   1.0 0.0  0.0 
                    0.0  1.0 0.0 
                    0.0  0.0  1.0];
    
%first 3 rows are the lattice vectors
    nmd.x.direct = dummy(4:length(dummy),:);
    
    nmd.x.cart(:,1) = nmd.x.direct(:,1)*nmd.latvec(1,1) +...
        nmd.x.direct(:,2)*nmd.latvec(2,1) +...
        nmd.x.direct(:,3)*nmd.latvec(3,1) ;
	nmd.x.cart(:,2) = nmd.x.direct(:,1)*nmd.latvec(1,2) +...
        nmd.x.direct(:,2)*nmd.latvec(2,2) +...
        nmd.x.direct(:,3)*nmd.latvec(3,2) ;
	nmd.x.cart(:,3) = nmd.x.direct(:,1)*nmd.latvec(1,3) +...
        nmd.x.direct(:,2)*nmd.latvec(2,3) +...
        nmd.x.direct(:,3)*nmd.latvec(3,3) ;

%--------------------------------------------------------------------------

%build supercell
N_cnt = 1;
for iNx = 0:nmd.Nx-1
    for iNy = 0:nmd.Ny-1
        for iNz = 0:nmd.Nz-1
nmd.x0( (N_cnt-1)*size(nmd.x.direct,1)+1:(N_cnt)*size(nmd.x.direct,1) ,3) =...
        nmd.x.cart(:,1) + iNx * nmd.latvec(1,1) +...
            iNy*nmd.latvec(2,1) + iNz*nmd.latvec(3,1); 
nmd.x0( (N_cnt-1)*size(nmd.x.direct,1)+1:(N_cnt)*size(nmd.x.direct,1) ,4) =...
        nmd.x.cart(:,2) + iNx * nmd.latvec(1,2) +...
            iNy*nmd.latvec(2,2) + iNz*nmd.latvec(3,2);
nmd.x0( (N_cnt-1)*size(nmd.x.direct,1)+1:(N_cnt)*size(nmd.x.direct,1) ,5) =...
        nmd.x.cart(:,3) + iNx * nmd.latvec(1,3) +...
            iNy*nmd.latvec(2,3) + iNz*nmd.latvec(3,3);
        N_cnt =N_cnt+1;
        end
    end
end

nmd.NUM_ATOMS = size(nmd.x0,1);


%create kptlist in integers
cnt=1;
for ix=(-(nmd.Nx/2)+1):1:(nmd.Nx/2)
    for iy=(-(nmd.Ny/2)+1):1:(nmd.Ny/2)
        for iz=(-(nmd.Nz/2)+1):1:(nmd.Nz/2)            
        nmd.kpt.integer(cnt,:) = [ix iy iz];
        nmd.kpt.cart(cnt,1:3) = ix/nmd.Nx*nmd.latvec_rec(1,:) +...
            iy/nmd.Ny*nmd.latvec_rec(2,:) +...
            iz/nmd.Nz*nmd.latvec_rec(3,:) ;
        nmd.kpt.NUM_KPTS = cnt;
        cnt=cnt+1;
        end
    end
end
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

%for nmd.seed.alloy=1:size(nmd.seed.alloy,2)

    str.cmd = ['mkdir -p ./' int2str(nmd.seed.alloy) '/nmd'];
    system(str.cmd);

    str.cmd = ['mkdir -p ./' int2str(nmd.seed.alloy)];
    system(str.cmd);
    
    str.cmd = ['cp ./lmp_submit.sh.temp ./' int2str(nmd.seed.alloy) '/lmp_submit.sh'];
    system(str.cmd);    
%randomize masses: only set up to do 2 species
    nmd.x0(:,1) = 1:size(nmd.x0,1);
    nmd.x0(:,2) = 1;
%rng(nmd.seed.alloy(nmd.seed.alloy));
    rng( nmd.seed.alloy );
    [I,J] =...
        find(...
        randperm(size(nmd.x0(:,1),1))'<ceil(nmd.alloy_conc*size(nmd.x0,1)));
    nmd.x0(I,2) = 2;
    
%set mass vector

nmd.mass(1:size(nmd.x0,1),1) = 0;
for imass = 1:size(nmd.m,2)
Imass = find(nmd.x0(:,2)==imass); nmd.mass(Imass) = nmd.m(imass);
end
    
%output universal

if exist(...
strcat(nmd.str.main,'/x0_',num2str(nmd.alloy_conc),'_',int2str(nmd.seed.alloy),'.data'), 'file')~=0
system(['rm -f ./x0_' num2str(nmd.alloy_conc) '_' int2str(nmd.seed.alloy) '.data']);
end  

    str.write=strcat(...
        nmd.str.main,'/x0_',num2str(nmd.alloy_conc),'_',int2str(nmd.seed.alloy),'.data');
    output = [size(nmd.x0,1) size(nmd.x.cart,1) nmd.latvec(1,1)*(nmd.Nx)...
        nmd.latvec(2,2)*(nmd.Ny) nmd.latvec(3,3)*(nmd.Nz) ];
    dlmwrite(str.write,output,'-append','delimiter',' ');
    dlmwrite(str.write,nmd.x0,'-append','delimiter',' ');
%output lammps    
        str.orig = 'NUM_ATOMS';
        str.change = [int2str(nmd.NUM_ATOMS)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
        if nmd.alloy_conc == 0.0
        str.orig = 'NUM_ATOMS_TYPE';
        str.change = [int2str(nmd.NUM_ATOMS_TYPE-1)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        else
        str.orig = 'NUM_ATOMS_TYPE';
        str.change = [int2str(nmd.NUM_ATOMS_TYPE)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        end
        
        str.orig = 'LX';
        str.change = [num2str((nmd.Nx)*nmd.latvec(1,1))];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LY';
        str.change = [num2str((nmd.Ny)*nmd.latvec(2,2))];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LZ';
        str.change = [num2str((nmd.Nz)*nmd.latvec(3,3))];
        str.cmd5 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'ATOM_MASS_1';
        str.change = ['1 ' num2str(nmd.m(1))];
        str.cmd6 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
    if nmd.alloy_conc == 0.0
        str.mass2 ='';
    else
    str.orig = 'ATOM_MASS_2';
    str.change = ['2 ' num2str(nmd.m(2))];
    str.mass2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    end
        
    str.cmd8 =...
        ['lmp.in.x0.temp > ./' int2str(nmd.seed.alloy) '/lmp.in.x0.' int2str(nmd.seed.alloy)];
        
        str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5...
            str.cmd6 str.mass2 str.cmd8];
        
        system(str.cmd);
        
        output = [nmd.x0(:,1:5)];
    str.write=strcat(nmd.str.main,['/' int2str(nmd.seed.alloy) '/lmp.in.x0.' int2str(nmd.seed.alloy)]);
        dlmwrite(str.write,output,'-append','delimiter','\t');

%loops over initial seeds
    for iseed=1:size(nmd.seed.initial,2)
        
%lmp_ISEED.sh------------------------------------------------------        
        str.orig = 'lmp.sh.temp';
        str.change = ['lmp' int2str(iseed) '.sh'];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'runpath';
        str.change = strcat(nmd.str.main,'/',int2str(nmd.seed.alloy));
        str.temp = strcat('-e ''s|',str.orig,'|',str.change);
        str.cmd2 = [str.temp '|g'' '];
        str.orig = 'LMP.TEMP';
        str.change = ['lmp.in.sed.' int2str(iseed)];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'lmp_temp';
        str.change = ['lmp' int2str(iseed)];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    
    str.cmd5 = ['lmp.sh.temp > ./' int2str(nmd.seed.alloy) '/lmp' int2str(iseed) '.sh'];
    
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5];
    
        system(str.cmd);
               
%lmp.in.sed.iseed
        str.orig = 'IN.X0';
        str.change = ['lmp.in.x0.' int2str(nmd.seed.alloy)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LMP_TEMP';
        str.change = ['lmp.in.sed.' int2str(iseed)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'IX0';
        str.change = [int2str(nmd.seed.alloy)];
        str.cmd3 = ['-e ''s/\' str.orig '>/' str.change '/g'' '];
        str.orig = 'ISEED_TMP';
        str.change = [int2str(iseed)];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'SEED_TMP';
        str.change = [int2str(iseed) int2str(iseed) int2str(iseed)...
            int2str(iseed) int2str(iseed)];
        str.cmd5 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'T_STEP';
        str.change = [num2str(nmd.t_step)];
        str.cmd6 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'T_FFT';
        str.change = [num2str(nmd.t_fft)];
        str.cmd7 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'T_TOTAL';
        str.change = [num2str(nmd.t_total)];
        str.cmd8 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
        str.cmd9 =...
            ['lmp.in.sed.temp > ./' int2str(nmd.seed.alloy) '/lmp.in.sed.' int2str(iseed)];
        
        str.cmd =...
            ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5...
            str.cmd6 str.cmd7 str.cmd8 str.cmd9 ];       
        system(str.cmd);
        
 	%lmp_submit.sh------------------------------------------------------------- 
    output =...
        ['qsub -l walltime=' int2str(nmd.walltime.lammps)...
        ':00:00 -l nodes=1:ppn=' int2str(nmd.cpu.lammps)...
        ' lmp' int2str(iseed) '.sh'];
    
        
    
    str.write = strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/lmp_submit.sh');
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
%GULP
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------

nmd.kptmaster = bsxfun(@times,nmd.kpt.cart,[nmd.Nx nmd.Ny nmd.Nz]);
nmd.NUM_KPTS = size(nmd.kptmaster(:,1:3),1);
nmd.kptmaster_index = 1:nmd.NUM_KPTS;

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

%Define number of atoms
nmd.NUM_ATOMS =size( nmd.x0,1); nmd.NUM_ATOMS_UCELL = size(nmd.x.cart,1); 
nmd.NUM_UCELL_COPIES=nmd.NUM_ATOMS/nmd.NUM_ATOMS_UCELL; 
nmd.NUM_MODES = 3*nmd.NUM_ATOMS_UCELL;
%Define finite difference increment
		dk = 10E-5;
%--------------------------------------------------------------------------
%-------------------------KPTS---------------------------------------------
%--------------------------------------------------------------------------

if exist(['./' int2str(nmd.seed.alloy) '/eigvec.dat'], 'file')~=0
    system(['rm -f ./' int2str(nmd.seed.alloy) '/eigvec.dat']);
    system(['rm -f ./' int2str(nmd.seed.alloy) '/freq.dat']);
    system(['rm -f ./' int2str(nmd.seed.alloy) '/vel.dat']);
end    

for ikpt=1:size(nmd.kptmaster,1)
    nmd.kptmaster(ikpt,:)           

    kpt = nmd.kptmaster(ikpt,:)./[nmd.Nx nmd.Ny nmd.Nz];

    eigvec = gulp_lj_eig(kpt,nmd.NUM_ATOMS_UCELL);
    freq = gulp_lj_freq(kpt,nmd.NUM_ATOMS_UCELL);
    vel = gulp_lj_vel(kpt,nmd.NUM_ATOMS_UCELL);

%Output formatted eigvec		
    str.write=strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/eigvec.dat');
    dlmwrite(str.write,eigvec,'-append','delimiter',' ');
%Output formatted freqs
    str.write=strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/freq.dat');
    dlmwrite(str.write,freq,'-append','delimiter',' ');
%Output velocities
    str.write=strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/vel.dat');
    dlmwrite(str.write,vel,'-append','delimiter',' ');
    
end

nmd.eigvec =...
    dlmread(strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/eigvec.dat'));
nmd.freq =...
    dlmread(strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/freq.dat'));
nmd.vel =...
    dlmread(strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/vel.dat'));

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%GULP
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

slice_length = size(nmd.kptmaster,1)/nmd.NUM_KSLICES;
% remainder_length = size(nmd.kptlist,1) - slice_length*(nmd.NUM_KSLICE-1);
for ikslice = 1:nmd.NUM_KSLICES
    nmd.kptlist(:,1:3,ikslice) =...
        nmd.kptmaster( (ikslice-1)*slice_length+1:(ikslice)*slice_length,1:3);
    nmd.kpt_index(:,ikslice) =...
        nmd.kptmaster_index( (ikslice-1)*slice_length+1:(ikslice)*slice_length);
end

%MAKES JOB FILES-----------------------------------------------------------

system(...
    strcat('cp ./nmd_submit.sh.temp ./',int2str(nmd.seed.alloy),'/nmd_submit.sh'));

for iseed=1:size(nmd.seed.initial,2)

    for ikslice = 1:nmd.NUM_KSLICES
%nmd_ISEED_IKSLICE.sh------------------------------------------------------        
        str.orig = 'nmd_temp';
        str.change = ['nmd_' int2str(iseed) '_' int2str(ikslice)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'runpath';
        str.change = strcat(nmd.str.main,'/',int2str(nmd.seed.alloy) );
        str.temp = strcat('-e ''s|',str.orig,'|',str.change);
        str.cmd2 = [str.temp '|g'' '];
        str.orig = 'nmd_TEMP.m';
        str.change = ['nmd_' int2str(iseed) '_' int2str(ikslice) '.m'];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.cmd4 =...
        ['nmd.sh.temp > ./' int2str(nmd.seed.alloy) '/nmd_' int2str(iseed) '_' int2str(ikslice) '.sh'];
    
    
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4];
        system(str.cmd);
%nmd_ISEED_IKSLICE.m-------------------------------------------------------        
        str.orig = 'ISEED';
        str.change = [int2str(iseed)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'IKSLICE';
        str.change = [int2str(ikslice)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.cmd3 = ['nmd.m.temp > ./' int2str(nmd.seed.alloy) '/nmd_' int2str(iseed) '_' int2str(ikslice) '.m'];
        str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3];
        system(str.cmd);
%nmd_submit.sh------------------------------------------------------------- 
    output =...
        ['qsub -l walltime=' int2str(nmd.walltime.matlab)...
        ':00:00,nodes=' int2str(nmd.cpu.matlab)...
        ',mem=' int2str(nmd.mem.matlab)...
        'gb nmd_' int2str(iseed) '_' int2str(ikslice) '.sh'];
    
    str.write = strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/nmd_submit.sh');
    dlmwrite(str.write,output,'-append','delimiter','');

    end
end


%nmd_grep.m-------------------------------------------------------        
    str.cmd3 = ['nmd.m.temp > ./' int2str(nmd.seed.alloy) '/nmd_' int2str(iseed) '_' int2str(ikslice) '.m'];
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3];
    system(str.cmd);


system(strcat('cp ./nmd_grep.m.temp ./',int2str(nmd.seed.alloy),'/nmd_grep.m'));

%SAVE nmd structure--------------------------------------------------------  

save(strcat(nmd.str.main,'/',int2str(nmd.seed.alloy),'/nmd.mat'), '-struct', 'nmd');

size(nmd.freq)


% str(1,:) = 'qdel 515042'; cnt=2;
% for i=515042:515121
% str(cnt,:) = ['qdel ' int2str(i)]; cnt = cnt+1;
% end
