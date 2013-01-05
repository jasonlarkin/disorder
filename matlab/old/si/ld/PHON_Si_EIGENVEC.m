
%--------------------------------------------------------------------------
%-------------------------INPUT--------------------------------------------
%--------------------------------------------------------------------------
constant.kb = 1.3806E-23;           %aJ/k (1.3806E-23 J/K)
constant.hbar = 1.054E-34;          %J/s
constant.i = sqrt(-1);
constant.c = 29979245800.00019;     %cm/s
constant.s2ps = 10E11;
constant.ang2m = 10E-11;
constant.mass_Si = 28.0855/(6.022 * 10^23)/(10^3);   %kg
constant.eV2J = 1.60217646E-19;
constant.Ang2m = 1E10;

%--------------------------------------------------------------------------
            str.main=strcat('D:\CMU\work\Si\PHON\4x\');
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
            format long
%--------------------------------------------------------------------------


%KPTLIST: Load kpt list
            %Linux
            str.read=strcat(str.main,'kptlist.dat');
            LD.kptlist(:,1:3) = load(str.read); [LD.NUM_KPTS, blank] = size(LD.kptlist(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
            %Linux
            str.read=strcat(str.main,'x0.data');
            x0 = load(str.read);
%Define number of atoms
            LD.NUM_ATOMS = x0(1,1); LD.NUM_ATOMS_UCELL = x0(1,2); LD.NUM_UCELL_COPIES=LD.NUM_ATOMS/LD.NUM_ATOMS_UCELL; 
	     LD.NUM_MODES = 3*LD.NUM_ATOMS_UCELL;
%Define box size and conventional cell lattice parameters
            LD.L(1) = x0(1,3); LD.L(2) = x0(1,4); LD.L(3) = x0(1,5); LD.alat = LD.L/((LD.NUM_ATOMS/8)^(1/3));
            LD.NUM_SUPERCELL = LD.L/LD.alat;
            %chop off first line of input structure
            LD.x0 = x0(2:length(x0),:);
            LD.m = x0(:,2)*constant.mass_Si;
%Define finite difference increment
		LD.dk = 0.000001;

%READ FORCE CONSTANTS
            %Linux
            str.read=strcat(str.main,'HARMONIC_MATLAB');
            dummy = dlmread(str.read);        
            for i1=1:(length(dummy)/5)
                FC.id(i1,1) = dummy( (i1-1)*5+1,1);         %i
                FC.id(i1,2) = dummy( (i1-1)*5+1,2);         %j
                FC.vec(i1,1:3) = dummy( (i1-1)*5+2,1:3);    %rij
                FC.FC(i1,1:3,1:3) = dummy( (i1-1)*5+3 : (i1-1)*5+5,1:3)*(constant.eV2J)*(constant.Ang2m^2)  ;      %eV/Ang^2
            end

%Calculate freq, eigv, and vel
            
    %gamma check   
    [freq,eigVsorted,velocity] = PHON_Si_LD(LD,FC,constant,[0 0 0]);
    freq
    eigVsorted
    velocity
    pause 

%             
% %OUTPUT
%         %Windoze
%         str_write_vec=strcat(str_main,'1atom\ME\eigvec.dat');
%         str_write_freq=strcat(str_main,'1atom\ME\freq.dat');
%         str_write_vel=strcat(str_main,'1atom\ME\vel.dat');
%         %Linux
%         %str_write=strcat(str_main,'1atom/SED_',int2str(iseed),'.txt');          
% 
%             for ikpt = 1:NUM_KPTS
%                 SED.kptlist(ikpt,1:3)
%                 [freq,eigVsorted,velocity] = PHON_Si_LD(alat,SED.kptlist(ikpt,1:3));
%                     dlmwrite(str_write_vec,eigVsorted,'-append','delimiter',' ');
%                     dlmwrite(str_write_freq,freq,'-append','delimiter',' ');
%                     dlmwrite(str_write_vel,velocity,'-append','delimiter',' ');
%             end
