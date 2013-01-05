%--------------INPUT-------------------------------------------------------

%LJ Potential and Material Parameters
epsilon_Ar = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
sigma_Ar = 3.4E-10;                 %Angstroms 3.4E-10 meters
a_0 = 5.2686E-10/sigma_Ar;            %the lattice constant of Ar: http://www.infoplease.com/periodictable.php?id=18
mass_Ar = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
%mass_Ar = mass_Ar/mass_Ar;
tau_Ar = sqrt((mass_Ar*(sigma_Ar^2))/epsilon_Ar);
kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
hbar = 1.054E-34;                %J/s
i = sqrt(-1);

%KPTLIST: Load kpt list

            %Windoze
            str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\perfect\SED\8x\1K\NMD\');
            str_read=strcat(str_main,'4atom\kptlist.dat');
            %Linux
            %str_main=strcat('/home/jason/lammps/LJ/crystal/perfect/SED/8x/');
            %str_read=strcat(str_main,'4atom/toucell/kptlist.dat');
            SED.kptlist(:,1:3) = load(str_read); [NUM_KPTS, blank] = size(SED.kptlist(:,1:3));
            %INITIAL POSITIONS: Set initial positions for id matching
            %Windoze
            str_read=strcat(str_main,'4atom\x0.data');
            %Linux
            %str_read=strcat(str_main,'4atom/toucell/x0.data');
            x0 = load(str_read);
            %Define number of atoms
            NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL; 
            %Define box size and conventional cell lattice parameters
            L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); alat = L/(NUM_UCELL_COPIES^(1/3));
            NUM_SUPERCELL = L/alat;
            %chop off first line of input structure
            x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
            %Windoze
            str_read=strcat(str_main,'4atom\SED_param.dat');
            %Linux
            %str_read=strcat(str_main,'4atom/toucell/SED_param.dat');
            SEDparam = load(str_read);
            N_wmax = SEDparam(1); N_wstep = SEDparam(2); t_total = SEDparam(3); t_step = SEDparam(4);
            dt = SEDparam(5);  NUM_FFTS = SEDparam(6); NUM_SEED = SEDparam(7);
            w_step = 2*pi/(t_total*dt); w_max = 2*pi/(t_step*dt*2);
            NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step); 
  
            
%Output SED
        %Windoze
        str_write_vec=strcat(str_main,'4atom\eigvec.dat');
        str_write_freq=strcat(str_main,'4atom\freq.dat');
        str_write_vel=strcat(str_main,'4atom\vel.dat');
        
        %Linux
        %str_write=strcat(str_main,'1atom/SED_',int2str(iseed),'.txt');          

            for ikpt = 1:NUM_KPTS
                [freq,eigVsorted,velocity] = LDAr_FCC(alat,SED.kptlist(ikpt,1:3)/NUM_SUPERCELL);
                    dlmwrite(str_write_vec,eigVsorted,'-append','delimiter',' ');
                    dlmwrite(str_write_freq,freq,'-append','delimiter',' ');
                    dlmwrite(str_write_vel,velocity,'-append','delimiter',' ');
            end
            
            

            