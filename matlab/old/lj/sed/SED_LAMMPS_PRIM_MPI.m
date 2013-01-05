
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

%MPI
NUM_PROC = 2;
if matlabpool('size') == 0 % checking to see if my pool is already open
    matlabpool open local 8
end

%KPTLIST: Load kpt list
            %Windoze
            str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\perfect\SED\32\');
            str_read=strcat(str_main,'1atom\kptlist.dat');
            %Linux
            %str_main=strcat('/home/jason/lammps/LJ/crystal/perfect/SED/6x/');
            %str_read=strcat(str_main,'1atom/kptlist.dat');
            SED.kptlist(:,1:3) = load(str_read); [NUM_KPTS, blank] = size(SED.kptlist(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
            %Windoze
            str_read=strcat(str_main,'1atom\x0.data');
            %Linux
            %str_read=strcat(str_main,'1atom/x0.data');
            x0 = load(str_read);
            %Define number of atoms
            NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL; 
            NUM_UCELL_INX = (NUM_ATOMS/4)^(1/3);
            %Define box size and conventional cell lattice parameters
            L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); alat = L/( NUM_UCELL_INX );
            %chop off first line of input structure
            x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
            %Windoze
            str_read=strcat(str_main,'1atom\SED_param.dat');
            %Linux
            %str_read=strcat(str_main,'1atom/SED_param.dat');
            SEDparam = load(str_read);
            N_wmax = SEDparam(1); N_wstep = SEDparam(2); t_total = SEDparam(3); t_step = SEDparam(4);
            dt = SEDparam(5);  NUM_FFTS = SEDparam(6); NUM_SEED = SEDparam(7);
            w_step = 2*pi/(t_total*dt); w_max = 2*pi/(t_step*dt*2);
            NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step);  
            
for iseed = 1:NUM_SEED
    %Loop over FFT Blocks
    for ifft = 1:NUM_FFTS
            %Initialize SED object to do averaging over seeds
                str_read=strcat(str_main,'dump_',int2str(iseed),'_',int2str(ifft),'.vel');
                fid=fopen(str_read);
                dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
            %Store velocity data of all atoms: subtract off the last time step
                vel = zeros(1:NUM_ATOMS,1:NUM_TSTEPS,1:3);
                leng_dummy = length(dummy{1});
%time the velocity loop
                tic
                    for iatom = 1:NUM_ATOMS  
                        vel(iatom,:,1) = dummy{1}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));
                        vel(iatom,:,2) = dummy{2}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));
                        vel(iatom,:,3) = dummy{3}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));
                    end
                toc
%Remove dummy
                clear dummy           
%Zero main SED FP: this gets averaged as you loop over the NUM_FFTS
            NUM_TSTEPSdiv2 = NUM_TSTEPS/2;
            FP(1:NUM_KPTS,1:(NUM_TSTEPS/2)) = 0.0;
            FP_TEMPX(1:NUM_KPTS,1:(NUM_TSTEPS)) = 0.0; 
            FP_TEMPY(1:NUM_KPTS,1:(NUM_TSTEPS)) = 0.0; 
            FP_TEMPZ(1:NUM_KPTS,1:(NUM_TSTEPS)) = 0.0;
%time the main loop
            tic     
            %Loop over kpts
            parfor ikpt = 1:NUM_KPTS
                for iaucell = 1:NUM_ATOMS_UCELL
                    velfftx(1:NUM_TSTEPS) = 0.0; velffty(1:NUM_TSTEPS) = 0.0; velfftz(1:NUM_TSTEPS) = 0.0; 
                    veltemp(1:NUM_TSTEPS) = 0.0;
                    for iucell = 1:NUM_UCELL_COPIES
                        spatial = i*dot( x0(NUM_ATOMS_UCELL*(iucell-1)+1,3:5) , (2*pi./alat(1)).*(SED.kptlist(ikpt,1:3)) );
                        veltemp = exp(spatial)*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),:,1);
                        FP_TEMPX(ikpt,:) = FP_TEMPX(ikpt,:) + veltemp;
                        veltemp = exp(spatial)*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),:,2);
                        FP_TEMPY(ikpt,:) = FP_TEMPY(ikpt,:) + veltemp;
                        veltemp = exp(spatial)*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),:,3);
                        FP_TEMPZ(ikpt,:) = FP_TEMPZ(ikpt,:) + veltemp;
                    end
                %Sum over directions x y z
                velfftx = fft(FP_TEMPX(ikpt,:));
                FP(ikpt,:) = FP(ikpt,:) + x0(iaucell,2)*( real(velfftx(1:NUM_TSTEPSdiv2)).^2 + imag(velfftx(1:NUM_TSTEPSdiv2)).^2 );
                velffty = fft(FP_TEMPY(ikpt,:));
                FP(ikpt,:) = FP(ikpt,:) + x0(iaucell,2)*( real(velffty(1:NUM_TSTEPSdiv2)).^2 + imag(velffty(1:NUM_TSTEPSdiv2)).^2 );
                velfftz = fft(FP_TEMPZ(ikpt,:));
                FP(ikpt,:) = FP(ikpt,:) + x0(iaucell,2)*( real(velfftz(1:NUM_TSTEPSdiv2)).^2 + imag(velfftz(1:NUM_TSTEPSdiv2)).^2 ); 
                end
                %f(FP(ikpt,:)) = FPTEMP(:);
            end %END ikpt
            toc
%Include prefactors
            FP = FP/(4*pi*(NUM_TSTEPS*dt/2)*NUM_UCELL_COPIES);            
    end %END ifft
        %Average over FFTS
        FP = FP/NUM_FFTS;
        %Define frequencies
        omega = (1:NUM_OMEGAS)*(w_max/NUM_OMEGAS);
        %semilogy(omega,FP(1,:))
        
%Output SED
        %Windoze
        str_write=strcat(str_main,'1atom\SED_',int2str(iseed),'.txt');
        %Linux
        %str_write=strcat(str_main,'1atom/SED_',int2str(iseed),'.txt');
                %Sum outer loops over NUM_ATOMS_UCELL and xyz
                for ikpt = 1:NUM_KPTS
                    dlmwrite(str_write,SED.kptlist(ikpt,1:3),'-append','delimiter',' ');
                    output(1:length(omega),1) = omega;
                    output(1:length(omega),2) = FP(ikpt,:);
                    dlmwrite(str_write,output,'-append','delimiter',' ');
                end %END ikpt
        
end %END iseed

matlabpool close
