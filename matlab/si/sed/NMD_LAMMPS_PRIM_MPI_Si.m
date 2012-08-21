
%--------------INPUT-------------------------------------------------------
kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
hbar = 1.054E-34;                %J/s
i = sqrt(-1);

mass_Si = 28.0855/(6.022 * 10^23)/(10^3);   %kg
ps2s = 10E11;
ang2m = 10E-11;

%MPI
%NUM_PROC = 2;
%if matlabpool('size') == 0 % checking to see if my pool is already open
%    matlabpool open local 8
%end

%KPTLIST: Load kpt list
    %Windoze
    str_main=strcat('E:\CMU\work\Si\SED\500K\4x\');
    str_read=strcat(str_main,'NMD\0.375\kptlist.dat');
    %Linux
    %str_main=strcat('/home/jason/lammps/Si/500K/4x/');
    %str_read=strcat(str_main,'NMD/prim/kptlist.dat');
    SED.kptlist(:,1:3) = load(str_read); [NUM_KPTS, blank] = size(SED.kptlist(:,1:3));
    SED.kptlist(:,1:3) = SED.kptlist(:,1:3);
%INITIAL POSITIONS: Set initial positions for id matching
    %Windoze
    str_read=strcat(str_main,'NMD\0.375\x0.data');
    %Linux
    %str_read=strcat(str_main,'NMD/prim/x0.data');
    x0 = load(str_read);
    %Define number of atoms
    NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL; 
    NUM_UCELL_INX = (NUM_ATOMS/8)^(1/3);
    %Define box size and conventional cell lattice parameters
    L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); alat = L/( NUM_UCELL_INX );
    %chop off first line of input structure
    x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
    %Windoze
    str_read=strcat(str_main,'NMD\0.375\SED_param.dat');
    %Linux
    %str_read=strcat(str_main,'NMD/prim/SED_param.dat');
    SEDparam = load(str_read);
    N_wmax = SEDparam(1); N_wstep = SEDparam(2); t_total = SEDparam(3); t_step = SEDparam(4);
    dt = SEDparam(5);  NUM_FFTS = SEDparam(6); NUM_SEED = SEDparam(7);
    w_step = 2*pi/(t_total*dt/ps2s); w_max = 2*pi/(t_step*dt*2/ps2s);
    NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step);  
    NUM_MODES = NUM_ATOMS_UCELL*3;
    
    
%Lattice Vectors
a1 = [alat(1) alat(1) 0]; a2 = [alat(1) 0 alat(1)]; a3 = [0 alat(1) alat(1)];

b1 = [-1 1 1]; b2 = [1 -1 1]; b3 = [1 1 -1];
            
for iseed = 1:NUM_SEED
    
    SED.SED(1:NUM_KPTS,1:(NUM_TSTEPS/2),1:NUM_MODES) = 0.0;
    %Loop over FFT Blocks
    for ifft = 1:NUM_FFTS
            %Initialize SED object to do averaging over seeds
            
%VElOCITIES
        str_read=strcat(str_main,'dump_',int2str(iseed),'_',int2str(ifft),'.vel');
        fid=fopen(str_read);
        dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
        fclose(fid);
%POSITIONS
        str_read=strcat(str_main,'dump_',int2str(iseed),'_',int2str(ifft),'.pos');
        fid=fopen(str_read);
        dummy2 = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--');  
        fclose(fid);
%Store velocity data of all atoms: subtract off the last time step
        posx = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        posy = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        posz = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        posx_temp = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        posy_temp = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        posz_temp = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        velx = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        vely = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        velz = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        velx_temp = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        vely_temp = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        velz_temp = zeros(1:NUM_ATOMS,1:NUM_TSTEPS);
        leng_dummy = length(dummy{1});
%time the velocity loop
        tic
            for iatom = 1:NUM_ATOMS  
                velx(iatom,1:NUM_TSTEPS) = dummy{1}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS))*ang2m*ps2s;
                vely(iatom,1:NUM_TSTEPS) = dummy{2}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS))*ang2m*ps2s;
                velz(iatom,1:NUM_TSTEPS) = dummy{3}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS))*ang2m*ps2s;
                posx(iatom,1:NUM_TSTEPS) = ( dummy2{1}(iatom:NUM_ATOMS:(length(dummy2{1}(:))-NUM_ATOMS))  - x0(iatom,3) )*ang2m;
                posy(iatom,1:NUM_TSTEPS) = ( dummy2{2}(iatom:NUM_ATOMS:(length(dummy2{1}(:))-NUM_ATOMS))  - x0(iatom,4) )*ang2m;
                posz(iatom,1:NUM_TSTEPS) = ( dummy2{3}(iatom:NUM_ATOMS:(length(dummy2{1}(:))-NUM_ATOMS))  - x0(iatom,5) )*ang2m;
                posx_temp(iatom,1:NUM_TSTEPS) = dummy2{1}(iatom:NUM_ATOMS:(length(dummy2{1}(:))-NUM_ATOMS));
                posy_temp(iatom,1:NUM_TSTEPS) = dummy2{2}(iatom:NUM_ATOMS:(length(dummy2{2}(:))-NUM_ATOMS));
                posz_temp(iatom,1:NUM_TSTEPS) = dummy2{3}(iatom:NUM_ATOMS:(length(dummy2{3}(:))-NUM_ATOMS));
                
            end
        toc
%SWAP THE CORRECT UNIT CELL COORDINATES       
%         x0_temp = x0;
%SWAP FOR PRIM CELL        
%         x0(6:8:NUM_ATOMS,:) = x0_temp(7:8:NUM_ATOMS,:);
%         x0(7:8:NUM_ATOMS,:) = x0_temp(6:8:NUM_ATOMS,:);
% 
%        velx = velx_temp;
%         
%        velx(6:8:NUM_ATOMS,1:NUM_TSTEPS) = velx_temp(7:8:NUM_ATOMS,1:NUM_TSTEPS);
%        velx(7:8:NUM_ATOMS,1:NUM_TSTEPS) = velx_temp(6:8:NUM_ATOMS,1:NUM_TSTEPS);
%        velx(6:8:NUM_ATOMS,1:NUM_TSTEPS) = velx_temp(7:8:NUM_ATOMS,1:NUM_TSTEPS);
%        velx(7:8:NUM_ATOMS,1:NUM_TSTEPS) = velx_temp(6:8:NUM_ATOMS,1:NUM_TSTEPS);
%  
%        vely = vely_temp;
%        
%        vely(6:8:NUM_ATOMS,1:NUM_TSTEPS) = vely_temp(7:8:NUM_ATOMS,1:NUM_TSTEPS);
%        vely(7:8:NUM_ATOMS,1:NUM_TSTEPS) = vely_temp(6:8:NUM_ATOMS,1:NUM_TSTEPS);
% 
%        velz = velx_temp;
% 
%        velz(6:8:NUM_ATOMS,1:NUM_TSTEPS) = velz_temp(7:8:NUM_ATOMS,1:NUM_TSTEPS);
%        velz(7:8:NUM_ATOMS,1:NUM_TSTEPS) = velz_temp(6:8:NUM_ATOMS,1:NUM_TSTEPS);
% 
%        posx = posx_temp;
%        
%        posx(6:8:NUM_ATOMS,1:NUM_TSTEPS) = posx_temp(7:8:NUM_ATOMS,1:NUM_TSTEPS);
%        posx(7:8:NUM_ATOMS,1:NUM_TSTEPS) = posx_temp(6:8:NUM_ATOMS,1:NUM_TSTEPS);
%       
%        posy = posy_temp;
%        
%        posy(6:8:NUM_ATOMS,1:NUM_TSTEPS) = posy_temp(7:8:NUM_ATOMS,1:NUM_TSTEPS);
%        posy(7:8:NUM_ATOMS,1:NUM_TSTEPS) = posy_temp(6:8:NUM_ATOMS,1:NUM_TSTEPS);
% 
%        posz = posz_temp;
%        
%        posz(6:8:NUM_ATOMS,1:NUM_TSTEPS) = posz_temp(7:8:NUM_ATOMS,1:NUM_TSTEPS);
%        posz(7:8:NUM_ATOMS,1:NUM_TSTEPS) = posz_temp(6:8:NUM_ATOMS,1:NUM_TSTEPS);
           
%Set mass array
        m = repmat(x0(:,2),1,NUM_TSTEPS) * mass_Si;                
%Remove dummy
        %clear dummy dummy2                
%EIGENVECTORS
        %Windoze
            str_read=strcat(str_main,'NMD\0.375\eigvec.dat');
        %Linux
            %str_read=strcat(str_main,'NMD/prim/eigvec.dat');
        eigenvec = dlmread(str_read);
%FREQUENCIES
        %Windoze
        str_read=strcat(str_main,'NMD\0.375\freq.dat');
        %Linux
        %str_read=strcat(str_main,'NMD/prim/freq.dat');
        
        freq = load(str_read);
                
%Zero main SED FP: this gets averaged as you loop over the NUM_FFTS
        NUM_TSTEPSdiv2 = NUM_TSTEPS/2;
        SED.SED(1:NUM_KPTS,1:(NUM_TSTEPS/2),1:NUM_MODES) = 0.0;

        Q = zeros(1,NUM_TSTEPS);
        QDOT = zeros(1,NUM_TSTEPS);
%time the main loop
        tic     
%Switch positions to location of unit cell copies

x0(2:2:NUM_ATOMS,3:5) = x0(1:2:NUM_ATOMS,3:5);

%Loop over kpts
        for imode = 1:NUM_MODES
            FP(1:NUM_KPTS,1:(NUM_TSTEPS/2)) = 0.0;
            %parfor ikpt = 1:NUM_KPTS
            for ikpt = 1:NUM_KPTS
                %K = SED.kptlist(ikpt,1)*b1 + SED.kptlist(ikpt,2)*b2 +
                %SED.kptlist(ikpt,3)*b3 ;
                spatial = i*(2*pi./alat(1))*( x0(:,3)*SED.kptlist(ikpt,1) + x0(:,4)*SED.kptlist(ikpt,2) + x0(:,5)*SED.kptlist(ikpt,3) );
                spatial = repmat(spatial,1,NUM_TSTEPS);
%Prepare the eigenvectors to be eigx,y,z(1:NUM_ATOMS,xyz), per mode
%WARNING: for CONV, must use :3:, where PRIM has :1: (: implicit)
                eigx = repmat(conj(eigenvec(((NUM_ATOMS_UCELL*3)*(ikpt-1)+1):3:((NUM_ATOMS_UCELL*3)*ikpt),imode)),NUM_UCELL_COPIES,NUM_TSTEPS);
                eigy = repmat(conj(eigenvec(((NUM_ATOMS_UCELL*3)*(ikpt-1)+2):3:((NUM_ATOMS_UCELL*3)*ikpt),imode)),NUM_UCELL_COPIES,NUM_TSTEPS);
                eigz = repmat(conj(eigenvec(((NUM_ATOMS_UCELL*3)*(ikpt-1)+3):3:((NUM_ATOMS_UCELL*3)*ikpt),imode)),NUM_UCELL_COPIES,NUM_TSTEPS);               
                Q = sum( (sqrt(m/NUM_UCELL_COPIES)).*( posx(:,1:NUM_TSTEPS).*eigx + posy(:,1:NUM_TSTEPS).*eigy + posz(:,1:NUM_TSTEPS).*eigz ).*(exp(spatial)) , 1);
                QDOT = sum( (sqrt(m/NUM_UCELL_COPIES)).*( velx(:,1:NUM_TSTEPS).*eigx + vely(:,1:NUM_TSTEPS).*eigy + velz(:,1:NUM_TSTEPS).*eigz ).*(exp(spatial)) , 1 );
                PEXCORR = xcorr(Q,'coeff');
                KEXCORR = xcorr(QDOT,'coeff');
                PEFFT = real(fft(PEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1))).^2 + imag(fft(PEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1))).^2;
                KEFFT = real(fft(KEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1))).^2 + imag(fft(KEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1))).^2;
                CROSSFFT = real(fft(PEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1))).*real(fft(KEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1))) +  imag(fft(PEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1))).*imag(fft(KEXCORR(NUM_TSTEPS:NUM_TSTEPS*2-1)));
            FP(ikpt,:) = PEFFT(1:(NUM_TSTEPS/2)) + KEFFT(1:(NUM_TSTEPS/2)) + 2*CROSSFFT(1:(NUM_TSTEPS/2));
            pause
            end %END ikpt
            pause
           SED.SED(:,:,imode) = FP(:,:);
        end %END imode
            toc  
    pause
    end %END ifft
%Average over FFTS
        SED.SED = SED.SED/NUM_FFTS;
%Define frequencies
        omega = (1:NUM_OMEGAS)*(w_max/NUM_OMEGAS);
        %semilogy(omega,SED.SED(1,:,1),omega,SED.SED(1,:,2),omega,SED.SED(1,:,3),omega,sum(SED.SED(1,:,:),3))
%Output SED
    %Windoze
        %str_write_total=strcat(str_main,'NMD\prim\SED_',int2str(iseed),'.txt');
        %str_write_single=strcat(str_main,'NMD\prim\SED_single_',int2str(iseed),'.txt');
	%Linux
        str_write_total=strcat(str_main,'NMD/0.375/SED_',int2str(iseed),'.txt');
        str_write_single=strcat(str_main,'NMD/0.375/SED_single_',int2str(iseed),'.txt');
%Sum outer loops over NUM_ATOMS_UCELL and xyz
        for ikpt = 1:NUM_KPTS
            dlmwrite(str_write_total,SED.kptlist(ikpt,1:3),'-append','delimiter',' ');
            output(1:length(omega),1) = omega;
            output(1:length(omega),2) = sum(SED.SED(ikpt,:,:),3);
            dlmwrite(str_write_total,output,'-append','delimiter',' ');
            %for imode = 1:NUM_MODES
            clear output
            dlmwrite(str_write_single,SED.kptlist(ikpt,1:3),'-append','delimiter',' ');
            output(1:length(omega),1) = omega;
            output(1:length(omega),2:(NUM_MODES+1)) = SED.SED(ikpt,:,:);
            dlmwrite(str_write_single,output,'-append','delimiter',' ');
            clear output
            %end %END imode
        end %END ikpt
        
end %END iseed

%matlabpool close