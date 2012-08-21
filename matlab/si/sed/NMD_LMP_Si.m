%MPI
%if matlabpool('size') == 0 
%    matlabpool open local 6
%end

%--------------INPUT-------------------------------------------------------
cons.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
cons.hbar = 1.054E-34;                %J/s
cons.i = sqrt(-1);
cons.mass_Si = 28.0855/(6.022 * 10^23)/(10^3);   %kg
cons.ps2s = 10E11;
cons.ang2m = 10E-11;

    str.main=strcat('D:\CMU\work\Si\SED\300K\4x\');

%--------------------------------------------------------------------------
%            [status str_main]=system('pwd');
%	str_main = strcat(str_main,'/');
%--------------------------------------------------------------------------
    
%KPTLIST: Load kpt list
    %Windoze
    str.read=strcat(str.main,'NMD\conv\kptlist.dat');
    %Linux
    %str.read=strcat(str.main,'NMD/conv/kptlist.dat');
    SED.kptlist(:,1:3) = load(str.read); [SED.NUM_KPTS, blank] = size(SED.kptlist(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
    %Windoze
    str.read=strcat(str.main,'NMD\conv\x0.data');
    %Linux
    %str.read=strcat(str.main,'NMD/conv/x0.data');
    SED.x0 = load(str.read);
    %Define number of atoms
    SED.NUM_ATOMS = SED.x0(1,1); SED.NUM_ATOMS_UCELL = SED.x0(1,2); SED.NUM_UCELL_COPIES=SED.NUM_ATOMS/SED.NUM_ATOMS_UCELL; 
    SED.NUM_UCELL_INX = (SED.NUM_ATOMS)^(1/3);
    %Define box size and conventional cell lattice parameters
    SED.L(1) = SED.x0(1,3); SED.L(2) = SED.x0(1,4); SED.L(3) = SED.x0(1,5); SED.alat = SED.L/( SED.NUM_UCELL_INX );
    %chop off first line of input structure
    SED.x0 = SED.x0(2:length(SED.x0),:);
%SED PARAMETERS: load SED parameters
    %Windoze
    str.read=strcat(str.main,'NMD\conv\SED_param.dat');
    %Linux
    %str.read=strcat(str.main,'NMD/conv/SED_param.dat');
    SEDparam = load(str.read);
    SED.N_wmax = SEDparam(1); SED.N_wstep = SEDparam(2); SED.t_total = SEDparam(3); SED.t_step = SEDparam(4);
    SED.dt = SEDparam(5);  SED.NUM_FFTS = SEDparam(6); SED.NUM_SEED = SEDparam(7);
    SED.w_step = 2*pi/(SED.t_total*SED.dt/cons.ps2s); SED.w_max = 2*pi/(SED.t_step*SED.dt*2/cons.ps2s);
    SED.NUM_TSTEPS = SED.t_total/SED.t_step; SED.NUM_OMEGAS = SED.t_total/(2*SED.t_step);  
    SED.NUM_MODES = SED.NUM_ATOMS_UCELL*3;
            
for iseed = 1:SED.NUM_SEED
    
    SED.SED(1:SED.NUM_KPTS,1:(SED.NUM_TSTEPS/2),1:SED.NUM_MODES) = 0.0;
    %Loop over FFT Blocks
    for ifft = 1:SED.NUM_FFTS
            %Initialize SED object to do averaging over seeds
            
%VElOCITIES
        str.read=strcat(str.main,'dump_',int2str(iseed),'_',int2str(ifft),'.vel');
        fid=fopen(str.read); dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
        fclose(fid);
%Store velocity data of all atoms: subtract off the last time step
        SED.velx = zeros(1:SED.NUM_ATOMS,1:SED.NUM_TSTEPS);
        SED.vely = zeros(1:SED.NUM_ATOMS,1:SED.NUM_TSTEPS);
        SED.velz = zeros(1:SED.NUM_ATOMS,1:SED.NUM_TSTEPS);
%time the velocity loop
        tic
            for iatom = 1:SED.NUM_ATOMS  
                SED.velx(iatom,1:SED.NUM_TSTEPS) = dummy{1}(iatom:SED.NUM_ATOMS:(length(dummy{1}(:))...
                    -SED.NUM_ATOMS))*cons.ang2m*cons.ps2s;
                SED.vely(iatom,1:SED.NUM_TSTEPS) = dummy{2}(iatom:SED.NUM_ATOMS:(length(dummy{1}(:))...
                    -SED.NUM_ATOMS))*cons.ang2m*cons.ps2s;
                SED.velz(iatom,1:SED.NUM_TSTEPS) = dummy{3}(iatom:SED.NUM_ATOMS:(length(dummy{1}(:))...
                    -SED.NUM_ATOMS))*cons.ang2m*cons.ps2s;
            end
        toc
%Remove dummy
        clear dummy   
%Set mass array
        SED.m = repmat(SED.x0(:,2),1,SED.NUM_TSTEPS) * cons.mass_Si;                
%Zero main SED FP: this gets averaged as you loop over the NUM_FFTS
        SED.NUM_TSTEPSdiv2 = SED.NUM_TSTEPS/2;
        SED.SED(1:SED.NUM_KPTS,1:(SED.NUM_TSTEPS/2),1:SED.NUM_MODES) = 0.0;
        QDOT = zeros(1,SED.NUM_TSTEPS);
%time the main loop
        tic     
%Loop over kpts
for imode = 1:SED.NUM_MODES
	FP(1:SED.NUM_KPTS,1:(SED.NUM_TSTEPS/2)) = 0.0;
	%parfor ikpt = 1:NUM_KPTS
        for ikpt = 1:SED.NUM_KPTS
            SED.kptlist(ikpt,1:3)
	%EIGENVECTORS
            str.read=strcat(str.main,'NMD\conv\eig\eigvec_',int2str(SED.kptlist(ikpt,1))...
                ,int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.dat');
            %str.read=strcat(str.main,'NMD/conv/eig/eigvec_',int2str(SED.kptlist(ikpt,1))...
            %,int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.dat');
        	SED.eigenvec = dlmread(str.read);
	%FREQUENCIES
            str.read=strcat(str.main,'NMD\conv\freq\freq_',int2str(SED.kptlist(ikpt,1))...
                ,int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.dat');
        	%str.read=strcat(str.main,'NMD/conv/freq/freq_',int2str(SED.kptlist(ikpt,1))...
            %,int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.dat');
        	SED.freq = load(str.read);
    %SPATIAL        
                SED.spatial = i*( SED.x0(:,3)*(2*pi./SED.L(1)).*(SED.kptlist(ikpt,1)) ...
                    + SED.x0(:,4)*(2*pi./SED.L(1)).*(SED.kptlist(ikpt,2)) ...
                    + SED.x0(:,5)*(2*pi./SED.L(1)).*(SED.kptlist(ikpt,3)) );
                SED.spatial = repmat(SED.spatial,1,SED.NUM_TSTEPS);
%Prepare the eigenvectors to be eigx,y,z(1:NUM_ATOMS,xyz), per mode
%WARNING: for CONV, must use :3:, where PRIM has :1: (: implicit)

                SED.eigx = repmat(conj(SED.eigenvec(1:3:SED.NUM_ATOMS_UCELL*3,imode)),...
                    SED.NUM_UCELL_COPIES,SED.NUM_TSTEPS);
                SED.eigy = repmat(conj(SED.eigenvec(2:3:SED.NUM_ATOMS_UCELL*3,imode)),...
                    SED.NUM_UCELL_COPIES,SED.NUM_TSTEPS);
                SED.eigz = repmat(conj(SED.eigenvec(3:3:SED.NUM_ATOMS_UCELL*3,imode)),...
                    SED.NUM_UCELL_COPIES,SED.NUM_TSTEPS);

                SED.QDOT = sum((sqrt(SED.m/SED.NUM_UCELL_COPIES)).*( SED.velx(:,1:SED.NUM_TSTEPS).*SED.eigx ...
                    + SED.vely(:,1:SED.NUM_TSTEPS).*SED.eigy + SED.velz(:,1:SED.NUM_TSTEPS).*SED.eigz ).*(exp(SED.spatial)) , 1 );
                KEXCORR = xcorr(SED.QDOT,'coeff');
                KEFFT = real(fft(KEXCORR(SED.NUM_TSTEPS:SED.NUM_TSTEPS*2-1))).^2 + imag(fft(KEXCORR(SED.NUM_TSTEPS:SED.NUM_TSTEPS*2-1))).^2;
            FP(ikpt,:) = KEFFT(1:(SED.NUM_TSTEPS/2)) ;
        end %END ikpt
        
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
        
           SED.SED(:,:,imode) = FP(:,:);
        end %END imode
            toc  
    end %END ifft
%Average over FFTS
        SED.SED = SED.SED/SED.NUM_FFTS;
%Define frequencies
        SED.omega = (1:SED.NUM_OMEGAS)*(SED.w_max/SED.NUM_OMEGAS);
        semilogy(SED.omega,sum(SED.SED(1,:,:),3))
       
        
%OUTPUT SED
        for ikpt = 1:SED.NUM_KPTS
            %Windoze
		    str.write_total=strcat(str.main,'NMD\conv\SED_',int2str(iseed),'_',...
                int2str(SED.kptlist(ikpt,1)),int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.txt');
        	str.write_single=strcat(str.main,'NMD\conv\SED_single_',int2str(iseed),'_',...
                int2str(SED.kptlist(ikpt,1)),int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.txt');
            %Linux
        	%str_write_total=strcat(str_main,'NMD/conv/SED_',int2str(iseed),'_',...
            %int2str(SED.kptlist(ikpt,1)),int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.txt');
        	%str_write_single=strcat(str_main,'NMD/conv/SED_single_',int2str(iseed),'_',...
            %int2str(SED.kptlist(ikpt,1)),int2str(SED.kptlist(ikpt,2)),int2str(SED.kptlist(ikpt,3)),'.txt');
            dlmwrite(str.write_total,SED.kptlist(ikpt,1:3),'-append','delimiter',' ');
            output(1:length(SED.omega),1) = SED.omega;
            output(1:length(SED.omega),2) = sum(SED.SED(ikpt,:,:),3);
            dlmwrite(str.write_total,output,'-append','delimiter',' ');
            clear output
            output(1:length(SED.omega),1) = SED.omega;
            output(1:length(SED.omega),2:(SED.NUM_MODES+1)) = SED.SED(ikpt,:,:);
            dlmwrite(str_write_single,output,'-append','delimiter',' ');
            clear output
            %end %END imode
        end %END ikpt
        
end %END iseed

matlabpool close