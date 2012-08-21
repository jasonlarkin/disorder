
%function SEDfit_JL_040711

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
            str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\perfect\SED\8\');
            str_read=strcat(str_main,'4atom\toucell\kptlist.dat');
            SED.kptlist(:,1:3) = load(str_read); [NUM_KPTS, blank] = size(SED.kptlist(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
            str_read=strcat(str_main,'4atom\toucell\x0.data');
            x0 = load(str_read);
            %Define number of atoms
            NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL; 
            %Define box size and conventional cell lattice parameters
            L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); alat = L/(NUM_UCELL_COPIES^(1/3));
            %chop off first line of input structure
            x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
            str_read=strcat(str_main,'4atom\toucell\SED_param.dat');
            SEDparam = load(str_read);
            N_wmax = SEDparam(1); N_wstep = SEDparam(2); t_total = SEDparam(3); t_step = SEDparam(4);
            dt = SEDparam(5);  NUM_FFTS = SEDparam(6); NUM_SEED = SEDparam(7);
            w_step = 2*pi/(t_total*dt); w_max = 2*pi/(t_step*dt*2);
            NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step);  
for iseed = 1:NUM_SEED
            
    %Loop over FFT Blocks

    %Zero main SED FP: this gets averaged as you loop over the NUM_FFTS
                FP(1:NUM_KPTS,1:(NUM_TSTEPS/2)) = 0.0;

    for ifft = 1:NUM_FFTS

            %Initialize SED object to do averaging over seeds
                str_read=strcat(str_main,int2str(iseed),'\dump_',int2str(iseed),'_',int2str(ifft),'.vel');
                fid=fopen(str_read);
                dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 

            %Store velocity data of all atoms: subtract off the last time step
                vel = zeros(1:NUM_ATOMS,1:NUM_TSTEPS,1:3);
                    for iatom = 1:NUM_ATOMS  
                        vel(iatom,1:NUM_TSTEPS,1) = dummy{1}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));
                        vel(iatom,1:NUM_TSTEPS,2) = dummy{2}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));
                        vel(iatom,1:NUM_TSTEPS,3) = dummy{3}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));  
                    end
            %Save dummy velocity data for now
                %SED.vel = dummy;
                clear dummy

            %Zero FP_X
                FP_X(1:NUM_KPTS,1:(NUM_TSTEPS/2),1:NUM_ATOMS_UCELL) = 0.0; %IM_X(1:NUM_KPTS,1:NUM_TSTEPS,1:NUM_ATOMS_UCELL) = 0.0;
                FP_Y(1:NUM_KPTS,1:(NUM_TSTEPS/2),1:NUM_ATOMS_UCELL) = 0.0; %IM_Y(1:NUM_KPTS,1:NUM_TSTEPS,1:NUM_ATOMS_UCELL) = 0.0;
                FP_Z(1:NUM_KPTS,1:(NUM_TSTEPS/2),1:NUM_ATOMS_UCELL) = 0.0; %IM_Z(1:NUM_KPTS,1:NUM_TSTEPS,1:NUM_ATOMS_UCELL) = 0.0;

                FP_TEMPX(1:NUM_TSTEPS) = 0.0; %IM_X(1:NUM_KPTS,1:NUM_TSTEPS,1:NUM_ATOMS_UCELL) = 0.0;
                FP_TEMPY(1:NUM_TSTEPS) = 0.0; %IM_Y(1:NUM_KPTS,1:NUM_TSTEPS,1:NUM_ATOMS_UCELL) = 0.0;
                FP_TEMPZ(1:NUM_TSTEPS) = 0.0; %IM_Z(1:NUM_KPTS,1:NUM_TSTEPS,1:NUM_ATOMS_UCELL) = 0.0;

                velfft(1:NUM_TSTEPS) = 0.0; 
                veltemp(1:NUM_TSTEPS) = 0.0; 

            tic     
            %Loop over kpts
            for ikpt = 1:NUM_KPTS
                for iaucell = 1:NUM_ATOMS_UCELL
                    FP_TEMPX(1:(NUM_TSTEPS)) = 0; FP_TEMPY(1:(NUM_TSTEPS)) = 0; FP_TEMPZ(1:(NUM_TSTEPS)) = 0;
                    for iucell = 1:NUM_UCELL_COPIES
                        %x0(NUM_ATOMS_UCELL*(iucell-1)+1,3:5)
                        %SED.kptlist(ikpt,1:3)

                        spatial = i*dot( x0(NUM_ATOMS_UCELL*(iucell-1)+1,3:5)*alat(1) , (2*pi./L).*(SED.kptlist(ikpt,1:3)) );
                        %spatial = i*dot( x0(NUM_ATOMS_UCELL*(iucell-1)+iaucell,3:5) , (2*pi./L).*(SED.kptlist(ikpt,1:3)) );

                        veltemp = exp(spatial)*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),1:(NUM_TSTEPS),1);
                        FP_TEMPX(1:NUM_TSTEPS) = FP_TEMPX(1:NUM_TSTEPS) + veltemp;
                        veltemp = exp(spatial)*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),1:(NUM_TSTEPS),2);
                        FP_TEMPY(1:NUM_TSTEPS) = FP_TEMPY(1:NUM_TSTEPS) + veltemp;
                        veltemp = exp(spatial)*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),1:(NUM_TSTEPS),3);
                        FP_TEMPZ(1:NUM_TSTEPS) = FP_TEMPZ(1:NUM_TSTEPS) + veltemp;

    %                     velfft = fft(spatial*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),1:(NUM_TSTEPS),1));
    %                     FP_X(ikpt,1:(NUM_TSTEPS/2),iaucell) = FP_X(ikpt,1:(NUM_TSTEPS/2),iaucell) + velfft(1:(NUM_TSTEPS/2));
    %                     velfft = fft(spatial*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),1:(NUM_TSTEPS),2));
    %                     FP_Y(ikpt,1:(NUM_TSTEPS/2),iaucell) = FP_X(ikpt,1:(NUM_TSTEPS/2),iaucell) + velfft(1:(NUM_TSTEPS/2));
    %                     velfft = fft(spatial*vel(iaucell+NUM_ATOMS_UCELL*(iucell-1),1:(NUM_TSTEPS),3));
    %                     FP_Z(ikpt,1:(NUM_TSTEPS/2),iaucell) = FP_X(ikpt,1:(NUM_TSTEPS/2),iaucell) + velfft(1:(NUM_TSTEPS/2));
                    end
                end
                velfft = fft(FP_TEMPX(1:(NUM_TSTEPS)));
                FP_X(ikpt,1:(NUM_TSTEPS/2),iaucell) = velfft(1:(NUM_TSTEPS/2)) ;
                velfft = fft(FP_TEMPY(1:(NUM_TSTEPS)));
                FP_Y(ikpt,1:(NUM_TSTEPS/2),iaucell) = velfft(1:(NUM_TSTEPS/2)) ;
                velfft = fft(FP_TEMPZ(1:(NUM_TSTEPS)));
                FP_Z(ikpt,1:(NUM_TSTEPS/2),iaucell) = velfft(1:(NUM_TSTEPS/2)) ;
            end %END ikpt
            toc

            %Sum outer loops over NUM_ATOMS_UCELL and xyz
            for ikpt = 1:NUM_KPTS
                for iaucell = 1:NUM_ATOMS_UCELL
                    FP(ikpt,1:1:(NUM_TSTEPS/2)) = FP(ikpt,1:(NUM_TSTEPS/2)) + x0(iaucell,2)*( real(FP_X(ikpt,1:(NUM_TSTEPS/2),iaucell)).^2 + imag(FP_X(ikpt,1:(NUM_TSTEPS/2),iaucell)).^2     +     real(FP_Y(ikpt,1:(NUM_TSTEPS/2),iaucell)).^2 + imag(FP_Y(ikpt,1:(NUM_TSTEPS/2),iaucell)).^2     +     real(FP_Z(ikpt,1:(NUM_TSTEPS/2),iaucell)).^2 + imag(FP_Z(ikpt,1:(NUM_TSTEPS/2),iaucell)).^2 );
                end %END iaucell
            end %END ikpt

            FP = FP/(4*pi*(NUM_TSTEPS*dt/2)*NUM_UCELL_COPIES);            
    end %END ifft

        %Average over FFTS
        FP = FP/NUM_FFTS;

        %Define frequencies
        omega = (1:NUM_OMEGAS)*(w_max/NUM_OMEGAS);
        semilogy(omega,FP(1,:))

        %Output SED
        str_write=strcat(str_main,'4atom\toucell\SED_',int2str(iseed),'.txt');
                %Sum outer loops over NUM_ATOMS_UCELL and xyz
                for ikpt = 1:NUM_KPTS
                    dlmwrite(str_write,SED.kptlist(ikpt,1:3),'-append','delimiter',' ');
                    output(1:length(omega),1) = omega;
                    output(1:length(omega),2) = FP(ikpt,:);
                    dlmwrite(str_write,output,'-append','delimiter',' ');
        end %END ikpt
        
end %END iseed


% 
% for i=0:num_cells    
%     for j=0:num_cells        
%         for k=0:num_cells
%             x_ucell2( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 1 ) = x(:,1) + i*alat(1,1) ;
%             x_ucell2( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 2 ) = x(:,2) + j*alat(1,2) ;
%             x_ucell2( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 3 ) = x(:,3) + k*alat(1,3) ;
%             index=index+1;
%         end
%     end
% end





























%Johns Code
%     for				for(OMEGA_COUNT=0;OMEGA_COUNT<OMEGA_COUNT_MAX;OMEGA_COUNT++){
% 					OMEGA_CURRNET=double(double(OMEGA_COUNT)*OMEGA_RES)+OMEGA_MIN;					
% 					for(K_COUNT=0;K_COUNT<K_COUNT_MAX;K_COUNT++){
% 						K_CURRENT=double(double(K_COUNT)*K_RES+K_MIN);
% 						for(p=0;p<FROZEN_TOTAL;p++){						
% 							for(nb=0;nb<BASIS_NUM;nb++){
% 								ndivN=double(double(nb)/double(BASIS_NUM));
% 								ARG_TEMP=ndivN*K_CURRENT-1.*OMEGA_CURRNET*T_CURRENT*DT;	
% 								COSINE_TERM=cos(ARG_TEMP);
% 								SINE_TERM=sin(ARG_TEMP);							
% 								(*RE_X)[p][OMEGA_COUNT][K_COUNT]=(*RE_X)[p][OMEGA_COUNT][K_COUNT]+CVX[CARBON_P[nb][p]]*COSINE_TERM;
% 								(*IM_X)[p][OMEGA_COUNT][K_COUNT]=(*IM_X)[p][OMEGA_COUNT][K_COUNT]+CVX[CARBON_P[nb][p]]*SINE_TERM;
% 								(*RE_Y)[p][OMEGA_COUNT][K_COUNT]=(*RE_Y)[p][OMEGA_COUNT][K_COUNT]+CVY[CARBON_P[nb][p]]*COSINE_TERM;
% 								(*IM_Y)[p][OMEGA_COUNT][K_COUNT]=(*IM_Y)[p][OMEGA_COUNT][K_COUNT]+CVY[CARBON_P[nb][p]]*SINE_TERM;							
% 								(*RE_Z)[p][OMEGA_COUNT][K_COUNT]=(*RE_Z)[p][OMEGA_COUNT][K_COUNT]+CVZ[CARBON_P[nb][p]]*COSINE_TERM;
% 								(*IM_Z)[p][OMEGA_COUNT][K_COUNT]=(*IM_Z)[p][OMEGA_COUNT][K_COUNT]+CVZ[CARBON_P[nb][p]]*SINE_TERM;																
% 							}
% 						}
% 					}
% 				}
% 				T_CURRENT=T_CURRENT+1.;
% 				if(t>DISP_START&&t%DISP_INT==0){
% 					for(OMEGA_COUNT=0;OMEGA_COUNT<OMEGA_COUNT_MAX;OMEGA_COUNT++){
% 						for(K_COUNT=0;K_COUNT<K_COUNT_MAX;K_COUNT++){					
% 							for(p=0;p<FROZEN_TOTAL;p++){DISP_MAG[OMEGA_COUNT][K_COUNT]=DISP_MAG[OMEGA_COUNT][K_COUNT]+(*RE_X)[p][OMEGA_COUNT][K_COUNT]*(*RE_X)[p][OMEGA_COUNT][K_COUNT]+(*IM_X)[p][OMEGA_COUNT][K_COUNT]*(*IM_X)[p][OMEGA_COUNT][K_COUNT]+(*RE_Y)[p][OMEGA_COUNT][K_COUNT]*(*RE_Y)[p][OMEGA_COUNT][K_COUNT]+(*IM_Y)[p][OMEGA_COUNT][K_COUNT]*(*IM_Y)[p][OMEGA_COUNT][K_COUNT]+(*RE_Z)[p][OMEGA_COUNT][K_COUNT]*(*RE_Z)[p][OMEGA_COUNT][K_COUNT]+(*IM_Z)[p][OMEGA_COUNT][K_COUNT]*(*IM_Z)[p][OMEGA_COUNT][K_COUNT];}
% 							K_CURRENT=double(double(K_COUNT)*K_RES+K_MIN);
% 							OMEGA_CURRNET=double(double(OMEGA_COUNT)*OMEGA_RES)+OMEGA_MIN;					
% 							disp_out<<OMEGA_CURRNET/(2.*PI*TSTAR*1.E12)<<'\t'<<K_CURRENT<<'\t'<<DT*DISP_MAG[OMEGA_COUNT][K_COUNT]*M_C/2./double(C)/double(C)<<endl;
% 						}
% 					}
% 				}
% 			}    


% for ikpt = 1:length(SED.kptlist)
%     for iatom = 1:NUM_ATOMS_UCELL
%         for iucell = iatom:NUM_ATOMS_UCELL:NUM_ATOMS
%             range = iatom:NUM_ATOMS_UCELL:NUM_ATOMS;
%             PHI_X = PHI_X + exp(i*SED.kptlist(ikpt,1)*x0(iucell,2))*fft(vel(iucell,:,1));
%             PHI_Y = PHI_Y + exp(i*SED.kptlist(ikpt,1)*x0(iucell,2))*fft(vel(iucell,:,2));
%             PHI_Z = PHI_Z + exp(i*SED.kptlist(ikpt,1)*x0(iucell,2))*fft(vel(iucell,:,3));
%         end
%         SED.PHI() = PHI_X.*PHI_X + PHI_Y.*PHI_Y + PHI_Y.*PHI_Y;
%     end
% end









    

% 
% 
%         SAMPLE_RATE = input('Enter sample rate: ');
%         NUM_UCELLS = input('Enter number of unit cells: ');
%         NUM_MODES = input('Enter number of modes to expect: ');
% 
%         %dummy is a dummy array to store all the SED over all kpts.  It has size
%         %dummy((1024+1)*numkpts,3).  The portion of the column data that contains
%         %the SED for different frequencies will have 3 columns, 1 with actual SED
%         %data the other 2 with NaNs.  Just omit these.
% 
%         %Initialize SED object to do averaging over seeds
%         str_main=strcat('E:\CMU\work\Phonons\SED\perfect\8x8x8\5K\');
%         str=strcat(str_main,'seed1\SpectralEnergy.txt');
%         %ALEX INPUT
%         %str_main=strcat('E:\CMU\work\Phonons\SED\perfect\alex\20K\');
%         %str=strcat(str_main,'SEDseed_avg.txt');
% 
%         dummy = importdata(str,'\t');  
%             %This figures out how many kpts are being read in by checking the length
%             %of dummy and dividing by the number of kpt sections SAMPLE_RATE+1.  
%             %we've been having SAMPLE_RATE=1024;
%         NUM_KPTS = length(dummy(:,1)) / (SAMPLE_RATE+1);
%         SED.redkpt.sed(1:SAMPLE_RATE,1:NUM_KPTS) = 0.0;
%         
%     %------AVG OVER SEEDS------------------------------------------------------
%         for i1=1:NUM_SEEDS
%                 %ME
%                 str=strcat(str_main,'seed',int2str(i1),'\SpectralEnergy.txt');
%                 %ALEX
%                 %str=strcat(str_main,'SEDseed_avg.txt');
% 
%                 dummy = importdata(str,'\t');  
%                 %This figures out how many kpts are being read in by checking the length
%                 %of dummy and dividing by the number of kpt sections SAMPLE_RATE+1.  
%                 %we've been having SAMPLE_RATE=1024;
%                 NUM_KPTS = length(dummy(:,1)) / (SAMPLE_RATE+1);
% 
%                 %This takes the SED data in dummy and puts it in the object SED.freq.  The
%                 %data is stored SED.freq(1:1024,1:NUM_KPTS).  The kpt data is stored in
%                 %SED.kpt(1:NUM_KPTS,1:3) => [kx ky kz];
%                     for i2=1:NUM_KPTS
%                     SED.redkpt.sed(:,i2) = SED.redkpt.sed(:,i2) + dummy((i2-1)*(SAMPLE_RATE+1)+2:(i2)*(SAMPLE_RATE+1),1);
%                     SED.redkpt.kpt(i2,1:3) = dummy((i2-1)*(SAMPLE_RATE+1)+1,1:3);
%                     end
%         end
%         SED.redkpt.sed(:,:) = SED.redkpt.sed(:,:)/NUM_SEEDS;
% 
% 
%     %------FIND NUMBER IRR KPTS------------------------------------------------ 
% 
%         % Identify irreducible k-points, store in SED.irrkpt
%         % The first kpt is automatically a irreducible point.
%         SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
%         %str_write=strcat(str_main,'redkpt.dat');
%         %dlmwrite(str_write,SED.redkpt.kpt(1,1:3),'-append');
%         for i1=2:NUM_KPTS
%             tempcnt = 0.0;
%             %dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
%             for i2=1:SED.irrkpt.numirr 
%                 if issym(SED.irrkpt.kpt(i2,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
%                      tempcnt = 1.0;
%                      degen = i2;
%                 end
%             end
%             if tempcnt==0.0
%             SED.irrkpt.numirr = SED.irrkpt.numirr +1;
%             SED.irrkpt.kpt(SED.irrkpt.numirr,1:3) = SED.redkpt.kpt(i1,1:3);
%             else
%             SED.redkpt.degen(i1)=1;
%             end  
%         end
%         
%         
% %--------------------OUTPUT KPTLIST----------------------------------------
% 
%         str_write=strcat(str_main,'redkpt.dat');
%         dlmwrite(str_write,SED.redkpt.kpt(1,1:3),'-append');
%         for kpt_cnt=1:SED.irrkpt.numirr
%                 %------FIND RED KPTS------------------------------------------------ 
%                     % Identify irreducible k-points, store in SED.irrkpt
%                     % The first kpt is automatically a irreducible point.
%                     SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
%                     for i1=1:NUM_KPTS
%                         if issym(SED.irrkpt.kpt(kpt_cnt,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
%                             dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
%                         end
%                     end
%         end
%                    
%         
%         pause
% 
%     %------AVG DEGEN KPTS------------------------------------------------------
%         SED.irrkpt.numdegen(1:SED.irrkpt.numirr)=0;
%         SED.irrkpt.sedavg(1:SAMPLE_RATE,1:SED.irrkpt.numirr)=0;
%         for i1=1:SED.irrkpt.numirr
%             for i2=1:NUM_KPTS
%                 if issym(SED.irrkpt.kpt(i1,1:3),SED.redkpt.kpt(i2,1:3)) == 1.0;
%                      SED.irrkpt.numdegen(i1) = SED.irrkpt.numdegen(i1) +1.0;
%                      SED.irrkpt.sedavg(:,i1) = SED.irrkpt.sedavg(:,i1) + SED.redkpt.sed(:,i2);
%                 end
%             end
%             SED.irrkpt.sedavg(:,i1) = SED.irrkpt.sedavg(:,i1)/SED.irrkpt.numdegen(i1);
%         end
%     %pause   
% 
% 
% %RE-DO LIST
%     REDO=0; 
%     AUTO=1;
% %8x m3 0.01    
%     %redo_list=  [7 10 14 15 18 19 20 21 23 24 26 27 28 30 33 34];
%     %redo_list=  [35];
% %6x m3 0.01
%     %redo_list = 1:SED.irrkpt.numirr;
% %Perfect 5K
%     %HLD_SCALING_PCT=1.0;
%     HLD_SCALING_PCT=1.017;
%     LC =5.2795e-10/sigma_Ar;        % For HLD peak calculation
% %Perfect 20K
%     %HLD_SCALING_PCT=1.0;
%     %HLD_SCALING_PCT=1.03;
%     %LC = 1.56328;        % For HLD peak calculation
% %Perfect 40K
%     %HLD_SCALING_PCT=1.04;
%     %LC = 1.57931;        % For HLD peak calculation
% %m3 0.01
% %    HLD_SCALING_PCT=0.8255;
% %    LC =5.27905e-10/sigma_Ar;        % For HLD peak calculation
% %m3 0.05
%     %HLD_SCALING_PCT=0.8;
% %m10 0.01
%     %HLD_SCALING_PCT=0.87;
%     
% %SET VOLUME FOR KAPPA    
%     VOLUME = (NUM_UCELLS*LC*sigma_Ar)^3;
%     
% %BUILD MAIN LIST OF IRRKPTS TO RUN OVER    
%     kpt_list = 1:SED.irrkpt.numirr;
%     RUN=1;
% 
% while RUN==1
% 
%     for kpt_cnt=1:length(kpt_list)
%         
%         %PRINT CURRENT KPT
%                     SED.irrkpt.kpt(kpt_list(kpt_cnt),1:3)
%         %CALCULATE HLD INFO
%         KPT_TOL = 10E-6; 
%         DEGEN_WIDTH=1.0;
%                     kx = SED.irrkpt.kpt(kpt_list(kpt_cnt),1)/NUM_UCELLS;
%                     ky = SED.irrkpt.kpt(kpt_list(kpt_cnt),2)/NUM_UCELLS;
%                     kz = SED.irrkpt.kpt(kpt_list(kpt_cnt),3)/NUM_UCELLS;
%                     
%                     if kx==0.5; kx = kx-KPT_TOL; end
%                     if ky==0.5; ky = ky-KPT_TOL; end
%                     if kz==0.5; kz = kz-KPT_TOL; end
%                     
%                     [HLDpks,eig,vel] = LDAr_FCC([LC LC LC],[kx ky kz]);
%                     %Convert to rads/s
%                     SED.irrkpt.HLDfreq(1:length(HLDpks),kpt_list(kpt_cnt)) = HLDpks/tau_Ar;
%                     %Convert to m/s
%                     SED.irrkpt.HLDvel(1:length(HLDpks),1:3,kpt_list(kpt_cnt)) = vel*(sigma_Ar/tau_Ar);
%                     %Print HLD freqs
%                     ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT)
% 
%         %FIND # OF NON_DEGENERATE HLD PEAKS
%                     %[HLDndpks,numndpks] = ndpks_HLD(ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT),DEGEN_WIDTH);
%                     
%         semilogy(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))),SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)),'.') 
%         
%         %SPECIFY LOCATION OF PEAKS AUTO OR MANUAL (REDO)        
%                     
%         if AUTO==1                       
%                     %FIND # OF NON_DEGENERATE PEAKS
%                     HLDdegenlist = group_peaks(ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT),DEGEN_WIDTH);
%                     [HLDndpks,numndpks] = ndpks_HLD(ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT),DEGEN_WIDTH);
%                     loc=HLDndpks
%                     %Don't look for 0 freq at the gamma pt
%                     if SED.irrkpt.kpt(kpt_list(kpt_cnt),1)==0 & SED.irrkpt.kpt(kpt_list(kpt_cnt),2)==0 & SED.irrkpt.kpt(kpt_list(kpt_cnt),3)==0
%                         loctemp=loc; clear loc
%                         loc=(loctemp(2:length(loctemp)));
%                     end   
%                     
%         elseif REDO==1
%             semilogy(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))),SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)),'.')
%             number_pks = input('Enter the number of peaks: ');
%             number_pks = 12;
%             for i2=1:number_pks
%                 str_input = strcat('Enter peak  ',int2str(i2),' location: ');
%                 loc(i2) = input(str_input);
%             end
%                     %FIND # OF NON_DEGENERATE PEAKS
%                     [HLDndpks,numndpks] = ndpks_HLD(loc,DEGEN_WIDTH);
%                     %loc=HLDndpks
%                     HLDdegenlist = group_peaks(loc,DEGEN_WIDTH);
%                     loc = HLDndpks
%         end
%      
%         
% %Set freq to 1:sample_size
%                 w(:,1)=(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))));   
% %Run over all peak locations found
%                 str_func= '';
% %GROUP LOCATION WIDTH
%                 if REDO==1
%                     GROUP_LOCATION_WIDTH=input('Enter GROUP LOCATION WIDTH: ');
%                 else
%                     GROUP_LOCATION_WIDTH=30;
%                 end
%                 %GROUP PEAKS FOR SINGLE OR MULTIPLE FITS        
%                 group_loc = group_peaks(loc,GROUP_LOCATION_WIDTH);
% %BUILD LORENTZIAN FUNCTIONS SINGLE AND MULTIPLE     
%     %Pad the frequencies and lifetimes by the 0 freqs from HLD
%                 HLD_FREQ_TOL = 0.01;
%                 Ipad = find(abs(SED.irrkpt.HLDfreq(:,kpt_list(kpt_cnt)))<HLD_FREQ_TOL);
%                 SED.irrkpt.sedfreq(1:length(Ipad),kpt_list(kpt_cnt)) = SED.irrkpt.HLDfreq(1:length(Ipad),kpt_list(kpt_cnt));
%                 
% %Loop over grouped peaks and fit 
%         %Keep track of pk count w.r.t. HLD degen
%         pk_cnt=1;
%         for group_id = 1:group_loc(length(group_loc))               
%             Igroup = find(group_loc==group_id);
%             str_func= '';
%             for i3=1:length(Igroup)
%                 if length(Igroup)==1
%                 strtemp= strcat('(c(',int2str(i3),'))./((w - c(',int2str(i3+2),')).^2 + (c(',int2str(i3+1),')/2).^2)');
%                 str_func = strcat(str_func,strtemp);
%                 else
%                 strtemp= strcat(' + ( c(',int2str(3*(i3-1)),'))./((w - c(',int2str(3*(i3-1)+2),')).^2 + (c(',int2str(3*(i3-1)+1),')/2).^2)');
%                 str_func = strcat(str_func,strtemp);
%                 end  
%             end
% 
%         %LORENTZIAN FUNCTION FOR SINGLE AND MULTIPLE PEAKS            
%             lor_func = inline(str_func,'c','w');
% %PERCENT PAST PEAKS
%         if REDO==1
%         PT_PERC=input('Enter PERCENT PAST PEAKS: ');
%         else
%             if length(Igroup) == 1
%                 PT_PERC = 0.5;
%             else
%                 PT_PERC = 0.5;
%             end
%         end
% 
%             %Find wleft    
%                     [I,J] = find(PT_PERC*SED.irrkpt.sedavg(loc(Igroup(1)),kpt_list(kpt_cnt))>SED.irrkpt.sedavg(1:loc(Igroup(1)),kpt_list(kpt_cnt)) );
% %                     loc(Igroup(1))
% %                     SED.irrkpt.sedavg(loc(Igroup(1)),kpt_list(kpt_cnt))
% %                     PT_PERC*SED.irrkpt.sedavg(loc(Igroup(1)),kpt_list(kpt_cnt))
%                     wleft = w(I(length(I)));
%             %Find wright
%                     [I,J] = find(PT_PERC*SED.irrkpt.sedavg(loc(Igroup(length(Igroup))),kpt_list(kpt_cnt))>SED.irrkpt.sedavg(loc(Igroup(length(Igroup))):length(SED.irrkpt.sedavg),kpt_list(kpt_cnt)) );
%                     wright = w(loc(Igroup(length(Igroup)))+I(1)  );
%                     gamma_guess = wright-wleft; 
%                     buffer = ceil(gamma_guess*0.25);
%             %Build intial guess array
%                     for i3=1:length(Igroup)   
%                         if length(Igroup)==1
%                         c0(1+(i3-1)*3:i3*3) = [ SED.irrkpt.sedavg(loc(Igroup(i3)),kpt_list(kpt_cnt)), gamma_guess, w(loc(Igroup(i3))) ];
%                         else
%                         %make gamma_guess/length(Igroup) to make a guess that all
%                         %multiple lorentzians have 1/N (1/3 for N=3 example) the width
%                             if REDO==1  
%                                 gamma_guess = input('Input gamma guess: ');
%                             c0(1+(i3-1)*3:i3*3) = [ SED.irrkpt.sedavg(loc(Igroup(i3)),kpt_list(kpt_cnt)), gamma_guess, w(loc(Igroup(i3))) ];    
%                             else
%                                 gamma_guess = 5;
%                             c0(1+(i3-1)*3:i3*3) = [ SED.irrkpt.sedavg(loc(Igroup(i3)),kpt_list(kpt_cnt)), gamma_guess, w(loc(Igroup(i3))) ];    
%                             end
%                         end
%                     end 
%                     
%             %FIT THE LORENTZIAN(S)
%             
%             [c_fit,r,j] = nlinfit(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt)),lor_func,c0);
% 
%             %Store separate liftimes and frequencies for single and MULTIPLE FITS
%             %Hold on to plot multiple peaks
%             hold on
%                     Igroup = find(group_loc==group_id);
%                         for i3=1:length(Igroup)  
%                             %if length(Ipad)==0
%                                 Idegen = find(HLDdegenlist==pk_cnt);
%                             %else
%                             %    Idegen = find(HLDdegenlist==pk_cnt+1);
%                             %end
%                             center=c_fit(3*i3)*w_step;
%                             lifetime=1/c_fit(3*i3-1)/w_step;
%                             %Store in rads/ps
%                             SED.irrkpt.sedfreq( Idegen ,kpt_list(kpt_cnt)) = center;
%                             %Store in ps
%                             SED.irrkpt.life( Idegen ,kpt_list(kpt_cnt)) = lifetime;
%                             %Keep track of pk position taking
%                             %into account degen w.r.t. HLD
%                             pk_cnt = pk_cnt +1;
%                         end
%             %Plot each fit, single and multiple
%                         plot(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt)),w(wleft:wright),lor_func(c_fit,w(wleft:wright)))
%                         axis([0 1.0*length(w) 0.0001*max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))) max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)))]);
%         end
%                 disp(sprintf('%s', 'SED FREQ:'));
%                 SED.irrkpt.sedfreq(:,kpt_list(kpt_cnt))
%                 disp(sprintf('%s', 'SED LIFE:'));
%                 SED.irrkpt.life(:,kpt_list(kpt_cnt))
%                 loc
%         %1=yes, 0=no
%         SED.irrkpt.flag(kpt_list(kpt_cnt)) = input('Flag?: ');
%         %Clear current plot
%         clf
%         hold off
%     clear I J buffer wleft wright c0 pks loc str_func lifetime center
%     end
%     
% %Check for any flags, if so redo those kpts manually
%     for i1=1:length(SED.irrkpt.flag(:))
%         Iflag = find(SED.irrkpt.flag==1);
%         if length(Iflag)==0;
%             RUN=0;
%         else
%             RUN=1; AUTO=0; REDO=1; kpt_list = Iflag;
%             %disp(sprintf('%s', 'REDO:'));
%         end
%     end
%     
% end
% 
% 
% 
% %--------------------OUTPUT DATA-------------------------------------------  
% 
%     str_loop = input('Write data to file?: ');
%     str_loop=1;
%     str_write=strcat(str_main,'SED_data.dat');
%     str_write_life=strcat(str_main,'SED_life.dat');
%     if str_loop ==1
%         NUM_MODES = length(SED.irrkpt.HLDfreq(:,1));
%         for kpt_cnt=1:SED.irrkpt.numirr
%                 %------FIND RED KPTS------------------------------------------------ 
%                     % Identify irreducible k-points, store in SED.irrkpt
%                     % The first kpt is automatically a irreducible point.
%                     SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
%                     for i1=1:NUM_KPTS
%                         if issym(SED.irrkpt.kpt(kpt_cnt,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
%                             %Write the wavevector first
%                             dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
%                             %Write the number of degenerate kpts for mult factor
%                             %dlmwrite(str,SED.irrkpt.numdegen(kpt_list(kpt_cnt)),'-append')
%                             %;
%                             %CALCULATE HLD DATA FOR RED KPT
%                                 kx = SED.redkpt.kpt(i1,1)/NUM_UCELLS;
%                                 ky = SED.redkpt.kpt(i1,2)/NUM_UCELLS;
%                                 kz = SED.redkpt.kpt(i1,3)/NUM_UCELLS;
%                                 if kx==0.5; kx = kx-KPT_TOL; end
%                                 if ky==0.5; ky = ky-KPT_TOL; end
%                                 if kz==0.5; kz = kz-KPT_TOL; end
% 
%                                 [HLDpks,eig,vel] = LDAr_FCC([LC LC LC],[kx ky kz]);
%                                 %Convert to rads/s
%                                 HLDpks = HLDpks/tau_Ar;
%                                 %Convert to m/s
%                                 vel = vel*(sigma_Ar/tau_Ar);
% 
%                                     for i2=1:NUM_MODES
%                                         whld = HLDpks(i2);
%                                         wsed = SED.irrkpt.sedfreq(i2,kpt_cnt);
%                                         lifetau = SED.irrkpt.life(i2,kpt_cnt);
%                                         velx = vel(i2,1);
%                                         vely = vel(i2,2);
%                                         velz = vel(i2,3);
%                                         svelx = velx*(wsed/whld);
%                                         svely = vely*(wsed/whld);
%                                         svelz = velz*(wsed/whld);
%                                         kappax = (kb/VOLUME)*lifetau*(svelx^2);
%                                         kappay = (kb/VOLUME)*lifetau*(svely^2);
%                                         kappaz = (kb/VOLUME)*lifetau*(svelz^2);
% 
% 
%                                         SED.redkpt.HLDfreq(i2,i1) = whld;
%                                         SED.redkpt.sedfreq(i2,i1) = wsed;
%                                         SED.redkpt.HLDfreq(i2,i1) = lifetau;
%                                         SED.redkpt.HLDvel(i2,1,i1) = velx;
%                                         SED.redkpt.HLDvel(i2,2,i1) = vely;
%                                         SED.redkpt.HLDvel(i2,3,i1) = velz;
%                                         SED.redkpt.svel(i2,1,i1) = svelx;
%                                         SED.redkpt.svel(i2,2,i1) = svely;
%                                         SED.redkpt.svel(i2,3,i1) = svelz;
%                                         SED.redkpt.kappa(i2,1,i1) = kappax;
%                                         SED.redkpt.kappa(i2,2,i1) = kappay;
%                                         SED.redkpt.kappa(i2,3,i1) = kappaz;
% 
%                                     output_format = [whld wsed lifetau velx vely velz svelx svely svelz,kappax,kappay,kappaz];
%                                     output_life = [whld wsed lifetau velx vely velz svelx svely svelz,kappax,kappay,kappaz];
%                                     dlmwrite(str_write,output_format,'-append');
%                                     dlmwrite(str_write_life,output_life,'-append');
% %                                 if wsed<(w_max*10^12) && wsed>(w_step) && lifetau<(10^-10) && lifetau>(10^-13)
% %                                 output_format = [whld wsed lifetau velx vely velz svelx svely svelz]; 
% %                                 else
% %                                 output_format = [0 0 0 0 0 0 0 0 0]; 
%                                     end
%                         end
%                     end
%         end
%     end
% %END MAIN FUNCTION    
% end
% 
% 
% 
% %-------------FUNCTIONS----------------------------------------------------
% 
% 
% function istrue=issym(kpt1,kpt2)
% istrue=0.0;
% istrue1=0.0; istrue2=0.0;
%         for i2=-1:2:1
%             for i3=-1:2:1
%                 for i4=-1:2:1
%                     
%                     temp(1) = i2*kpt2(1); temp(2) = i3*kpt2(2); temp(3) = i4*kpt2(3);
% 
%                     if kpt1(1) == temp(1) && kpt1(2) == temp(2) && kpt1(3) == temp(3)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(3) && kpt1(2) == temp(1) && kpt1(3) == temp(2)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(2) && kpt1(2) == temp(3) && kpt1(3) == temp(1)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(1) && kpt1(2) == temp(3) && kpt1(3) == temp(2)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(3) && kpt1(2) == temp(2) && kpt1(3) == temp(1)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(2) && kpt1(2) == temp(1) && kpt1(3) == temp(3)
%                         istrue=1.0;
%                     end
%                 end
%             end
%         end
% end
% 
% 
%      
% function group_loc = group_peaks(loc,groupdis)
%     sup_peak_dist = groupdis;
%     clear fitpks
%     for i=1:length(loc)-1
%         dist(i) = loc(i+1) - loc(i);
%     end
% 
%     id = 1;
%     for i=1:length(loc) - 1
%         fitpks(i) = id;
%         if (dist(i) > sup_peak_dist)
%             id = id + 1;
%         end
%     end
%     fitpks(i + 1) = id;
%     group_loc = fitpks; 
% end
% 
% function [HLDndpks,numndpks] = ndpks_HLD(HLDpks,DEGEN_WIDTH)
%     numndpks = 1;
%     HLDndpks(1) = HLDpks(1);
%     for j=2:length(HLDpks) %should be equal to 12
%         degencount = 0;
%         for k=1:numndpks
%             if abs(HLDpks(j)-HLDndpks(k)) < DEGEN_WIDTH
%                 degencount = 1;
%             end
%         end
%         if degencount == 0
%             numndpks = numndpks +1;
%             HLDndpks(numndpks) = HLDpks(j);
%         end
%     end
% end
% 

