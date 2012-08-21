
%function SEDfit_JL_LAMMPS_051511

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




        
str_main = 'E:\CMU\work\Phonons\LJArgon\Solid\crystal\perfect\SED\32\1atom\';

%KPTLIST: Load kpt list
            str_read=strcat(str_main,'kptlist.dat');
            SED.kptlist(:,1:3) = load(str_read); [NUM_KPTS, blank] = size(SED.kptlist(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
            str_read=strcat(str_main,'x0.data');
            x0 = load(str_read);
            %Define number of atoms
            NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL; 
            %Define box size and conventional cell lattice parameters
            NUM_SUPERCELL = 4;
            L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); LC = L(1)/(NUM_SUPERCELL);
            %chop off first line of input structure
            x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
            str_read=strcat(str_main,'SED_param.dat');
            SEDparam = load(str_read);
            N_wmax = SEDparam(1); N_wstep = SEDparam(2); t_total = SEDparam(3); t_step = SEDparam(4);
            dt = SEDparam(5);  NUM_FFTS = SEDparam(6); NUM_SEEDS = SEDparam(7);
            w_step = 2*pi/(t_total*dt*tau_Ar); w_max = 2*pi/(t_step*dt*2*tau_Ar);
            w_plot = 2^10;  
            NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step);
            %Perfect 20K
            HLD_SCALING_PCT=1.03;
            %HLD_SCALING_PCT=0.95;
            SAMPLE_RATE = w_max/w_step;
            %NUM_UCELLS = (NUM_ATOMS/NUM_ATOMS_UCELL)^(1/3);
            VOLUME = (NUM_SUPERCELL*LC*sigma_Ar)^3;
            %[HLDpks,eig,vel] = LDAr_Prim([LC LC LC],[kx ky kz]);
            
            
%Read Different Size files
        str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\perfect\SED\32\1atom\');
        str=strcat(str_main,'SED_life.dat');
        SED1.data = importdata(str,','); 
        str=strcat(str_main,'kptlist.dat');
        SED1.kpt = importdata(str,'\t'); 
        
        str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\perfect\SED\8x\1atom\');
        str=strcat(str_main,'SED_life.dat');
        SED2.data = importdata(str,','); 
        str=strcat(str_main,'kptlist.dat');
        SED2.kpt = importdata(str,'\t'); 
        
        
        
        index = 1;
        for i1=1:length(SED1.kpt)
        I = find( SED2.kpt(:,1) == SED1.kpt(i1,1) & SED2.kpt(:,2) == SED1.kpt(i1,2) & SED2.kpt(:,3) == SED1.kpt(i1,3) & SED1.data(3*(i1-1)+1,10)<1000 );
        if length(I)~=0
        Index2(index) = I;
        Index1(index) = i1; 
        index=index+1;
        end
        end
        INDEX1 = [];
        for i1=1:length(Index1)
            INDEX1 = [INDEX1 3*(Index1(i1)-1)+(1:3)];
        end
        INDEX2 = [];
        for i1=1:length(Index2)
            INDEX2 = [INDEX2 3*(Index2(i1)-1)+(1:3)];
        end
        
        pause
        plot(SED1.data(INDEX1,1),SED1.data(INDEX1,3),'.',SED2.data(INDEX2,1),SED2.data(INDEX2,3),'.')
        pause
        plot(SED1.data(INDEX1,1),SED1.data(INDEX1,12),'.',SED2.data(:,1),SED2.data(:,12),'.')
        pause
        plot(SED1.data(INDEX1,1),SED1.data(INDEX1,3)./SED2.data(INDEX2,3),'.')
         
        pause
        plot(SED1.data(INDEX1,1),SED1.data(INDEX1,12),'.',SED2.data(:,1),SED2.data(:,12),'.')
        pause
        plot(SED1.data(INDEX1,1),SED1.data(INDEX1,4)./SED2.data(INDEX2,4),'.')
        pause
        %PLOT 
        I=find((SED1.data(INDEX1(:),4)./SED2.data(INDEX2(:),4))<0.95 );
        plot(SED1.data(INDEX1(I),1),SED1.data(INDEX1(I),7)./SED2.data(INDEX2(I),7),'.')
        
        pause
        
        I=find(SED1.data(INDEX1(:),10)>0.00001 );
        plot(SED1.data(INDEX1(I),1),SED1.data(INDEX1(I),10)./SED2.data(INDEX2(I)/8,10),'.')
        
        
%vel*(sigma_Ar/tau_Ar);
    
        
%                 if issym(SED.irrkpt.kpt(i2,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
%                      tempcnt = 1.0;
%                      idegen = i2;
%                 end
%             end
% 
% 
% %--------------------OUTPUT KPTLIST----------------------------------------
% 
% %         str_write=strcat(str_main,'kptlist.dat');
% %         dlmwrite(str_write,SED.redkpt.kpt(1,1:3),'-append','delimiter',' ');
% %         for kpt_cnt=1:NUM_KPTS
% %                     output_kpt = SED.redkpt.kpt(kpt_cnt,1:3);
% %                             dlmwrite(str_write,output_kpt,'-append','delimiter',' ');
% %         end        
% 
% % %--------------------OUTPUT REDKPTLIST----------------------------------------
% % 
% %         str_write=strcat(str_main,'redkpt.dat');
% %         dlmwrite(str_write,SED.redkpt.kpt(1,1:3),'-append');
% %         for kpt_cnt=1:SED.irrkpt.numirr
% %                 %------FIND RED KPTS------------------------------------------------ 
% %                     % Identify irreducible k-points, store in SED.irrkpt
% %                     % The first kpt is automatically a irreducible point.
% %                     SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
% %                     for i1=1:NUM_KPTS
% %                         if issym(SED.irrkpt.kpt(kpt_cnt,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
% %                             dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
% %                         end
% %                     end
% %         end
%                    
% 
% 
%        
% 
% 
%                     
%  
