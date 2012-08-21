
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

LC = 1.548;        % For HLD peak calculation
w_step = 2*pi/(2^16)/(5E-15);
w_max = 2*pi/(2^6)/(5E-15);

NUM_SEEDS=10;

        SAMPLE_RATE = input('Enter sample rate: ');
        %dummy is a dummy array to store all the SED over all kpts.  It has size
        %dummy((1024+1)*numkpts,3).  The portion of the column data that contains
        %the SED for different frequencies will have 3 columns, 1 with actual SED
        %data the other 2 with NaNs.  Just omit these.

        %Initialize SED object to do averaging over seeds
        %str_main=strcat('E:\CMU\work\Phonons\SED\defect\sigma\8x8x8\1.1\5K\');
        %str_main=strcat('E:\CMU\work\Phonons\SED\defect\mass\8x8x8\m3\5K\');
        str_main=strcat('E:\CMU\work\Phonons\SED\defect\mass\0.5\');
        str=strcat(str_main,'seed1\SpectralEnergy.txt');

        dummy = importdata(str,'\t');  
            %This figures out how many kpts are being read in by checking the length
            %of dummy and dividing by the number of kpt sections SAMPLE_RATE+1.  
            %we've been having SAMPLE_RATE=1024;
        NUM_KPTS = length(dummy(:,1)) / (SAMPLE_RATE+1);
        SED.redkpt.sed(1:SAMPLE_RATE,1:NUM_KPTS) = 0.0;
        
%------AVG OVER SEEDS------------------------------------------------------
    for i1=1:NUM_SEEDS
        
            str=strcat(str_main,'seed',int2str(i1),'\SpectralEnergy.txt');

            dummy = importdata(str,'\t');  
            %This figures out how many kpts are being read in by checking the length
            %of dummy and dividing by the number of kpt sections SAMPLE_RATE+1.  
            %we've been having SAMPLE_RATE=1024;
            NUM_KPTS = length(dummy(:,1)) / (SAMPLE_RATE+1);

            %This takes the SED data in dummy and puts it in the object SED.freq.  The
            %data is stored SED.freq(1:1024,1:NUM_KPTS).  The kpt data is stored in
            %SED.kpt(1:NUM_KPTS,1:3) => [kx ky kz];
                for i2=1:NUM_KPTS
                SED.redkpt.sed(:,i2) = SED.redkpt.sed(:,i2) + dummy((i2-1)*(SAMPLE_RATE+1)+2:(i2)*(SAMPLE_RATE+1),1);
                SED.redkpt.kpt(i2,1:3) = dummy((i2-1)*(SAMPLE_RATE+1)+1,1:3);
                end
    end
    SED.redkpt.sed(:,:) = SED.redkpt.sed(:,:)/NUM_SEEDS;


%------FIND NUMBER IRR KPTS------------------------------------------------ 
    
    % Identify irreducible k-points, store in SED.irrkpt
    % The first kpt is automatically a irreducible point.
    SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
    for i1=2:NUM_KPTS
        tempcnt = 0.0;
        for i2=1:SED.irrkpt.numirr 
            if issym(SED.irrkpt.kpt(i2,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
                 tempcnt = 1.0;
                 degen = i2;
            end
        end
        if tempcnt==0.0
        SED.irrkpt.numirr = SED.irrkpt.numirr +1;
        SED.irrkpt.kpt(SED.irrkpt.numirr,1:3) = SED.redkpt.kpt(i1,1:3);
        %SED.irrkpt.numdegen(i2) = SED.irrkpt.numdegen(i2) +1; 
        %SED.irrkpt.degen(i1)=0;
        %SED.numdegen(i2) = SED.numdegen(i2) +1;
        %SED.avgfreq(:,SED.numirr) = SED.avgfreq(:,SED.numirr) + SED.freq(:,i1); 
        else
        SED.redkpt.degen(i1)=1;
        end
       
    end

%------AVG DEGEN KPTS------------------------------------------------------
    SED.irrkpt.numdegen(1:SED.irrkpt.numirr)=0;
    SED.irrkpt.sedavg(1:SAMPLE_RATE,1:SED.irrkpt.numirr)=0;
    for i1=1:SED.irrkpt.numirr
        for i2=1:NUM_KPTS
            if issym(SED.irrkpt.kpt(i1,1:3),SED.redkpt.kpt(i2,1:3)) == 1.0;
                 SED.irrkpt.numdegen(i1) = SED.irrkpt.numdegen(i1) +1.0;
                 SED.irrkpt.sedavg(:,i1) = SED.irrkpt.sedavg(:,i1) + SED.redkpt.sed(:,i2);
            end
        end
        SED.irrkpt.sedavg(:,i1) = SED.irrkpt.sedavg(:,i1)/SED.irrkpt.numdegen(i1);
    end
%pause   





%------FIND SED PEAKS------------------------------------------------------




for i1=1:SED.numirr
    
    %PRINT CURRENT KPT
    SED.irrkpt.kpt(i1,1:3)
    
    %------CALCULATE HLD PEAKS-------------------------------------------------     
    [HLDpks,eig,vel] = LDAr_FCC([LC LC LC],SED.irrkpt.kpt(i1,1:3));
    HLDpks = ceil(HLDpks/tau_Ar/w_step);
    
    SED.irrkpt.HLDfreq(1:12,i1) = HLDpks;
    SED.irrkpt.HLDvel(1:12,1:3,i1) = vel;
    
    
    %------FIND # OF NON_DEGENERATE PEAKS--------------------------------------
    numndpks = 1;
    HLDndpks(1) = HLDpks(1);
    for j=2:length(HLDpks) %should be equal to 12
        degencount = 0;
        for k=1:numndpks
            if abs((HLDpks(j)-HLDndpks(k))/HLDndpks(k)) < 0.05
                degencount = 1;
            end
        end
        if degencount == 0
            numndpks = numndpks +1;
            HLDndpks(numndpks) = HLDpks(j);
        end
    end
    
    numndpks
    
    
    
    %------SCAN THRESH AND MPD-------------------------------------------------     
    threshlist=30:10:150; mpdlist=5:1:20;
    for i2=1:length(threshlist)
        for i3=1:length(mpdlist)
            [pks,loc] = findpeaks(SED.irrkpt.sedavg(:,i1),'threshold',threshlist(i2),'minpeakdistance',mpdlist(i3));
            numpks(i2,i3) = length(pks);
        end
    end    
    
      
    %------USER INPUT THRESH AND MPD-------------------------------------------       
    
    str_loop=0;
    
    while str_loop==0
            subplot(2,1,1), surf(mpdlist,threshlist,numpks)
            subplot(2,1,2), semilogy(1:length(SED.irrkpt.sedavg(:,i1)),SED.irrkpt.sedavg(:,i1),'.')
            %plateau = mode(mode(numpks));
            %[I,J] = find(mode(mode(numpks))==numpks);
            %thresh = ceil((max(I)-min(I))/2);
            %minpd = ceil((max(J)-min(J))/2);   

            thresh = input('Enter thresh: ');
            minpd = input('Enter minpd: ');

            % identify number of peaks, locations and heights for the appropriate
            % threshold, minpeakdistance
            %[pks,loc] = findpeaks(SED.freqavg(:,i1),'threshold',threshlist(thresh),'minpeakdistance',mpdlist(minpd));

            [pks,loc] = findpeaks(SED.irrkpt.sedavg(:,i1),'threshold',thresh,'minpeakdistance',minpd);
            %initial parameter guess
            subplot(2,1,2), semilogy(1:length(SED.irrkpt.sedavg(:,i1)),SED.irrkpt.sedavg(:,i1),loc,pks,'.')
            %SED.marked(i1) = input('Mark?: ');
            str_loop = input('Good find?: ');
    end  
    
%The inline version
% c(1) -> A
% c(2) -> gamma
% c(3) -> w0
func = inline('(c(1)*c(2))./((w - c(3)).^2 + (c(2)/2).^2)','c','w');   

%!!!!!!!!!!!!!!!!!!!!!!!!!!!CHANGE THIS TO ACTUAL FREQUENCY VALUES
w(:,1)=(1:length(SED.irrkpt.sedavg(:,i1)));   

    for i2=1:length(loc)

       PT_PERC = 0.55;
            
    %Find wleft    
            [I,J] = find(PT_PERC*SED.irrkpt.sedavg(loc(i2),i1)>SED.irrkpt.sedavg(1:loc(i2),i1) );
            wleft = w(I(length(I)));
    %Find wright
            [I,J] = find(PT_PERC*SED.irrkpt.sedavg(loc(i2),i1)>SED.irrkpt.sedavg(loc(i2):length(SED.freqavg),i1) );
            wright = w(I(1)+loc(i2));
            gamma_guess = wright-wleft; buffer = ceil(gamma_guess*0.25);
            c0 = [ gamma_guess*SED.irrkpt.sedavg(loc(i2),i1)/4, gamma_guess, w(loc(i2)) ];
            
            [c,r,j] = nlinfit(w( (wleft-buffer):(wright+buffer)),SED.irrkpt.sedavg((wleft-buffer):(wright+buffer),i1),func,c0);
            plot(w( (wleft-buffer):(wright+buffer)),SED.irrkpt.sedavg((wleft-buffer):(wright+buffer),i1),w( (wleft-buffer):(wright+buffer)),func(c,w( (wleft-buffer):(wright+buffer))))
            
            center=c(3)*w_step/10^12;
            lifetime=10^12/c(2)/w_step;
            
            SED.irrkpt.sedfreq(i1,i2,1) = c(3)*w_step/10^12;
            SED.irrkpt.life(i1,i2,2) = 10^12/c(2)/w_step;     
            pause
    end
    subplot(1,1,1), semilogy( w*w_step*10E-13, SED.irrkpt.sedavg(:,i1),loc*(w_step*10E-13),pks,'.')

                    clear I J buffer wleft wright c0 pks loc    
end








%         SED.life_freq(:,1) = 0; SED.life_freq(:,2) = 0; 
%         [I]=find(SED.life(1,:,3)~=0);
%         lenI = size(I);
%         SED.life_freq(1:lenI(2),1) = SED.life(1,I,2);
%         SED.life_freq(1:lenI(2),2) = SED.life(1,I,3);
%         
%         for i1=2:SED.numirr
%             lifesize = size(SED.life_freq);
%             [I]=find(SED.life(i1,:,3)~=0);
%             lenI = size(I);
%             
%             SED.life_freq((lifesize(1,1)+1):(lifesize(1,1)+lenI(2)),1) = SED.life(i1,I,2);
%             SED.life_freq((lifesize(1,1)+1):(lifesize(1,1)+lenI(2)),2) = SED.life(i1,I,3);
%         end
%         
%     str=strcat(str_main,'tau_vs_freq.dat');
%     dlmwrite(str,SED.life_freq);   
        
%         str=strcat(str_main,'tau_vs_freq.dat');
% 
%         for i1=1:SED.numirr
%             dlmwrite(str,SED.life(i1,:,2),'-append');
%             dlmwrite(str,SED.life(i1,:,3),'-append');
%         end
        
     

%Function to fit lorentzians

%X = LSQCURVEFIT(@fun_lorentzian(,X0,XDATA,YDATA);




%Portion to see how number of peaks found varies with threhold value.
%SED(:,1) =load( 'E:\CMU\work\Phonons\SED\Matlab\SED210.txt');





%[xmax,imax,xmin,imin] = extrema(SED(:,1))

%-------------FUNCTIONS----------------------------------------------------


% function istrue=issym(kptlist,kpt2)
% istrue=0.0;
% sizekpt1 = size(kpt1);
%     for i1=1:sizekpt1(1,1)
%         %kpt1 = kptlist(i1,1:3);
%         for i2=-1:2:1
%             for i3=-1:2:1
%                 for i4=-1:2:1
%                     
%                     temp(1) = i2*kpt2(1); temp(2) = i3*kpt2(2); temp(3) = i4*kpt2(3);
%                     
%                     tmp1 = find(temp(1)==kpt1); tmp2 = find(temp(2)==kpt1); tmp3 = find(temp(3)==kpt1);
%                     
%                     if tmp1~=0.0 & tmp2~=0.0 & tmp3~=0.0
%                         istrue=1;
%                     end
%                 end
%             end
%         end
%     end
%             
            
