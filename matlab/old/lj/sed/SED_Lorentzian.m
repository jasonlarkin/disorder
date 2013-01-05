
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

LC = 1.563;        % For HLD peak calculation
w_step = 2*pi/(2^16)/(5E-15);
w_max = 2*pi/(2^6)/(5E-15);


sample_rate = input('Enter sample rate: ');
%dummy is a dummy array to store all the SED over all kpts.  It has size
%dummy((1024+1)*numkpts,3).  The portion of the column data that contains
%the SED for different frequencies will have 3 columns, 1 with actual SED
%data the other 2 with NaNs.  Just omit these.
dummy = importdata('E:\CMU\work\Phonons\SED\Matlab\Defect\SpectralEnergy.txt','\t');  
%This figures out how many kpts are being read in by checking the length
%of dummy and dividing by the number of kpt sections sample_rate+1.  
%we've been having sample_rate=1024;
num_kpts = length(dummy(:,1)) / (sample_rate+1);

%This takes the SED data in dummy and puts it in the object SED.freq.  The
%data is stored SED.freq(1:1024,1:num_kpts).  The kpt data is stored in
%SED.kpt(1:num_kpts,1:3) => [kx ky kz];
    for i1=1:num_kpts
    SED.freq(:,i1) = dummy((i1-1)*(sample_rate+1)+2:(i1)*(sample_rate+1),1);
    SED.kpt(i1,1:3) = dummy((i1-1)*(sample_rate+1)+1,1:3);
    end

%------FIND IRR KPTS-------------------------------------------------------    
    
    % Identify irreducible k-points, store in SED.irrkpt
    SED.irrkpt(1,1:3) = SED.kpt(1,1:3); SED.degen(1)=0; SED.numirr=1;
    for i1=2:num_kpts
        tempcnt = 0.0;
        for i2=1:SED.numirr 
            if issym(SED.irrkpt(i2,1:3),SED.kpt(i1,1:3)) == 1.0;
                 tempcnt = 1.0; 
            end
        end
        if tempcnt==0.0
        SED.numirr = SED.numirr +1;
        SED.irrkpt(SED.numirr,1:3) = SED.kpt(i1,1:3);
        SED.degen(i1)=0;
        %SED.numdegen(i2) = SED.numdegen(i2) +1;
        %SED.avgfreq(:,SED.numirr) = SED.avgfreq(:,SED.numirr) + SED.freq(:,i1); 
        else
        SED.degen(i1)=1;
        end
       
    end

%------AVG DEGEN KPTS------------------------------------------------------    

    SED.numdegen(1:SED.numirr)=0;
    SED.freqavg(1:sample_rate,1:SED.numirr)=0;
    for i1=1:SED.numirr
        for i2=1:num_kpts
            if issym(SED.irrkpt(i1,1:3),SED.kpt(i2,1:3)) == 1.0;
                 SED.numdegen(i1) = SED.numdegen(i1) +1.0;
                 SED.freqavg(:,i1) = SED.freqavg(:,i1) + SED.freq(:,i2);
            end
        end
        SED.freqavg(:,i1) = SED.freqavg(:,i1)/SED.numdegen(i1);
    end
%pause   





%------FIND SED PEAKS------------------------------------------------------

%The inline version
% c(1) -> A
% c(2) -> gamma
% c(3) -> w0
func = inline('(c(1)*c(2))./((w - c(3)).^2 + (c(2)/2).^2)','c','w');  

%!!!!!!!!!!!!!!!!!!!!!!!!!!!CHANGE THIS TO ACTUAL FREQUENCY VALUES
w(:,1)=1:length(SED.freqavg);



for i1=1:SED.numirr
    
    %------CALCULATE HLD PEAKS-------------------------------------------------     
    [HLDpks] = LDAr_FCC([LC LC LC],SED.irrkpt(i1,1:3));
    HLDpks = ceil(HLDpks/tau_Ar/w_step);
    
    
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
    threshlist=10:10:150; mpdlist=5:1:20;
    for i2=1:length(threshlist)
        for i3=1:length(mpdlist)
            [pks,loc] = findpeaks(SED.freqavg(:,i1),'threshold',threshlist(i2),'minpeakdistance',mpdlist(i3));
            numpks(i2,i3) = length(pks);
        end
    end    
    subplot(2,1,1), surf(mpdlist,threshlist,numpks)
    plateau = mode(mode(numpks));
    [I,J] = find(mode(mode(numpks))==numpks);
    thresh = ceil((max(I)-min(I))/2);
    minpd = ceil((max(J)-min(J))/2);   
    
    % identify number of peaks, locations and heights for the appropriate
    % threshold, minpeakdistance
    [pks,loc] = findpeaks(SED.freqavg(:,i1),'threshold',threshlist(thresh),'minpeakdistance',mpdlist(minpd));
    %initial parameter guess
    subplot(2,1,2), semilogy(1:length(SED.freqavg(:,i1)),SED.freqavg(:,i1),loc,pks,'.')
    pause
    

    for i2=1:length(loc)

    %Find wleft    
            [I,J] = find(0.5*SED.freqavg(loc(i2),i1)>SED.freqavg(1:loc(i2),i1) );
            wleft = w(I(length(I)));
    %Find wright
            [I,J] = find(0.5*SED.freqavg(loc(i2),i1)>SED.freqavg(loc(i2):length(SED.freqavg),i1) );
            wright = w(I(1)+loc(i2));
            gamma_guess = wright-wleft; buffer = ceil(gamma_guess*0.25);
            c0 = [ gamma_guess*SED.freqavg(loc(i2),i1)/4, gamma_guess, w(loc(i2)) ];
            
            [c,r,j] = nlinfit(w( (wleft-buffer):(wright+buffer)),SED.freqavg((wleft-buffer):(wright+buffer),i1),func,c0);
            plot(w( (wleft-buffer):(wright+buffer)),SED.freqavg((wleft-buffer):(wright+buffer),i1),w( (wleft-buffer):(wright+buffer)),func(c,w( (wleft-buffer):(wright+buffer))))
            pause
    end
    subplot(1,1,1), semilogy(1:length(SED.freqavg(:,i1)),SED.freqavg(:,i1),loc,pks,'.')
    pause
                    clear I J buffer wleft wright c0 pks loc    
end


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
            
