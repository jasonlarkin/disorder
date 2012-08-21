
dummy = importdata('C:\Users\Alex\My Documents\CMU\Research\Mar11\4x\20K\SEDseed_avg.txt','\t');
LC = 1.563;
%LC = 1.579;
temp = 20;

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

% Suppresses scientific notation in all output
format short;


sample_rate = input('Enter sample rate: ');
dim = input('Enter number of unit cells in x,y,z: ');
atoms_simcell = input('Enter number of atoms in sim. cell: ');
pxa = 3*atoms_simcell;
%dummy is a dummy array to store all the SED over all kpts.  It has size
%dummy((1024+1)*numkpts,3).  The portion of the column data that contains
%the SED for different frequencies will have 3 columns, 1 with actual SED
%data the other 2 with NaNs.  Just omit these.
  
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

%------BUILD A COMPLETE LIST OF K-POINTS-----------------------------------
    count = 1;
    for i1=SED.kpt(1,1):-1:SED.kpt(1,1)-dim+1
        for i2=SED.kpt(1,1):-1:SED.kpt(1,1)-dim+1
            for i3=SED.kpt(1,1):-1:SED.kpt(1,1)-dim+1
                SED.kpt_full(count,1) = i1;
                SED.kpt_full(count,2) = i2;
                SED.kpt_full(count,3) = i3;
                count = count + 1;
            end
        end
    end
    
    ntotalkpts = length(SED.kpt_full);
    
    
    
%------BUILD DATA STRUCTURE WHICH WILL STORE PEAK CENTER, LIFETIME, AND
%GROUP VELOCITY FOR EVERY PEAK OF EVERY K-POINT----------------------------

%First index  -> k-point id number
%Second index -> w0
%Third index  -> tau
%Fourth index  -> vg
    count = 1;
    for i1=1:ntotalkpts
        for i2=0:pxa-1
            SED.full_data(count,1) = i1;
            SED.full_data(count,2) = 0;
            SED.full_data(count,3) = 0;
            SED.full_data(count,4) = 0;
            count = count + 1;
        end
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

%------BUILD TEMPORARY DATA STRUCTURE WHICH WILL STORE PEAK CENTER, LIFETIME, AND
%GROUP VELOCITY FOR EVERY PEAK OF THE IRREDUCIBLE K-POINTS-----------------
%First index  -> k-point id number
%Second index -> w0
%Third index  -> tau
%Fourth index  -> vg
    count = 1;
    for i1=1:SED.numirr
        for i2=0:pxa-1
            SED.temp_data(count,1) = i1;
            SED.temp_data(count,2) = 0;
            SED.temp_data(count,3) = 0;
            SED.temp_data(count,4) = 0;
            count = count + 1;
        end
    end

%------FIND SED PEAKS------------------------------------------------------

%The inline version
% c(1) -> A
% c(2) -> gamma
% c(3) -> w0
func1 = inline('((c(1)*c(2))./((w - c(3)).^2 + (c(2)/2).^2))','c','w');
func2 = inline('((c(1)*c(2))./((w - c(3)).^2 + (c(2)/2).^2)) + ((c(4)*c(5))./((w - c(6)).^2 + (c(5)/2).^2))','c','w'); 
func3 = inline('((c(1)*c(2))./((w - c(3)).^2 + (c(2)/2).^2)) + ((c(4)*c(5))./((w - c(6)).^2 + (c(5)/2).^2)) + ((c(7)*c(8))./((w - c(9)).^2 + (c(8)/2).^2))','c','w');
func4 = inline('((c(1)*c(2))./((w - c(3)).^2 + (c(2)/2).^2)) + ((c(4)*c(5))./((w - c(6)).^2 + (c(5)/2).^2)) + ((c(7)*c(8))./((w - c(9)).^2 + (c(8)/2).^2)) + ((c(10)*c(11))./((w - c(12)).^2 + (c(11)/2).^2))','c','w');
func5 = inline('((c(1)*c(2))./((w - c(3)).^2 + (c(2)/2).^2)) + ((c(4)*c(5))./((w - c(6)).^2 + (c(5)/2).^2)) + ((c(7)*c(8))./((w - c(9)).^2 + (c(8)/2).^2)) + ((c(10)*c(11))./((w - c(12)).^2 + (c(11)/2).^2)) + ((c(13)*c(14))./((w - c(15)).^2 + (c(14)/2).^2))','c','w');

%!!!!!!!!!!!!!!!!!!!!!!!!!!!CHANGE THIS TO ACTUAL FREQUENCY VALUES
w(:,1)=1:length(SED.freqavg);


longcounter = 1;
for i1=1:SED.numirr
    
    %------CALCULATE HLD PEAKS-------------------------------------------------
    %In the HLD code, the BZ edge lies at k=[0.5 0 0], so a scaling factor is
    %required
    LDscaling = 1/dim;
    scaledkpt = LDscaling*SED.irrkpt(i1,1:3);
    %The gamma point and any k-point that lies on the BZ edge will not give
    %the correct group velocities when plugged into LDAR_FCCgv, a small
    %correction must be added or subtracted
    if scaledkpt(1)==0 & scaledkpt(2)==0 & scaledkpt(3)==0
        scaledkpt = scaledkpt + 0.01;
    end
    for i=1:3
        if scaledkpt(i)==0.5
            scaledkpt(i)=0.49;
        end
    end
    
    [HLDpks egein vel] = LDAr_FCC([LC LC LC],scaledkpt);
    HLDpks_ind = ceil(HLDpks/tau_Ar/w_step);
    vel = vel*sigma_Ar/tau_Ar;
    
    
    %------FIND # OF NON_DEGENERATE PEAKS--------------------------------------
    numndpks = 1;
    HLDndpks(1) = HLDpks_ind(1);
    for j=2:length(HLDpks_ind) %should be equal to apx (here, 12)
        degencount = 0;
        for k=1:numndpks
            if abs((HLDpks_ind(j)-HLDndpks(k))/HLDndpks(k)) < 0.05
                degencount = 1;
            end
        end
        if degencount == 0
            numndpks = numndpks +1;
            HLDndpks(numndpks) = HLDpks_ind(j);
        end
    end
    
    %------STORE DEGENERACY INFO FOR LATER---------------------------------
    clear pdegen;
    sortedHLDpks_ind = sort(HLDpks_ind);
    pdegen(1,1) = sortedHLDpks_ind(1);
    pdegen(1,2) = 1;
    for i=2:numndpks
        pdegen(i,1) = 0;
        pdegen(i,2) = 0;
    end
    degencount = 1;
    for i=1:length(sortedHLDpks_ind)-1
        if abs((sortedHLDpks_ind(i)-sortedHLDpks_ind(i+1))/sortedHLDpks_ind(i)) < 0.05
            pdegen(degencount,2) = pdegen(degencount,2) + 1;
        else
            degencount = degencount + 1;
            pdegen(degencount,1) = sortedHLDpks_ind(i+1);
            pdegen(degencount,2) = 1;
        end      
    end
    %pause
    
    disp(sprintf('\n\nFitting peaks for k = %d%d%d...\n\n',SED.irrkpt(i1,1),SED.irrkpt(i1,2),SED.irrkpt(i1,3)))

    
    
    %------SCAN THRESH AND MPD-------------------------------------------------     
    threshlist=5:10:145; mpdlist=5:20;
    for i2=1:length(threshlist)
        for i3=1:length(mpdlist)
            [pks,loc] = findpeaks(SED.freqavg(:,i1),'threshold',threshlist(i2),'minpeakdistance',mpdlist(i3));
            numpks(i2,i3) = length(pks);
        end
    end    
    %subplot(2,1,1), surf(mpdlist,threshlist,numpks)
    %plateau = mode(mode(numpks));
    
    % [I,J] = find(mode(mode(numpks))==numpks);
    
    % If scanning 000, DO NOT LOOK FOR THE PEAK AT THE GAMMA POINT, there will be no
    % peak in the SED data at the gamma point
    if i1==SED.numirr
        numndpks = numndpks - 1;
    end
    
    [I,J] = find(numndpks==numpks);
    %thresh = ceil((max(I)+min(I))/2);
    %minpd = ceil((max(J)+min(J))/2);  
    
    thresh = I(1);
    minpd = J(1);
    
    % identify number of peaks, locations and heights for the appropriate
    % threshold, minpeakdistance
    [pks,loc] = findpeaks(SED.freqavg(:,i1),'threshold',threshlist(thresh),'minpeakdistance',mpdlist(minpd));
    %initial parameter guess
    semilogy(1:length(SED.freqavg(:,i1)),SED.freqavg(:,i1),loc,pks,'.')
    
    

    
    
    %Output SED spectrum with peak locations, request user input
    disp(sprintf('\n\nHLD predicts %d non-degenerate peaks',numndpks))
    reply = input('Do these peaks appear correct? (y/n): ','s');
    while strcmp(reply,'n') | strcmp(reply,'N')

        %Identify the peak which was mislabeled
        reply = str2num(input('Which peak was placed incorrectly? (index): ','s'));
        dist = 10000;
        for i=1:length(loc)
            tempdist = abs(reply-loc(i));
            if tempdist < dist
                dist = tempdist;
                badpeak = i;
            end
        end
        %Replace the mislabeled peak with user input peak height and
        %location
        goodpeak = str2num(input('Identify the new peak? (index): ','s'));
        newheight = SED.freqavg(goodpeak,i1);
        pks(badpeak) = newheight;
        loc(badpeak) = goodpeak;
        
        %Re-plot and ask the user for any other peak changes
        semilogy(1:length(SED.freqavg(:,i1)),SED.freqavg(:,i1),loc,pks,'.')        
        reply = input('Do these peaks appear correct? (y/n): ','s');

    end
    
    % Peak data needs to be sorted from smallest to largest peak frequency
    loc = sort(loc);
    for i=1:length(pks)
        pks(i) = SED.freqavg(loc(i),i1);
    end
    
    %pause
    
    
    % --- Find out if some peaks are too close and should be fit with
    % superposed Lorentzians ---
    
    sup_peak_dist = 70;
    width_ratio = 0.2;
    
    clear fitpks
    
    for i=1:length(loc)-1
        dist(i) = loc(i+1) - loc(i);
    end

    id = 1;
    for i=1:length(loc) - 1
        fitpks(i) = id;
        if (dist(i) > sup_peak_dist)
            id = id + 1;
        end
    end
    fitpks(i + 1) = id;
    
    
    %Initialize temp_kptdata for temporary storage of peak centers and 
    %lifetimes. They will then be moved to SED.temp_data after the peaks
    %are unfolded for degeneracy.
    clear temp_kptdata
    for i=1:numndpks
        temp_kptdata(i,1) = 0;
        temp_kptdata(i,2) = 0;
    end
    
    
    loc_counter = 0;
    for i2=1:fitpks(length(fitpks))
        width_ratio = 0.2;
        loc_counter = loc_counter + 1;
        reply = 'n';
        
        % Fit singular peak with one Lorentzian
        if length(find(fitpks==i2)) == 1
            while strcmp(reply,'n') | strcmp(reply,'N')
        %Find wleft and left bound of data set for fitting    
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                wleft = w(I(length(I)));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                fitleft = w(I(length(I)));
        %Find wright and right bound of data set for fitting  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(loc(loc_counter):length(SED.freqavg),i1) );
                wright = w(I(1)+loc(loc_counter));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(loc(loc_counter):length(SED.freqavg),i1) );
                fitright = w(I(1)+loc(loc_counter));
                gamma_guess = wright-wleft;
                c0 = [ gamma_guess*SED.freqavg(loc(loc_counter),i1)/4, gamma_guess, w(loc(loc_counter)) ];
            
                [c,r,j] = nlinfit(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),func1,c0);
                plot(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),w( (fitleft):(fitright)),func1(c,w( (fitleft):(fitright))))
                
                reply = input('Good fit? (y/n): ','s');
                if strcmp(reply,'n') | strcmp(reply,'N')
                    reply_width = input('Enter a new width ratio (decimal): ');
                    width_ratio = reply_width;
                end
            end
            %Print peak center and lifetime to screen
            center=c(3)*w_step/10^12;
            lifetime=10^12/c(2)/w_step;
            fprintf('\nPeak center: %5.2f rad/ps\n', center)
            fprintf('Lifetime: %5.2f ps\n',lifetime)
            %Append peak center and lifetime to temp_kptdata
            temp_kptdata(loc_counter,1) = center;
            temp_kptdata(loc_counter,2) = lifetime;
                
                
                
            %pause
            
        % Fit two peaks with two Lorentzians
        elseif length(find(fitpks==i2)) == 2
            while strcmp(reply,'n') | strcmp(reply,'N')
        %Find wleft of first peak and left bound of data set for fitting    
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                wleft1 = w(I(length(I)));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                fitleft = w(I(length(I)));
        %Find wright of first peak
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(loc(loc_counter):length(SED.freqavg),i1) );
                wright1 = w(I(1)+loc(loc_counter));
        %Find wleft of second peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(1:loc(loc_counter + 1),i1) );
                wleft2 = w(I(length(I)));
        %Find wright of second peak and right bound of data set for fitting
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(loc(loc_counter + 1):length(SED.freqavg),i1) );
                wright2 = w(I(1)+loc(loc_counter + 1));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(loc(loc_counter + 1):length(SED.freqavg),i1) );
                fitright = w(I(1)+loc(loc_counter + 1));
                
        %Make guesses
                gamma_guess1 = wright1-wleft1;
                gamma_guess2 = wright2-wleft2;
                c0 = [ gamma_guess1*SED.freqavg(loc(loc_counter),i1)/4, gamma_guess1, w(loc(loc_counter)), gamma_guess2*SED.freqavg(loc(loc_counter + 1),i1)/4, gamma_guess2, w(loc(loc_counter + 1)) ];
            
                [c,r,j] = nlinfit(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),func2,c0);
                plot(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),w( (fitleft):(fitright)),func2(c,w( (fitleft):(fitright))))
                reply = input('Good fit? (y/n): ','s');
                if strcmp(reply,'n') | strcmp(reply,'N')
                    reply_width = input('Enter a new width ratio (decimal): ');
                    width_ratio = reply_width;
                end
            end
            center1=c(3)*w_step/10^12;
            lifetime1=10^12/c(2)/w_step;
            center2=c(6)*w_step/10^12;
            lifetime2=10^12/c(5)/w_step;
            fprintf('\nPeak(1) center: %5.2f rad/ps\n', center1)
            fprintf('Lifetime(1): %5.2f ps\n',lifetime1)
            fprintf('\nPeak(2) center: %5.2f rad/ps\n', center2)
            fprintf('Lifetime(2): %5.2f ps\n',lifetime2)
            temp_kptdata(loc_counter,1) = center1;
            temp_kptdata(loc_counter,2) = lifetime1;
            temp_kptdata(loc_counter+1,1) = center2;
            temp_kptdata(loc_counter+1,2) = lifetime2;
                
            loc_counter = loc_counter + 1;    
            %pause
            
         % Fit three peaks with three Lorentzians
        elseif length(find(fitpks==i2)) == 3
            while strcmp(reply,'n') | strcmp(reply,'N')
        %Find wleft of first peak and left bound of data set for fitting    
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                wleft1 = w(I(length(I)));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                fitleft = w(I(length(I)));
        %Find wright of first peak
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(loc(loc_counter):length(SED.freqavg),i1) );
                wright1 = w(I(1)+loc(loc_counter));
        %Find wleft of second peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(1:loc(loc_counter + 1),i1) );
                wleft2 = w(I(length(I)));
        %Find wright of second peak
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(loc(loc_counter + 1):length(SED.freqavg),i1) );
                wright2 = w(I(1)+loc(loc_counter + 1));
        %Find wleft of third peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 2),i1)>SED.freqavg(1:loc(loc_counter + 2),i1) );
                wleft3 = w(I(length(I)));
        %Find wright of third peak and right bound of data set for fitting
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 2),i1)>SED.freqavg(loc(loc_counter + 2):length(SED.freqavg),i1) );
                wright3 = w(I(1)+loc(loc_counter + 2));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter + 2),i1)>SED.freqavg(loc(loc_counter + 2):length(SED.freqavg),i1) );
                fitright = w(I(1)+loc(loc_counter + 2));
                
        %Make guesses
                gamma_guess1 = wright1-wleft1;
                gamma_guess2 = wright2-wleft2;
                gamma_guess3 = wright3-wleft3;
                c0 = [ gamma_guess1*SED.freqavg(loc(loc_counter),i1)/4, gamma_guess1, w(loc(loc_counter)), gamma_guess2*SED.freqavg(loc(loc_counter + 1),i1)/4, gamma_guess2, w(loc(loc_counter + 1)), gamma_guess3*SED.freqavg(loc(loc_counter + 2),i1)/4, gamma_guess3, w(loc(loc_counter + 2)) ];
            
                [c,r,j] = nlinfit(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),func3,c0);
                plot(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),w( (fitleft):(fitright)),func3(c,w( (fitleft):(fitright))))
                reply = input('Good fit? (y/n): ','s');
                if strcmp(reply,'n') | strcmp(reply,'N')
                    reply_width = input('Enter a new width ratio (decimal): ');
                    width_ratio = reply_width;
                end
            end
            center1=c(3)*w_step/10^12;
            lifetime1=10^12/c(2)/w_step;
            center2=c(6)*w_step/10^12;
            lifetime2=10^12/c(5)/w_step;
            center3=c(9)*w_step/10^12;
            lifetime3=10^12/c(8)/w_step;
            fprintf('\nPeak(1) center: %5.2f rad/ps\n', center1)
            fprintf('Lifetime(1): %5.2f ps\n',lifetime1)
            fprintf('\nPeak(2) center: %5.2f rad/ps\n', center2)
            fprintf('Lifetime(2): %5.2f ps\n',lifetime2)
            fprintf('\nPeak(3) center: %5.2f rad/ps\n', center3)
            fprintf('Lifetime(3): %5.2f ps\n',lifetime3)
            %Append peak center and lifetime to temp_kptdata
            temp_kptdata(loc_counter,1) = center1;
            temp_kptdata(loc_counter,2) = lifetime1;
            temp_kptdata(loc_counter+1,1) = center2;
            temp_kptdata(loc_counter+1,2) = lifetime2;
            temp_kptdata(loc_counter+2,1) = center3;
            temp_kptdata(loc_counter+2,2) = lifetime3;
                
            loc_counter = loc_counter + 2;
           % pause
            
         % Fit four peaks with four Lorentzians
        elseif length(find(fitpks==i2)) == 4
            while strcmp(reply,'n') | strcmp(reply,'N')
        %Find wleft of first peak and left bound of data set for fitting    
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                wleft1 = w(I(length(I)));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                fitleft = w(I(length(I)));
        %Find wright of first peak
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(loc(loc_counter):length(SED.freqavg),i1) );
                wright1 = w(I(1)+loc(loc_counter));
        %Find wleft of second peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(1:loc(loc_counter + 1),i1) );
                wleft2 = w(I(length(I)));
        %Find wright of second peak
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(loc(loc_counter + 1):length(SED.freqavg),i1) );
                wright2 = w(I(1)+loc(loc_counter + 1));
        %Find wleft of third peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 2),i1)>SED.freqavg(1:loc(loc_counter + 2),i1) );
                wleft3 = w(I(length(I)));
        %Find wright of third peak and right bound of data set for fitting
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 2),i1)>SED.freqavg(loc(loc_counter + 2):length(SED.freqavg),i1) );
                wright3 = w(I(1)+loc(loc_counter + 2));
        %Find wleft of fourth peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 3),i1)>SED.freqavg(1:loc(loc_counter + 3),i1) );
                wleft4 = w(I(length(I)));
        %Find wright of fourth peak and right bound of data set for fitting
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 3),i1)>SED.freqavg(loc(loc_counter + 3):length(SED.freqavg),i1) );
                wright4 = w(I(1)+loc(loc_counter + 3));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter + 3),i1)>SED.freqavg(loc(loc_counter + 3):length(SED.freqavg),i1) );
                fitright = w(I(1)+loc(loc_counter + 3));
                
        %Make guesses
                gamma_guess1 = wright1-wleft1;
                gamma_guess2 = wright2-wleft2;
                gamma_guess3 = wright3-wleft3;
                gamma_guess4 = wright4-wleft4;
                c0 = [ gamma_guess1*SED.freqavg(loc(loc_counter),i1)/4, gamma_guess1, w(loc(loc_counter)), gamma_guess2*SED.freqavg(loc(loc_counter + 1),i1)/4, gamma_guess2, w(loc(loc_counter + 1)), gamma_guess3*SED.freqavg(loc(loc_counter + 2),i1)/4, gamma_guess3, w(loc(loc_counter + 2)),gamma_guess4*SED.freqavg(loc(loc_counter + 3),i1)/4, gamma_guess4, w(loc(loc_counter + 3)) ];
            
                [c,r,j] = nlinfit(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),func4,c0);
                plot(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),w( (fitleft):(fitright)),func4(c,w( (fitleft):(fitright))))
                reply = input('Good fit? (y/n): ','s');
                if strcmp(reply,'n') | strcmp(reply,'N')
                    reply_width = input('Enter a new width ratio (decimal): ');
                    width_ratio = reply_width;
                end
            end
            center1=c(3)*w_step/10^12;
            lifetime1=10^12/c(2)/w_step;
            center2=c(6)*w_step/10^12;
            lifetime2=10^12/c(5)/w_step;
            center3=c(9)*w_step/10^12;
            lifetime3=10^12/c(8)/w_step;
            center4=c(12)*w_step/10^12;
            lifetime4=10^12/c(11)/w_step;
            fprintf('\nPeak(1) center: %5.2f rad/ps\n', center1)
            fprintf('Lifetime(1): %5.2f ps\n',lifetime1)
            fprintf('\nPeak(2) center: %5.2f rad/ps\n', center2)
            fprintf('Lifetime(2): %5.2f ps\n',lifetime2)
            fprintf('\nPeak(3) center: %5.2f rad/ps\n', center3)
            fprintf('Lifetime(3): %5.2f ps\n',lifetime3)
            fprintf('\nPeak(4) center: %5.2f rad/ps\n', center4)
            fprintf('Lifetime(4): %5.2f ps\n',lifetime4)
            %Append peak center and lifetime to temp_kptdata
            temp_kptdata(loc_counter,1) = center1;
            temp_kptdata(loc_counter,2) = lifetime1;
            temp_kptdata(loc_counter+1,1) = center2;
            temp_kptdata(loc_counter+1,2) = lifetime2;
            temp_kptdata(loc_counter+2,1) = center3;
            temp_kptdata(loc_counter+2,2) = lifetime3;
            temp_kptdata(loc_counter+3,1) = center4;
            temp_kptdata(loc_counter+3,2) = lifetime4;
              
            loc_counter = loc_counter + 3;
          %  pause
        % Fit five peaks with four Lorentzians
        elseif length(find(fitpks==i2)) == 5
            while strcmp(reply,'n') | strcmp(reply,'N')
        %Find wleft of first peak and left bound of data set for fitting    
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                wleft1 = w(I(length(I)));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(1:loc(loc_counter),i1) );
                fitleft = w(I(length(I)));
        %Find wright of first peak
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter),i1)>SED.freqavg(loc(loc_counter):length(SED.freqavg),i1) );
                wright1 = w(I(1)+loc(loc_counter));
        %Find wleft of second peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(1:loc(loc_counter + 1),i1) );
                wleft2 = w(I(length(I)));
        %Find wright of second peak
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 1),i1)>SED.freqavg(loc(loc_counter + 1):length(SED.freqavg),i1) );
                wright2 = w(I(1)+loc(loc_counter + 1));
        %Find wleft of third peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 2),i1)>SED.freqavg(1:loc(loc_counter + 2),i1) );
                wleft3 = w(I(length(I)));
        %Find wright of third peak and right bound of data set for fitting
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 2),i1)>SED.freqavg(loc(loc_counter + 2):length(SED.freqavg),i1) );
                wright3 = w(I(1)+loc(loc_counter + 2));
        %Find wleft of fourth peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 3),i1)>SED.freqavg(1:loc(loc_counter + 3),i1) );
                wleft4 = w(I(length(I)));
        %Find wright of fourth peak and right bound of data set for fitting
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 3),i1)>SED.freqavg(loc(loc_counter + 3):length(SED.freqavg),i1) );
                wright4 = w(I(1)+loc(loc_counter + 3));
        %Find wleft of fifth peak  
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 4),i1)>SED.freqavg(1:loc(loc_counter + 4),i1) );
                wleft5 = w(I(length(I)));
        %Find wright of fifth peak and right bound of data set for fitting
                [I,J] = find(0.5*SED.freqavg(loc(loc_counter + 4),i1)>SED.freqavg(loc(loc_counter + 4):length(SED.freqavg),i1) );
                wright5 = w(I(1)+loc(loc_counter + 4));
                [I,J] = find(width_ratio*SED.freqavg(loc(loc_counter + 4),i1)>SED.freqavg(loc(loc_counter + 4):length(SED.freqavg),i1) );
                fitright = w(I(1)+loc(loc_counter + 4));
                
        %Make guesses
                gamma_guess1 = wright1-wleft1;
                gamma_guess2 = wright2-wleft2;
                gamma_guess3 = wright3-wleft3;
                gamma_guess4 = wright4-wleft4;
                gamma_guess5 = wright5-wleft5;
                c0 = [ gamma_guess1*SED.freqavg(loc(loc_counter),i1)/4, gamma_guess1, w(loc(loc_counter)), gamma_guess2*SED.freqavg(loc(loc_counter + 1),i1)/4, gamma_guess2, w(loc(loc_counter + 1)), gamma_guess3*SED.freqavg(loc(loc_counter + 2),i1)/4, gamma_guess3, w(loc(loc_counter + 2)),gamma_guess4*SED.freqavg(loc(loc_counter + 3),i1)/4, gamma_guess4, w(loc(loc_counter + 3)),gamma_guess5*SED.freqavg(loc(loc_counter + 4),i1)/4, gamma_guess5, w(loc(loc_counter + 4)) ];
            
                [c,r,j] = nlinfit(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),func5,c0);
                plot(w( (fitleft):(fitright)),SED.freqavg((fitleft):(fitright),i1),w( (fitleft):(fitright)),func5(c,w( (fitleft):(fitright))))
                reply = input('Good fit? (y/n): ','s');
                if strcmp(reply,'n') | strcmp(reply,'N')
                    reply_width = input('Enter a new width ratio (decimal): ');
                    width_ratio = reply_width;
                end
            end
            center1=c(3)*w_step/10^12;
            lifetime1=10^12/c(2)/w_step;
            center2=c(6)*w_step/10^12;
            lifetime2=10^12/c(5)/w_step;
            center3=c(9)*w_step/10^12;
            lifetime3=10^12/c(8)/w_step;
            center4=c(12)*w_step/10^12;
            lifetime4=10^12/c(11)/w_step;
            center5=c(15)*w_step/10^12;
            lifetime5=10^12/c(14)/w_step;
            fprintf('\nPeak(1) center: %5.2f rad/ps\n', center1)
            fprintf('Lifetime(1): %5.2f ps\n',lifetime1)
            fprintf('\nPeak(2) center: %5.2f rad/ps\n', center2)
            fprintf('Lifetime(2): %5.2f ps\n',lifetime2)
            fprintf('\nPeak(3) center: %5.2f rad/ps\n', center3)
            fprintf('Lifetime(3): %5.2f ps\n',lifetime3)
            fprintf('\nPeak(4) center: %5.2f rad/ps\n', center4)
            fprintf('Lifetime(4): %5.2f ps\n',lifetime4)
            fprintf('\nPeak(5) center: %5.2f rad/ps\n', center5)
            fprintf('Lifetime(5): %5.2f ps\n',lifetime5)
            temp_kptdata(loc_counter,1) = center1;
            temp_kptdata(loc_counter,2) = lifetime1;
            temp_kptdata(loc_counter+1,1) = center2;
            temp_kptdata(loc_counter+1,2) = lifetime2;
            temp_kptdata(loc_counter+2,1) = center3;
            temp_kptdata(loc_counter+2,2) = lifetime3;
            temp_kptdata(loc_counter+3,1) = center4;
            temp_kptdata(loc_counter+3,2) = lifetime4;
            temp_kptdata(loc_counter+4,1) = center5;
            temp_kptdata(loc_counter+4,2) = lifetime5;
                
          %  loc_counter = loc_counter + 4;
           % pause
            else
                t = sprintf('Six or more peaks are too close!');
                disp(t)
        end   
    end
    
%-------------START DEGENERACY UNFOLDING-------------------------------
%Start first by copying data in temp_kptdata to SED.temp_data by
%the corresponding factor in pdegen
    vel_counter = 1;
    if i1<SED.numirr
        for i=1:numndpks
            for j=1:pdegen(i,2)
                SED.temp_data(longcounter,2) = HLDpks(vel_counter)/tau_Ar/10^12;
                SED.temp_data(longcounter,3) = temp_kptdata(i,1);
                SED.temp_data(longcounter,4) = temp_kptdata(i,2);
                SED.temp_data(longcounter,5) = vel(vel_counter,1);
                SED.temp_data(longcounter,6) = vel(vel_counter,2);
                SED.temp_data(longcounter,7) = vel(vel_counter,3);
                SED.temp_data(longcounter,8) = vel(vel_counter,1)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                SED.temp_data(longcounter,9) = vel(vel_counter,2)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                SED.temp_data(longcounter,10) = vel(vel_counter,3)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                longcounter = longcounter + 1;
                vel_counter = vel_counter + 1;
            end
            
        end
    else
        for i=1:numndpks + 1
            for j=1:pdegen(i,2)
                if i==1
                    SED.temp_data(longcounter,2) = HLDpks(vel_counter)/tau_Ar/10^12;
                    SED.temp_data(longcounter,3) = 0;
                    SED.temp_data(longcounter,4) = 0;
                    SED.temp_data(longcounter,5) = vel(vel_counter,1);
                    SED.temp_data(longcounter,6) = vel(vel_counter,2);
                    SED.temp_data(longcounter,7) = vel(vel_counter,3);
                    SED.temp_data(longcounter,8) = vel(vel_counter,1)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                    SED.temp_data(longcounter,9) = vel(vel_counter,2)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                    SED.temp_data(longcounter,10) = vel(vel_counter,3)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                    longcounter = longcounter + 1;
                    vel_counter = vel_counter + 1;
                else
                    SED.temp_data(longcounter,2) = HLDpks(vel_counter)/tau_Ar/10^12;
                    SED.temp_data(longcounter,3) = temp_kptdata(i,1);
                    SED.temp_data(longcounter,4) = temp_kptdata(i,2);
                    SED.temp_data(longcounter,5) = vel(vel_counter,1);
                    SED.temp_data(longcounter,6) = vel(vel_counter,2);
                    SED.temp_data(longcounter,7) = vel(vel_counter,3);
                    SED.temp_data(longcounter,8) = vel(vel_counter,1)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                    SED.temp_data(longcounter,9) = vel(vel_counter,2)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                    SED.temp_data(longcounter,10) = vel(vel_counter,3)*temp_kptdata(i,1)/(HLDpks(vel_counter)/tau_Ar/10^12);
                    vel_counter = vel_counter + 1;
                    longcounter = longcounter + 1;
                end
            end
        end
    end
    
    
    semilogy(1:length(SED.freqavg(:,i1)),SED.freqavg(:,i1),loc,pks,'.')
    %pause
                    clear I J buffer wleft wright c0 pks loc    
end


%-------------CONTINUE DEGENERACY UNFOLDING-------------------------------
%We now want to match every block of pxa (here, 12) peak parameters stored
%in SED.temp_data to every original k-point with which they are symmetric
%(e.g. 200 in SED.temp_data -> 200, 020, and 002 in SED.full_data)

for i=1:SED.numirr
    simcount(i) = 0;
    for j=1:dim*dim*dim
        if issym(SED.irrkpt(i,1:3),SED.kpt_full(j,1:3)) == 1.0
            %for k=1:pxa
                %SED.full_data((j-1)*pxa+k,2) = SED.temp_data((i-1)*pxa+k,2);
                %SED.full_data((j-1)*pxa+k,3) = SED.temp_data((i-1)*pxa+k,3);
            %end
            simcount(i) = simcount(i) + 1;
        end
    end
end

fillcount = 1;
for i=1:SED.numirr
    for j=1:pxa
        SED.temp_data(fillcount,1) = simcount(i);
        fillcount = fillcount + 1;
    end
end
    

