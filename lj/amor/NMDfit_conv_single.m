clear all
%function SEDfit_JL_LAMMPS_051511

%--------------INPUT-------------------------------------------------------

%LJ Potential and Material Parameters
epsilon_Ar = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
sigma_Ar = 3.4E-10;                 %Angstroms 3.4E-10 meters
mass_Ar = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
%mass_Ar = mass_Ar/mass_Ar;
tau_Ar = sqrt((mass_Ar*(sigma_Ar^2))/epsilon_Ar);
kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
hbar = 1.054E-34;                %J/s



%--------------------------------------------------------------------------
    str_main=strcat('/home/jason/lammps/LJ/amorphous/4x/AF/NMD/NMD/');
%--------------------------------------------------------------------------
    
%KPTLIST: Load kpt list
    str_read=strcat(str_main,'kptlist.dat');
    SED.kptlist(:,1:3) = load(str_read); [NUM_KPTS, blank] = size(SED.kptlist(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
    str_read=strcat(str_main,'x0.data');
    x0 = load(str_read);
    %Define number of atoms
    NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL;
    NUM_UCELL_INX = (NUM_ATOMS/NUM_ATOMS_UCELL)^(1/3);
    %Define box size and conventional cell lattice parameters
    L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); LC = L(1)/(NUM_UCELL_INX);
    %chop off first line of input structure
    x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
    str_read=strcat(str_main,'SED_param.dat');
    SEDparam = load(str_read);

    t_total = SEDparam(1); t_step = SEDparam(2);
    dt = SEDparam(3);  NUM_FFTS = SEDparam(4); NUM_SEEDS = SEDparam(5);
    
    SEEDS = [2 3];
    
    w_step = 2*pi/(t_total*dt*tau_Ar); w_max = 2*pi/(t_step*dt*2*tau_Ar);
    w_plot = 2^10;  
    NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step);
    %SET VOLUME FOR KAPPA    
    VOLUME = (NUM_UCELL_INX*LC*sigma_Ar)^3;
    NUM_MODES = NUM_ATOMS_UCELL*3;
    SAMPLE_RATE = w_max/w_step;
    %NUM_UCELLS = (NUM_ATOMS/NUM_ATOMS_UCELL)^(1/3);

%FREQUENCIES
    %Windoze
    str_read=strcat(str_main,'freq.dat');
    %Linux
    %str_read=strcat(str_main,'NMD/1atom/freq.dat');
    freq = load(str_read);
%GROUP VEL
    %Windoze
    str_read=strcat(str_main,'vel.dat');
    %Linux
    %str_read=strcat(str_main,'NMD/1atom/freq.dat');
    group_vel = load(str_read);
    
%--------------------------------------------------------------------------    
%------AVG OVER SEEDS and NOT KPTS-----------------------------------------     
%-------------------------------------------------------------------------- 
        SED.irrkpt.sedavg(1:SAMPLE_RATE,1:NUM_MODES,1:NUM_KPTS) = 0.0;  
        SED.irrkpt.HLDfreq(1:NUM_MODES,1:NUM_KPTS) = 0.0;
        SED.irrkpt.HLDvel(1:NUM_MODES,1:3,1:NUM_KPTS) = 0.0;
%------AVG OVER SEEDS------------------------------------------------------
for ikpt=1:NUM_KPTS
    for iseed=1:NUM_SEEDS
        SED.irrkpt.kpt(ikpt,1:3) = SED.kptlist(ikpt,1:3);
        str_read_single=strcat(str_main,'SED_',num2str(SED.kptlist(ikpt,1)),...
        num2str(SED.kptlist(ikpt,2)),num2str(SED.kptlist(ikpt,3)),'_',...
        int2str(SEEDS(iseed)),'.txt');
        dummy = dlmread(str_read_single);
        SED.irrkpt.sedavg(:,:,ikpt) = dummy(:,2:(NUM_MODES+1)) + SED.irrkpt.sedavg(:,:,ikpt);
    end
    SED.irrkpt.sedavg(:,:,:) = SED.irrkpt.sedavg(:,:,:)/NUM_SEEDS;
   
    
    SED.irrkpt.HLDvel(1:NUM_MODES,1:3,ikpt) = ...
    group_vel((ikpt-1)*NUM_ATOMS_UCELL*3+1:(ikpt)*NUM_ATOMS_UCELL*3,1:3);
    SED.irrkpt.HLDfreq(1:NUM_MODES,ikpt) = ...
    freq(ikpt,1:NUM_MODES)';   
end


%--------------------------------------------------------------------------
%pause   
%--------------------------------------------------------------------------                 


%BUILD MAIN LIST OF IRRKPTS TO RUN OVER    
    kpt_list = 1:NUM_KPTS;
for ikpt=1:length(kpt_list) 
    
    %How far down to fit the Lor
    PT_PERC=1.0;
    %Initial guess for peak height above max(SED).
    PEAK_PERC = 1.5;
    gamma_guess = 20;
    HLD_SCALING_PCT = 1.0;
    
        %PRINT CURRENT KPT
        SED.irrkpt.kpt(kpt_list(ikpt),1:3)
        %CALCULATE HLD INFO

         kx = SED.irrkpt.kpt(kpt_list(ikpt),1)/NUM_UCELL_INX;
         ky = SED.irrkpt.kpt(kpt_list(ikpt),2)/NUM_UCELL_INX;
         kz = SED.irrkpt.kpt(kpt_list(ikpt),3)/NUM_UCELL_INX;      
         
    %Print HLD freqs
    w_guess = ceil(SED.irrkpt.HLDfreq(1:NUM_MODES,kpt_list(ikpt))/tau_Ar/w_step*HLD_SCALING_PCT)

if kx==0 & ky==0 & kz==0
    SED.irrkpt.sedfreq( 1:NUM_MODES ,kpt_list(ikpt)) = 0;
    %Store in ps
    SED.irrkpt.life( 1:NUM_MODES ,kpt_list(ikpt)) = 0; 
else
    for imode = 1:NUM_MODES
       
        SED.irrkpt.sedavg(:,imode,kpt_list(ikpt)) = ...
            SED.irrkpt.sedavg(:,imode,kpt_list(ikpt))/max(SED.irrkpt.sedavg(:,imode,kpt_list(ikpt)));
        
        semilogy(1:w_plot,SED.irrkpt.sedavg(1:w_plot,imode,kpt_list(ikpt)),'.')
%Set freq to 1:sample_size
        w(:,1)=(1:length(SED.irrkpt.sedavg(:,imode,kpt_list(ikpt))));   
%LORENTZIAN FUNCTION FOR SINGLE AND MULTIPLE PEAKS 
        [Ipeak,Jpeak]=max(SED.irrkpt.sedavg(:,imode,kpt_list(ikpt)));
    %Find wleft fitting range    
        [I,J] = find(SED.irrkpt.sedavg(1:w_guess(imode),imode,kpt_list(ikpt))<PT_PERC*SED.irrkpt.sedavg(Jpeak,imode,kpt_list(ikpt)));
        wleft = w(I(length(I)));
%	 wleft = 1;
    %Find wright fitting range
        [I,J] = find(SED.irrkpt.sedavg(w_guess(imode):length(w),imode,kpt_list(ikpt))>PT_PERC*SED.irrkpt.sedavg(Jpeak,imode,kpt_list(ikpt)));
        wright = w_guess(imode) + I(length(I));
%	 wright = 1024;


            
        [Imax,Jmax]=max(SED.irrkpt.sedavg(wleft:wright,imode,kpt_list(ikpt)));
        %weights=SED.irrkpt.sedavg(wleft:wright,kpt_list(ikpt),imode)/Imax;
%DEFINE POSSIBLE WEIGHTS
    weights = ones(length(w(wleft:wright)),1);
%DEFINE LORENTZIAN FUNCTION
    lor_func = @(c,w)weights.*(c(1))./(1 + ( (w - c(3))./ c(2) ).^2 );
%DEFINE THE FITTING PARAMETERS
    options = optimset('MaxIter',10000,'MaxFunEvals',10000,'ScaleProblem','Jacobian'); 
    [Ipeak,Jpeak]=max(SED.irrkpt.sedavg(:,imode,kpt_list(ikpt)));
    c0 = [ PEAK_PERC*Ipeak, gamma_guess, Jpeak ]; 
%FIT THE LORENTZIAN(S)
    %[c_fit,r,j] = nlinfit(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(ikpt)),lor_func,c);
    lb(1:length(c0)) = 0.0; 
    ub(1:3:length(c0)) = PEAK_PERC*Ipeak; 
    ub(2:3:length(c0)) = 100; 
    ub(3:3:length(c0)) = w(length(w));
    [c_fit] = lsqcurvefit(lor_func,c0,w(wleft:wright), ...
        SED.irrkpt.sedavg(wleft:wright,imode,kpt_list(ikpt)).*weights,lb,ub,options);
%Store separate liftimes and frequencies for single and MULTIPLE FITS
    center=c_fit(3)*w_step;
    lifetime=1/(2*c_fit(2))/w_step;
    %Store in rads/ps
    SED.irrkpt.sedfreq( imode ,kpt_list(ikpt)) = center;
    %Store in ps
    SED.irrkpt.life( imode ,kpt_list(ikpt)) = lifetime;
%Plot each fit, single and multiple
    semilogy(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,imode,kpt_list(ikpt)),'.', ... 
        w(wleft:wright),lor_func(c_fit,w(wleft:wright))./weights)
    axis([0 w_plot 0.00001*max(SED.irrkpt.sedavg(:,imode,kpt_list(ikpt))) max(SED.irrkpt.sedavg(:,imode,kpt_list(ikpt)))]);
%-----------------------------------------------------------
%pause
%-----------------------------------------------------------
    end
        
    disp(sprintf('%s', 'SED FREQ:'));
    SED.irrkpt.sedfreq(:,kpt_list(ikpt))
    disp(sprintf('%s', 'SED LIFE:'));
    SED.irrkpt.life(:,kpt_list(ikpt))
 
    clear I J buffer wleft wright c0 pks loc str_func lifetime center
end
end %END KPTLSIT
    


%--------------------OUTPUT DATA-------------------------------------------  

%str_loop = input('Write data to file?: ');
str_loop = 1;
str_write=strcat(str_main,'SED_data.dat');
str_write_life=strcat(str_main,'SED_life.dat');
if str_loop ==1
    NUM_MODES = length(SED.irrkpt.HLDfreq(:,1));
    for ikpt=1:NUM_KPTS
        %Write the wavevector first
        dlmwrite(str_write,SED.irrkpt.kpt(ikpt,1:3),'-append');

        for imode=1:NUM_MODES
            %Convert to rads/s
            whld = SED.irrkpt.HLDfreq(imode,ikpt)/tau_Ar;
            wsed = SED.irrkpt.sedfreq(imode,ikpt)/tau_Ar;
            lifetau = SED.irrkpt.life(imode,ikpt);
            %Convert to m/s
            velx = SED.irrkpt.HLDvel(imode,1,ikpt)*(sigma_Ar/tau_Ar);
            vely = SED.irrkpt.HLDvel(imode,2,ikpt)*(sigma_Ar/tau_Ar);
            velz = SED.irrkpt.HLDvel(imode,3,ikpt)*(sigma_Ar/tau_Ar);
            kappax = (kb/VOLUME)*lifetau*(velx^2);
            kappay = (kb/VOLUME)*lifetau*(vely^2);
            kappaz = (kb/VOLUME)*lifetau*(velz^2);
            
            output_format = [whld wsed lifetau velx vely velz kappax kappay kappaz];
            output_life = [whld wsed lifetau velx vely velz kappax kappay kappaz];
            dlmwrite(str_write,output_format,'-append');
            dlmwrite(str_write_life,output_life,'-append');
        end
    end
end

%END MAIN FUNCTION    
%end



%-------------FUNCTIONS----------------------------------------------------


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


