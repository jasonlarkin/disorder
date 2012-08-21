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

%Set main directory and read in first file
    str.main=strcat('/home/jason/lammps/LJ/4x/NMD/conv/');
%KPTLIST: Load kpt list
    str.read=strcat(str.main,'kptlist.dat');
    SED.kptlist.master(:,1:3) = load(str.read); 
    [NUM_KPTS, blank] = size(SED.kptlist.master(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
    str.read=strcat(str.main,'x0.data');
    x0 = load(str.read);
%Define number of atoms
    NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); 
    NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL; 
%Define box size and conventional cell lattice parameters
NUM_UCELL_INX = (NUM_ATOMS/4)^(1/3);
    L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); LC = L(1)/(NUM_UCELL_INX);
%chop off first line of input structure
    x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
    str.read=strcat(str.main,'SED_param.dat');
    SEDparam = load(str.read);
    t_total = SEDparam(1); t_step = SEDparam(2); dt = SEDparam(3);  
    NUM_FFTS = SEDparam(4); NUM_SEEDS = SEDparam(5);
    w_step = 2*pi/(t_total*dt*tau_Ar); w_max = 2*pi/(t_step*dt*2*tau_Ar);
    w_plot = 2^10;  
    NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step);
    NUM_MODES = NUM_ATOMS_UCELL*3; NUM_UCELLS = (NUM_ATOMS/NUM_ATOMS_UCELL)^(1/3);
    
%Perfect 20K
    HLD_SCALING_PCT=1.0;
%SET VOLUME FOR KAPPA    
    VOLUME = L(1)*L(2)*L(3);
    SAMPLE_RATE = w_max/w_step;
    
pause

    str=strcat(str.main,'SED_single_1.txt');
        dummy = dlmread(str,' ');  
            %This figures out how many kpts are being read in by checking the length
            %of dummy and dividing by the number of kpt sections SAMPLE_RATE+1.  
            %we've been having SAMPLE_RATE=1024;
        %NUM_KPTS = floor(length(dummy(:,1)) / (SAMPLE_RATE+1));
        SED.redkpt.sed(1:SAMPLE_RATE,1:NUM_KPTS,1:NUM_MODES) = 0.0;
        
 %Create symmetry kpts
 cnt = 1;
    for i1=-1:2:1
        for i2=-1:2:1
            for i3=-1:2:1 
            SED.kptlist.sym(:,1:3,cnt) = [i1*SED.kptlist.master(:,1) ...
                i2*SED.kptlist.master(:,2) i3*SED.kptlist.master(:,3)];
            [TF(:,cnt),LOC(:,cnt)] = ismember(SED.kptlist.master, ... 
                SED.kptlist.sym(:,:,cnt),'rows');
            cnt = cnt+1;
            end
        end
    end
    
    
    
        
%------AVG OVER SEEDS------------------------------------------------------

    for i1=1:NUM_SEEDS
    str=strcat(str.main,'SED_single_',int2str(i1),'.txt');
        for ikpt=1:NUM_KPTS
            for imode = 1:NUM_MODES
                SED.redkpt.sed(:,ikpt,imode) = SED.redkpt.sed(:,ikpt,imode) + dummy((ikpt-1)*(SAMPLE_RATE+1)+2:(ikpt)*(SAMPLE_RATE+1),imode+1);
                SED.redkpt.kpt(ikpt,1:3) = dummy((ikpt-1)*(SAMPLE_RATE+1)+1,1:3);
            end
        end
    end
    SED.redkpt.sed(:,:,:) = SED.redkpt.sed(:,:,:)/NUM_SEEDS;

  

%------FIND NUMBER IRR KPTS AND AVG------------------------------------ 

%REWRITE THE DEGENERACY ROUTINE USING ISMEMBER AND BSXFUN

        % Identify irreducible k-points, store in SED.irrkpt
        % The first kpt is automatically a irreducible point.
        SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=1; SED.irrkpt.numirr=1;
        SED.irrkpt.sedavg(1:SAMPLE_RATE,SED.irrkpt.numirr,1:NUM_MODES)=0;
        %str_write=strcat(str.main,'redkpt.dat');
        %dlmwrite(str_write,SED.redkpt.kpt(1,1:3),'-append');
        for i1=2:NUM_KPTS
            tempcnt = 0.0;
            %dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
            for i2=1:SED.irrkpt.numirr 
                if issym(SED.irrkpt.kpt(i2,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
                     tempcnt = 1.0;
                     idegen = i2;
                end
            end
            
            if tempcnt==0.0 %didn't find in the irrkpt list, its a new point
            SED.irrkpt.numirr = SED.irrkpt.numirr +1;
            SED.irrkpt.kpt(SED.irrkpt.numirr,1:3) = SED.redkpt.kpt(i1,1:3);
            SED.irrkpt.sedavg(:,SED.irrkpt.numirr,1:NUM_MODES) = SED.redkpt.sed(:,i1,1:NUM_MODES);
            SED.irrkpt.numdegen(SED.irrkpt.numirr) = 1;
            SED.redkpt.degen(i1)=SED.irrkpt.numirr;
            else %found it in the irrkpt list, add it as a degen
            SED.irrkpt.numdegen(idegen) = SED.irrkpt.numdegen(idegen) +1;
            SED.irrkpt.sedavg(:,idegen,1:NUM_MODES) = SED.irrkpt.sedavg(:,idegen,1:NUM_MODES) + SED.redkpt.sed(:,i1,1:NUM_MODES);
            SED.redkpt.degen(i1)=idegen;
            end  
        end
 %AVG over degen pts       
    for i2=1:SED.irrkpt.numirr         
        SED.irrkpt.sedavg(:,i2,1:NUM_MODES) = SED.irrkpt.sedavg(:,i2,1:NUM_MODES)./SED.irrkpt.numdegen(i2);
    end
           
%-----------------------------------------------------        
%pause   
%-----------------------------------------------------                     

%RE-DO LIST
    REDO=0; 
    AUTO=1;
%BUILD MAIN LIST OF IRRKPTS TO RUN OVER    
    kpt_list = 1:SED.irrkpt.numirr;
    RUN=1;
while RUN==1
    for kpt_cnt=1:length(kpt_list)      
        %PRINT CURRENT KPT
                    SED.irrkpt.kpt(kpt_list(kpt_cnt),1:3)
        %CALCULATE HLD INFO
        KPT_TOL = 10E-6; 
        DEGEN_WIDTH=3.0;
         kx = SED.irrkpt.kpt(kpt_list(kpt_cnt),1)/NUM_SUPERCELL;
         ky = SED.irrkpt.kpt(kpt_list(kpt_cnt),2)/NUM_SUPERCELL;
         kz = SED.irrkpt.kpt(kpt_list(kpt_cnt),3)/NUM_SUPERCELL;      
            [HLDpks,eig,vel] = LDAr_FCC([LC LC LC],[kx ky kz]);
            %Convert to rads/s
            SED.irrkpt.HLDfreq(1:length(HLDpks),kpt_list(kpt_cnt)) = HLDpks/tau_Ar;
            %Convert to m/s
            SED.irrkpt.HLDvel(1:length(HLDpks),1:3,kpt_list(kpt_cnt)) = vel*(sigma_Ar/tau_Ar);
            %Print HLD freqs
            w_guess = ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT)
        %FIND # OF NON_DEGENERATE HLD PEAKS
                    %[HLDndpks,numndpks] = ndpks_HLD(ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT),DEGEN_WIDTH);
if kx==0 & ky==0 & kz==0
        SED.irrkpt.sedfreq( 1:NUM_MODES ,kpt_list(kpt_cnt)) = 0;
        %Store in ps
        SED.irrkpt.life( 1:NUM_MODES ,kpt_list(kpt_cnt)) = 0; 
        SED.irrkpt.flag(kpt_list(kpt_cnt))=0;
else
    for imode = 1:NUM_MODES
        semilogy(1:w_plot,SED.irrkpt.sedavg(1:w_plot,kpt_list(kpt_cnt),imode),'.')
%Set freq to 1:sample_size
            w(:,1)=(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt),imode)));   
%LORENTZIAN FUNCTION FOR SINGLE AND MULTIPLE PEAKS 
            %PT_PERC=input('Enter PERCENT PAST PEAKS: ');
            [Ipeak,Jpeak]=max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt),imode));
            PT_PERC=0.001;
            %Find wleft    
                    [I,J] = find(SED.irrkpt.sedavg(1:w_guess(imode),kpt_list(kpt_cnt),imode)<PT_PERC*SED.irrkpt.sedavg(Jpeak,kpt_list(kpt_cnt),imode));
                    wleft = w(I(length(I)));
            %Find wright
                    [I,J] = find(SED.irrkpt.sedavg(w_guess(imode):length(w),kpt_list(kpt_cnt),imode)>PT_PERC*SED.irrkpt.sedavg(Jpeak,kpt_list(kpt_cnt),imode));
                    wright = w_guess(imode) + I(length(I));
                    %gamma_guess = wright-wleft; 
                    gamma_guess = 1;
            
            [Imax,Jmax]=max(SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt),imode));
            %weights=SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt),imode)/Imax;
            weights = ones(length(w(wleft:wright)),1);
            str_func= '';
            %strtemp= strcat('(c(1))./(1 + ( (w - c(3))./ c(2) ).^2 )');
            %str_func = strcat(str_func,strtemp);
            %lor_func = inline(str_func,'c','w');
            lor_func = @(c,w)weights.*(c(1))./(1 + ( (w - c(3))./ c(2) ).^2 );

            options = optimset('MaxIter',10000,'MaxFunEvals',10000,'ScaleProblem','Jacobian'); 
            [Ipeak,Jpeak]=max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt),imode));
            c0 = [ Ipeak*10, gamma_guess, Jpeak ]; 
%FIT THE LORENTZIAN(S)
            %[c_fit,r,j] = nlinfit(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt)),lor_func,c);
            lb(1:length(c0)) = 0.0; ub(1:3:length(c0)) = max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)))*10; ub(2:3:length(c0)) = 100; ub(3:3:length(c0)) = w(length(w));
            [c_fit] = lsqcurvefit(lor_func,c0,w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt),imode).*weights,lb,ub,options);

%Store separate liftimes and frequencies for single and MULTIPLE FITS
            %Hold on to plot multiple peaks
            %hold on
            center=c_fit(3)*w_step;
            lifetime=1/(2*c_fit(2))/w_step;
            %Store in rads/ps
            SED.irrkpt.sedfreq( imode ,kpt_list(kpt_cnt)) = center;
            %Store in ps
            SED.irrkpt.life( imode ,kpt_list(kpt_cnt)) = lifetime;

%Plot each fit, single and multiple
    semilogy(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt),imode),'.',w(wleft:wright),lor_func(c_fit,w(wleft:wright))./weights)
                        %axis([0 1.0*length(w) 0.0001*max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))) max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)))]);
    axis([0 w_plot 0.00001*max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt),imode)) max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt),imode))]);
%-----------------------------------------------------------
pause
%-----------------------------------------------------------
    end
        
        %Calculate integral of SED = 2TE
            SED.irrkpt.sedintegral(kpt_list(kpt_cnt)) = (sigma_Ar*tau_Ar)*trapz(w*(w_step*5E-15),SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)));
        
                disp(sprintf('%s', 'SED FREQ:'));
                SED.irrkpt.sedfreq(:,kpt_list(kpt_cnt))
                disp(sprintf('%s', 'SED LIFE:'));
                SED.irrkpt.life(:,kpt_list(kpt_cnt))
        %1=yes, 0=no
        %SED.irrkpt.flag(kpt_list(kpt_cnt)) = input('Flag?: ');
        SED.irrkpt.flag(kpt_list(kpt_cnt))=0;
        %Clear current plot
        clf
        hold off
    clear I J buffer wleft wright c0 pks loc str_func lifetime center
end
    end %END KPTLSIT
    

    %Check for any flags, if so redo those kpts manually
        for i1=1:length(SED.irrkpt.flag(:))
            Iflag = find(SED.irrkpt.flag==1);
            if length(Iflag)==0;
                RUN=0;
            else
                RUN=1; AUTO=0; REDO=1; kpt_list = Iflag;
                %disp(sprintf('%s', 'REDO:'));
            end
        end
    
end



%--------------------OUTPUT DATA-------------------------------------------  

    %str_loop = input('Write data to file?: ');
    str_loop = 1;
    str_write=strcat(str.main,'SED_data.dat');
    str_write_life=strcat(str.main,'SED_life.dat');
    str_write_integral=strcat(str.main,'SED_integral.dat');
    if str_loop ==1
        NUM_MODES = length(SED.irrkpt.HLDfreq(:,1));
                %------FIND RED KPTS------------------------------------------------ 

            for i1=1:NUM_KPTS
                for kpt_cnt=1:SED.irrkpt.numirr
                        if issym(SED.irrkpt.kpt(kpt_cnt,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
                            %Write the wavevector first
                            dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
                            %Write the number of degenerate kpts for mult factor
                            %dlmwrite(str,SED.irrkpt.numdegen(kpt_list(kpt_cnt)),'-append')
                            %;
                            
                            %CALCULATE CONTRIBUTION TO TE INTEGRAL
                            SED.redkpt.sedintegral(i1) = SED.irrkpt.sedintegral(kpt_cnt);
                            dlmwrite(str_write_integral,SED.redkpt.sedintegral(i1),'-append');
                            
                            
                            %CALCULATE HLD DATA FOR RED KPT
                                kx = SED.redkpt.kpt(i1,1)/NUM_SUPERCELL;
                                ky = SED.redkpt.kpt(i1,2)/NUM_SUPERCELL;
                                kz = SED.redkpt.kpt(i1,3)/NUM_SUPERCELL;
                                if kx==0.5; kx = kx-KPT_TOL; end
                                if ky==0.5; ky = ky-KPT_TOL; end
                                if kz==0.5; kz = kz-KPT_TOL; end

                                [HLDpks,eig,vel] = LDAr_FCC([LC LC LC],[kx ky kz]);
                                %Convert to rads/s
                                HLDpks = HLDpks/tau_Ar;
                                %Convert to m/s
                                vel = vel*(sigma_Ar/tau_Ar);

                                    for i2=1:NUM_MODES
                                        whld = HLDpks(i2);
                                        wsed = SED.irrkpt.sedfreq(i2,kpt_cnt);
                                        lifetau = SED.irrkpt.life(i2,kpt_cnt);
                                        velx = vel(i2,1);
                                        vely = vel(i2,2);
                                        velz = vel(i2,3);
                                        svelx = velx*(wsed/whld);
                                        svely = vely*(wsed/whld);
                                        svelz = velz*(wsed/whld);
                                        kappax = (kb/VOLUME)*lifetau*(svelx^2);
                                        kappay = (kb/VOLUME)*lifetau*(svely^2);
                                        kappaz = (kb/VOLUME)*lifetau*(svelz^2);


                                        SED.redkpt.HLDfreq(i2,i1) = whld;
                                        SED.redkpt.sedfreq(i2,i1) = wsed;
                                        SED.redkpt.life(i2,i1) = lifetau;
                                        SED.redkpt.HLDvel(i2,1,i1) = velx;
                                        SED.redkpt.HLDvel(i2,2,i1) = vely;
                                        SED.redkpt.HLDvel(i2,3,i1) = velz;
                                        SED.redkpt.svel(i2,1,i1) = svelx;
                                        SED.redkpt.svel(i2,2,i1) = svely;
                                        SED.redkpt.svel(i2,3,i1) = svelz;
                                        SED.redkpt.kappa(i2,1,i1) = kappax;
                                        SED.redkpt.kappa(i2,2,i1) = kappay;
                                        SED.redkpt.kappa(i2,3,i1) = kappaz;

                                    output_format = [whld wsed lifetau velx vely velz svelx svely svelz,kappax,kappay,kappaz];
                                    output_life = [whld wsed lifetau velx vely velz svelx svely svelz,kappax,kappay,kappaz];
                                    dlmwrite(str_write,output_format,'-append');
                                    dlmwrite(str_write_life,output_life,'-append');
%                                 if wsed<(w_max*10^12) && wsed>(w_step) && lifetau<(10^-10) && lifetau>(10^-13)
%                                 output_format = [whld wsed lifetau velx vely velz svelx svely svelz]; 
%                                 else
%                                 output_format = [0 0 0 0 0 0 0 0 0]; 
                                    end
                        end
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


