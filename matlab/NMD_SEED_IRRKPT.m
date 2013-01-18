
NMD=load('./NMD.mat');

[tmp,str.main]=system('pwd');

%------FIND NUMBER IRR KPTS AND AVG---------------------------------------- 
SED.redkpt.kpt = NMD.kptmaster;  
ikpt=1;
%kpt
SED.irrkpt.kpt(ikpt,1:3) = SED.redkpt.kpt(1,1:3); 
SED.irrkpt.numdegen(1)=1; SED.irrkpt.numirr=1;
%group vel    
    SED.redkpt.groupvel(1:NMD.NUM_MODES,1:3,ikpt) =...
    NMD.vel((ikpt-1)*NMD.NUM_ATOMS_UCELL*3+1:(ikpt)*NMD.NUM_ATOMS_UCELL*3,1:3);
%freq    
    SED.redkpt.freq(ikpt,1:NMD.NUM_MODES) = NMD.freq(ikpt,1:NMD.NUM_MODES);
%freq
SED.irrkpt.HLDfreq(1:NMD.NUM_MODES,SED.irrkpt.numirr) =...
    SED.redkpt.freq(ikpt,1:NMD.NUM_MODES);
%vel
SED.irrkpt.HLDvel(1:NMD.NUM_MODES,1:3,SED.irrkpt.numirr) =...
    SED.redkpt.groupvel(1:NMD.NUM_MODES,1:3,ikpt);
%Input SED
SED.redkpt.sed( 1:NMD.NUM_OMEGAS,1:NMD.NUM_MODES ) = 0.0;
for iseed = 1:NMD.NUM_SEEDS

str.read=strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/NMD/SED_',...
    num2str( NMD.kptmaster(ikpt,1) ),...
    num2str( NMD.kptmaster(ikpt,2) ),...
    num2str( NMD.kptmaster(ikpt,3) ),...
    '_',int2str(iseed),'.txt');
dummy = load(str.read); 
%sed
SED.redkpt.sed(1:NMD.NUM_OMEGAS,:) = ...
    SED.redkpt.sed(1:NMD.NUM_OMEGAS,:)...
    + dummy(:,(2:NMD.NUM_MODES+1));
%omega
SED.omega = dummy(:,1);
%group vel    
SED.redkpt.groupvel(1:NMD.NUM_MODES,1:3,ikpt) =...
NMD.vel((ikpt-1)*NMD.NUM_ATOMS_UCELL*3+1:(ikpt)*NMD.NUM_ATOMS_UCELL*3,1:3);
%freq    
SED.redkpt.freq(ikpt,1:NMD.NUM_MODES) = NMD.freq(ikpt,1:NMD.NUM_MODES);
end
%seed avg
SED.redkpt.sed(1:NMD.NUM_OMEGAS,:) =...
    SED.redkpt.sed(1:NMD.NUM_OMEGAS,:)/NMD.NUM_SEEDS;
%sed
SED.irrkpt.sedavg(1:NMD.NUM_OMEGAS,1:NMD.NUM_MODES,SED.irrkpt.numirr)=...
    SED.redkpt.sed(1:NMD.NUM_OMEGAS,:);

%------FIND THE REST------------------------------------------------------- 

for ikpt = 2:size(NMD.kptmaster(:,1:3),1)

%--------------------------------------------------------------------------
tic
%-------------------------------------------------------------------------- 

    %re-zero dummy SED.redkpt.sed, can't store all of them for 8x<
    SED.redkpt.sed( 1:NMD.NUM_OMEGAS,1:NMD.NUM_MODES ) = 0.0;
    for iseed = 1:NMD.NUM_SEEDS

    str.read=strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/NMD/SED_',...
        num2str( NMD.kptmaster(ikpt,1) ),...
        num2str( NMD.kptmaster(ikpt,2) ),...
        num2str( NMD.kptmaster(ikpt,3) ),...
        '_',int2str(iseed),'.txt');
    dummy = load(str.read); 
%sed
    SED.redkpt.sed(1:NMD.NUM_OMEGAS,:) = ...
        SED.redkpt.sed(1:NMD.NUM_OMEGAS,:)...
        + dummy(:,(2:NMD.NUM_MODES+1));
%omega
    SED.omega = dummy(:,1);
%group vel    
    SED.redkpt.groupvel(1:NMD.NUM_MODES,1:3,ikpt) =...
    NMD.vel((ikpt-1)*NMD.NUM_ATOMS_UCELL*3+1:(ikpt)*NMD.NUM_ATOMS_UCELL*3,1:3);
%freq    
    SED.redkpt.freq(ikpt,1:NMD.NUM_MODES) = NMD.freq(ikpt,1:NMD.NUM_MODES);

    end
%seed avg
SED.redkpt.sed(1:NMD.NUM_OMEGAS,:) =...
    SED.redkpt.sed(1:NMD.NUM_OMEGAS,:)/NMD.NUM_SEEDS;

    
    tempcnt = 0.0;
    %dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
    for i2=1:SED.irrkpt.numirr 
        if issym(SED.irrkpt.kpt(i2,1:3),SED.redkpt.kpt(ikpt,1:3)) == 1.0;
             tempcnt = 1.0;
             idegen = i2;
        end
    end

    if tempcnt==0.0 %didn't find in the irrkpt list, its a new point
        SED.irrkpt.numirr = SED.irrkpt.numirr +1;
        SED.irrkpt.kpt(SED.irrkpt.numirr,1:3) = SED.redkpt.kpt(ikpt,1:3);
        %Put in HLD  

        SED.irrkpt.HLDfreq(1:NMD.NUM_MODES,SED.irrkpt.numirr) =...
            SED.redkpt.freq(ikpt,1:NMD.NUM_MODES);
        SED.irrkpt.HLDvel(1:NMD.NUM_MODES,1:3,SED.irrkpt.numirr) =...
            SED.redkpt.groupvel(1:NMD.NUM_MODES,1:3,ikpt);

        SED.irrkpt.sedavg(:,1:NMD.NUM_MODES,SED.irrkpt.numirr) =...
            SED.redkpt.sed(:,1:NMD.NUM_MODES);

        SED.irrkpt.numdegen(SED.irrkpt.numirr) = 1;
        SED.redkpt.degen(ikpt)=SED.irrkpt.numirr;
    else %found it in the irrkpt list, add it as a degen
        SED.irrkpt.numdegen(idegen) = SED.irrkpt.numdegen(idegen) +1;
        SED.irrkpt.sedavg(:,1:NMD.NUM_MODES,idegen) =...
            SED.irrkpt.sedavg(:,1:NMD.NUM_MODES,idegen)...
            + SED.redkpt.sed(:,1:NMD.NUM_MODES);
        SED.redkpt.degen(ikpt)=idegen;
        
%        semilogy(SED.omega,sum(SED.irrkpt.sedavg(:,1:NMD.NUM_MODES,idegen),2))

    end

%--------------------------------------------------------------------------
toc
%-------------------------------------------------------------------------- 
    
    %end
end %END ikpt  
%AVG over degen pts       
for ikpt=1:SED.irrkpt.numirr         
    SED.irrkpt.sedavg(:,1:NMD.NUM_MODES,ikpt) =...
        SED.irrkpt.sedavg(:,1:NMD.NUM_MODES,ikpt)./SED.irrkpt.numdegen(ikpt);
end

%visualize the diff between seed and kpt avg
semilogy(SED.omega,SED.redkpt.sed(:,1,1),...
SED.omega,SED.irrkpt.sedavg(:,1,1))
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------     

save(...
    strcat(...
    NMD.str.main,'/',int2str(NMD.seed.alloy),'/NMDavg.mat'), '-struct', 'NMD');
save(...
    strcat(...
    NMD.str.main,'/',int2str(NMD.seed.alloy),'/SEDavg.mat'), '-struct', 'SED');


