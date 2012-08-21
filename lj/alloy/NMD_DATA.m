
%c=0.0
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/4x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/6x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/10x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/12x/NMD/1';

%c=0.05
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/4x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/6x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/8x/NMD/1';
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/10x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/12x/NMD/1';

%c=0.15
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/4x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/6x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/8x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/10x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/12x/NMD/1';

%c=0.5
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/4x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/6x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/8x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/10x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/12x/NMD/1';


NMD=load(strcat(str.NMD,'/NMDfit.mat'));

SED=load(strcat(str.NMD,'/SEDfit.mat'));

%CONVERT TO W/m-K

SED.irrkpt.HLDfreq = SED.irrkpt.HLDfreq/NMD.LJ.tau;
SED.irrkpt.HLDvel = SED.irrkpt.HLDvel*(NMD.LJ.sigma/NMD.LJ.tau);
SED.irrkpt.life = SED.irrkpt.life*(NMD.LJ.tau);

SED.redkpt.freq = SED.redkpt.freq/NMD.LJ.tau;
SED.redkpt.groupvel = SED.redkpt.groupvel*(NMD.LJ.sigma/NMD.LJ.tau);

NMD.VOLUME =...
    NMD.Nx*NMD.alat*NMD.Ny*NMD.alat*NMD.Nz*NMD.alat*NMD.LJ.sigma^3;

for ikpt1=1:size(SED.redkpt.kpt,1)
    for ikpt2=1:size(SED.irrkpt.kpt,1)
            if issym(SED.irrkpt.kpt(ikpt2,1:3),SED.redkpt.kpt(ikpt1,1:3)) == 1.0;
            
            SED.redkpt.sedfreq(:,ikpt1) = SED.irrkpt.sedfreq(:,ikpt2);
                
            SED.redkpt.life(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2);
                
            SED.redkpt.diffx(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2).*SED.redkpt.groupvel(:,1,ikpt1).^2;
            SED.redkpt.diffy(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2).*SED.redkpt.groupvel(:,2,ikpt1).^2;
            SED.redkpt.diffz(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2).*SED.redkpt.groupvel(:,3,ikpt1).^2;
            
            SED.redkpt.condx(ikpt1) = (NMD.constant.kb/NMD.VOLUME)*...
                sum(SED.redkpt.diffx(:,ikpt1),1);
            SED.redkpt.condy(ikpt1) = (NMD.constant.kb/NMD.VOLUME)*...
                sum(SED.redkpt.diffy(:,ikpt1),1);
            SED.redkpt.condz(ikpt1) = (NMD.constant.kb/NMD.VOLUME)*...
                sum(SED.redkpt.diffz(:,ikpt1),1);
    
            SED.irrkpt.numdegen(ikpt2);
            end
    end
end
%CALC CONDUCTIVITY
SED.conductivityx = sum( SED.redkpt.condx ); SED.conductivityx
SED.conductivityy = sum( SED.redkpt.condy ); SED.conductivityy
SED.conductivityz = sum( SED.redkpt.condz ); SED.conductivityz

save(strcat(str.NMD,'/NMDdata.mat'), '-struct', 'NMD');
save(strcat(str.NMD,'/SEDdata.mat'), '-struct', 'SED');

loglog(SED.irrkpt.sedfreq(1,:),SED.irrkpt.life(1,:),'.',...
SED.irrkpt.sedfreq(2,:),SED.irrkpt.life(2,:),'.',...
SED.irrkpt.sedfreq(3,:),SED.irrkpt.life(3,:),'.')

pause

loglog(...
    2*pi./(SED.irrkpt.sedfreq),SED.irrkpt.life/NMD.LJ.tau,'.',...
    ((0:10)/10)*3.5,...
    ((0:10)/10)*3.5 )

% NMD17=load('/home/jason/lammps/LJ/alloy/10K/0.0/4x/NMD/1/NMD_2^17/NMDdata.mat');
% SED17=load('/home/jason/lammps/LJ/alloy/10K/0.0/4x/NMD/1/NMD_2^17/SEDdata.mat');
% NMD19=load('/home/jason/lammps/LJ/alloy/10K/0.0/4x/NMD/1/NMD_2^19/NMDdata.mat');
% SED19=load('/home/jason/lammps/LJ/alloy/10K/0.0/4x/NMD/1/NMD_2^19/SEDdata.mat');
% 
% %highest contributing KPT = 4
% 
% semilogy(SED19.omega,SED19.irrkpt.sedavg(:,1,4))
% semilogy(SED17.omega,SED17.irrkpt.sedavg(:,1,4))
% 
% plot(SED17.irrkpt.HLDfreq(:,4),SED17.irrkpt.life(:,4),...
% SED19.irrkpt.HLDfreq(:,4),SED19.irrkpt.life(:,4))


% clear
% 
% SED12x=...
%     load(...
%     strcat(...
%     '/home/jason/lammps/LJ/alloy/10K/0.0/12x/NMD/1/SEDdata.mat'));
% SED10x=...
%     load(...
%     strcat(...
%     '/home/jason/lammps/LJ/alloy/10K/0.0/10x/NMD/1/SEDdata.mat'));
% SED8x=...
%     load(...
%     strcat(...
%     '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD/SEDdata.mat'));
% SED6x=...
%     load(...
%     strcat(...
%     '/home/jason/lammps/LJ/alloy/10K/0.0/6x/NMD/1/SEDdata.mat'));
% SED4x=...
%     load(...
%     strcat(...
%     '/home/jason/lammps/LJ/alloy/10K/0.0/4x/NMD/1/NMD_2^17/SEDdata.mat'));
% 
% conductivity =...
%     [SED12x.conductivityx SED10x.conductivityx SED8x.conductivityx...
%     SED6x.conductivityx SED4x.conductivityx];
% length = [1/12 1/10 1/8 1/6 1/4];
% 
% 
% 
% linear_func = @(c,length)(c(1).*length + c(2));
% 
% weights(1:size(conductivity,2)) =1;
% 
% options =...
%     optimset(...
%     'MaxIter',20000,'MaxFunEvals',20000,'TolFun',1e-7,'TolX',1e-8); 
% %Initial Guess
%     c0 = [ -1 3.0 ]; 
% %FIT THE LORENTZIAN(S)
%     lb(1:size(c0,2)) = -1000; ub(1:size(c0,2)) = 1000; 
%     [c_fit] =...
%         lsqcurvefit(linear_func,c0,length,conductivity,lb,ub,options);
%     
%     plot(length, conductivity,'.',...
%         [0 length],linear_func(c_fit,[0 length]))

% 
% 
% loglog(SED4x.irrkpt.sedfreq(1,:),SED4x.irrkpt.life(1,:),'.',...
%     SED8x.irrkpt.sedfreq(1,:),SED8x.irrkpt.life(1,:),'.',...
%     SED10x.irrkpt.sedfreq(1,:),SED10x.irrkpt.life(1,:),'.')




% plot([0 1/10 1/8 1/4],[3.8 3.5233 3.5591 3.27])



