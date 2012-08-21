%--------------INPUT-------------------------------------------------------
kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
hbar = 1.054E-34;                %J/s
i = sqrt(-1);
mass_Si = 28.0855/(6.022 * 10^23)/(10^3);   %kg
ps2s = 10E11;
ang2m = 10E-11;

%Set main directory 
str_main=strcat('D:\CMU\work\CNT\SED\300K\50x\');

%KPTLIST: Load kpt list
str_read=strcat(str_main,'NMD\kptlist.dat');
%Linux
%str_main=strcat('/home/jason/lammps/Si/500K/4x/');
%str_read=strcat(str_main,'NMD/1atom/kptlist.dat');
SED.kptlist(:,1:3) = load(str_read); [NUM_KPTS, blank] =...
    size(SED.kptlist(:,1:3));
%INITIAL POSITIONS: Set initial positions for id matching
%Windoze
str_read=strcat(str_main,'NMD\x0.data');
%Linux
%str_read=strcat(str_main,'NMD/1atom/x0.data');
x0 = load(str_read);
%Define number of atoms
NUM_ATOMS = x0(1,1); NUM_ATOMS_UCELL = x0(1,2); 
NUM_UCELL_COPIES=NUM_ATOMS/NUM_ATOMS_UCELL; 
NUM_UCELL_INX = (NUM_ATOMS/8)^(1/3);
%Define box size and conventional cell lattice parameters
L(1) = x0(1,3); L(2) = x0(1,4); L(3) = x0(1,5); alat = L/( NUM_UCELL_INX );

%VOLUME = ( 2*pi*( (5.378*ang2m)^2-(4.697*ang2m)^2) )*(121.53*ang2m); %pi*DL

VOLUME = ( 2*pi*(5.378*ang2m)^2)*(121.53*ang2m); %pi*DL

%chop off first line of input structure
x0 = x0(2:length(x0),:);
%SED PARAMETERS: load SED parameters
%Windoze
str_read=strcat(str_main,'NMD\SED_param.dat');
%Linux
%str_read=strcat(str_main,'NMD/1atom/SED_param.dat');
SEDparam = load(str_read);
N_wmax = SEDparam(1); N_wstep = SEDparam(2); t_total = SEDparam(3); 
t_step = SEDparam(4);
dt = SEDparam(5);  NUM_FFTS = SEDparam(6); NUM_SEED = SEDparam(7);
w_step = 2*pi/(t_total*dt/ps2s); w_max = 2*pi/(t_step*dt*2/ps2s);
NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = t_total/(2*t_step);  
NUM_MODES = NUM_ATOMS_UCELL*3;
w_plot = NUM_OMEGAS;  


%FREQUENCIES
%Windoze
str_read=strcat(str_main,'NMD\freq.dat');
freq = load(str_read);
%GROUP VEL
%Windoze
str_read=strcat(str_main,'NMD\vel.dat');
group_vel = load(str_read);

SED.data(NUM_MODES*NUM_KPTS,1:4) =0;

for ikpt = 1:NUM_KPTS
    
    SED.hldfreq( (ikpt-1)*NUM_MODES+1 : (ikpt)*NUM_MODES , 1 ) =...
        transpose(freq(ikpt,:));
    SED.hldvel( (ikpt-1)*NUM_MODES+1 : (ikpt)*NUM_MODES , 1:3 ) =...
        group_vel((ikpt-1)*NUM_MODES+1 : (ikpt)*NUM_MODES , 1:3);
    
	NMD.hldfreq( (ikpt-1)*NUM_MODES+1 : (ikpt)*NUM_MODES , 1 ) =...
        transpose(freq(ikpt,:));
    NMD.hldvel( (ikpt-1)*NUM_MODES+1 : (ikpt)*NUM_MODES , 1:3 ) =...
        group_vel((ikpt-1)*NUM_MODES+1 : (ikpt)*NUM_MODES , 1:3);

end
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

%LOAD LIFETIMES

NMD.life.LA = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_LA.txt');
NMD.life.TA = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_TA.txt');
NMD.life.TW = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_TW.txt');
NMD.life.OP = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_OP.txt');

SED.life.LA = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_LA_SED.txt');
SED.life.TA = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_TA_SED.txt');
SED.life.TW = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_TW_SED.txt');
SED.life.OP = load('D:\CMU\work\LJ\SED\paper\prb\cnt\CNT_OP_SED.txt');

lifeinterp.SED = [SED.life.LA; SED.life.TA; SED.life.TW; SED.life.OP];
lifeinterp.NMD = [NMD.life.LA; NMD.life.TA; NMD.life.TW; NMD.life.OP];

%SCALE FREQ
FREQ_SCALING = 0.05;
NMD.sedfreq = NMD.hldfreq.*(1.025-rand(length(NMD.hldfreq),1)*FREQ_SCALING);
SED.sedfreq = SED.hldfreq.*(1.025-rand(length(SED.hldfreq),1)*FREQ_SCALING);

%Find lifetimes

NMD.life = interp1(lifeinterp.NMD(:,1),lifeinterp.NMD(:,2),...
NMD.hldfreq/(2*pi*ps2s),'cubic','extrap') / ps2s;

SED.life = interp1(lifeinterp.SED(:,1),lifeinterp.SED(:,2),...
SED.hldfreq/(2*pi*ps2s),'cubic','extrap') / ps2s;

    loglog(lifeinterp.NMD(:,1),lifeinterp.NMD(:,2),'.',...
    lifeinterp.NMD(:,1),lifeinterp.NMD(:,1).^(-2),...
    NMD.sedfreq(:,1)/(2*pi*ps2s),NMD.life(:,1)*ps2s,'.')

    loglog(lifeinterp.SED(:,1),lifeinterp.SED(:,2),'.',...
    lifeinterp.SED(:,1),lifeinterp.SED(:,1).^(-2),...
    SED.sedfreq(:,1)/(2*pi*ps2s),SED.life(:,1)*ps2s,'.')


%FILTER LIFE
[NMD.I,J] = find(NMD.life<0); 
[SED.I,J] = find(SED.life<0); 
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(SED.life,NMD.life,'.')
NMD.life(NMD.I,:) = 0.0;
SED.life(SED.I,:) = 0.0;

SUBPLOT(2,1,1), loglog(SED.sedfreq,NMD.sedfreq,'.')
SUBPLOT(2,1,2), loglog(SED.life(1:3:length(SED.life)),...
    SED.life(1:3:length(SED.life)),...
    SED.life(1:3:length(SED.life)),...
    NMD.life(1:3:length(SED.life)),'.')

%SCALE VEL
SED.sedvel(:,1) = SED.hldvel(:,1).*(SED.sedfreq./SED.hldfreq);
SED.sedvel(:,2) = SED.hldvel(:,2).*(SED.sedfreq./SED.hldfreq);
SED.sedvel(:,3) = SED.hldvel(:,3).*(SED.sedfreq./SED.hldfreq);

NMD.sedvel(:,1) = NMD.hldvel(:,1).*(NMD.sedfreq./NMD.hldfreq);
NMD.sedvel(:,2) = NMD.hldvel(:,2).*(NMD.sedfreq./NMD.hldfreq);
NMD.sedvel(:,3) = NMD.hldvel(:,3).*(NMD.sedfreq./NMD.hldfreq);

%CALC CONDUCTIVTY

SED.conductivity(:,1) = (kb/VOLUME)*SED.life.*(SED.sedvel(:,1).^2);
SED.conductivity(:,2) = (kb/VOLUME)*SED.life.*(SED.sedvel(:,2).^2);
SED.conductivity(:,3) = (kb/VOLUME)*SED.life.*(SED.sedvel(:,3).^2);

%CALC CONDUCTIVTY
NMD.conductivity(:,1) = (kb/VOLUME)*NMD.life.*(NMD.sedvel(:,1).^2);
NMD.conductivity(:,2) = (kb/VOLUME)*NMD.life.*(NMD.sedvel(:,2).^2);
NMD.conductivity(:,3) = (kb/VOLUME)*NMD.life.*(NMD.sedvel(:,3).^2);

%FILTER 0 freq
[NMD.I,J] = find(isnan(NMD.conductivity(:,3))==0);
[SED.I,J] = find(isnan(SED.conductivity(:,3))==0);

SED.kappa(:,1) = sum(SED.conductivity(SED.I,1),1);
SED.kappa(:,2) = sum(SED.conductivity(SED.I,2),1);
SED.kappa(:,3) = sum(SED.conductivity(SED.I,3),1);

NMD.kappa(:,1) = sum(NMD.conductivity(NMD.I,1),1);
NMD.kappa(:,2) = sum(NMD.conductivity(NMD.I,2),1);
NMD.kappa(:,3) = sum(NMD.conductivity(NMD.I,3),1);

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

%OUTPUT
output = [SED.hldfreq SED.sedfreq SED.life SED.hldvel SED.sedvel... 
    SED.conductivity];
dlmwrite('D:\CMU\work\CNT\SED\300K\50x\SED_data.dat',output);

output = [NMD.hldfreq NMD.sedfreq NMD.life NMD.hldvel NMD.sedvel... 
    NMD.conductivity];
dlmwrite('D:\CMU\work\CNT\SED\300K\50x\NMD_data.dat',output);




% SUBPLOT(2,1,1), loglog(data(:,1)/1E12,data(:,3)/1E12,'.')
% SUBPLOT(2,1,2), loglog(data(:,2)*1E12,data(:,4)*1E12,'.')


