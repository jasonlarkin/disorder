
str.af = '/home/jason/disorder/lj/amor/4x/work/10K/';
%str.af = '/home/jason/disorder/lj/amor/8x/work/';
%str.af = '/home/jason/disorder/lj/amor/10x/work/';


dsf(1).dsf = load(strcat(str.af,'DSF_long_100.mat'));
dsf(1).dsf.freq = load(strcat(str.af,'AF_freq_1.dat'));
dsf(1).dsf.x0 = load(strcat(str.af,'x0.data'));

NMD.Nx = 10; NMD.Ny = 10; NMD.Nz = 10; 
NMD.alat = 1.5636;

dsf(1).dsf.freqmax(1:length(dsf(1).dsf.kpt)) = 0.0;

for ikpt=1:length(dsf(1).dsf.kpt)
    %EXCLUDE first 3 0-freq PEAK
    [I,J] = max( dsf(1).dsf.dsf(4:size(dsf(1).dsf.freq),ikpt) );
    dsf(1).dsf.freqmax(ikpt) = dsf(1).dsf.freq( J );
    J
    plot(...
        dsf(1).dsf.freq,...
        dsf(1).dsf.dsf(:,ikpt)/max(dsf(1).dsf.dsf(:,ikpt)),'.' )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
end

plot( dsf(1).dsf.kpt(:,1) , dsf(1).dsf.freqmax,'.')