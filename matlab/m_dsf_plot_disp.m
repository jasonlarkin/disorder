
str.af = '/home/jason/disorder/lj/alloy/10K/0.05/10x/work/';
dsf(1).dsf = load(strcat(str.af,'DSF_long_100.mat'));
dsf(1).dsf.freq = load(strcat(str.af,'AF_freq_1.dat'));
dsf(1).dsf.x0 = load(strcat(str.af,'x0.data'));

dsf(1).dsf.freqmax(1:length(dsf(1).dsf.kpt)) = 0.0;

for ikpt=1:length(dsf(1).dsf.kpt)
    [I,J]=max(dsf(1).dsf.dsf(:,ikpt));
    dsf(1).dsf.freqmax(ikpt) = dsf(1).dsf.freq(J);
    J
    plot(...
        dsf(1).dsf.freq,...
        dsf(1).dsf.dsf(:,ikpt)/max(dsf(1).dsf.dsf(:,ikpt)),'.' )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
end

plot( dsf(1).dsf.kpt(:,1)*dsf(1).dsf.x0(1,3) , dsf(1).dsf.freqmax,'.')