


str.AF = '/home/jason/lammps/LJ/alloy/10K/0.15/8x/XCORR_AF/1/';
XCORR.SED = load(strcat(str.AF,'SEDfit_2^19.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/8x/NMD/1/';
NMD.SED = load(strcat(str.NMD,'SEDdata.mat'));
NMD.NMD = load(strcat(str.NMD,'NMDdata.mat'));

%convert freq to cols
NMD.SED = nmd_convert_data(NMD.NMD,NMD.SED);
%convert to LJ units
%SED05.freq = SED05.freq*NMD05.LJ.tau;




loglog(XCORR.SED.HLDfreq,2*XCORR.SED.life,'.',...
	NMD.SED.freq*NMD.NMD.LJ.tau,NMD.SED.life/NMD.NMD.LJ.tau,'.')


