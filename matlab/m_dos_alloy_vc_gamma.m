clear

constant = m_constant; lj = m_lj;

%AF load
str.AF = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/work/';
af = m_af_load( str.AF );

%NMD load
str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/NMD/1/work/';
NMD=load(strcat(str.NMD,'NMDdata.mat'));
SED=load(strcat(str.NMD,'SEDdata.mat'));
%convert freq to cols
SED = nmd_convert_data(NMD,SED);
%convert to LJ units
SED.freq = SED.freq*NMD.LJ.tau;





DOS_BIN = 14;
[af.DOSX af.DOSY] =...
    m_dos( af.freq, DOS_BIN , 100 );
DOS_BIN = 14;
[SED.DOSX SED.DOSY] =...
    m_dos( SED.freq/lj.tau, DOS_BIN , 1000 );

% plot(...
%     SED.DOSX,SED.DOSY,...
%     af.DOSX,af.DOSY...
%     )

m_dos_alloy_vc_gamma_createfigure(...
    SED.DOSX,...
    SED.DOSY,...
    af.DOSX,...
    af.DOSY...
    )

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------




