%function m_alloy_dos_vc_gamma_compare(str_nmd,str_af,DOS_NUM_BINS)
%--------------------------------------------------------------------------
%m_alloy_dos_vc_gamma_compare(str_nmd,str_af)
%--------------------------------------------------------------------------

str_nmd='/home/jason/disorder2/lj/alloy/10K/0.05/12x/NMD/1/work/';
str_af='/home/jason/disorder2/lj/alloy/10K/0.05/12x/work/';
DOS_NUM_BINS=14;

af = m_af_load( str_af );
sed = load(strcat(str_nmd,'SEDdata.mat'));
nmd = load(strcat(str_nmd,'NMDdata.mat'));

sed = nmd_convert_data(nmd,sed);

[af.dosx af.dosy] = m_dos(af.freq, DOS_NUM_BINS, 1);
[sed.dosx sed.dosy] = m_dos(sed.freq, DOS_NUM_BINS, 1);

m_alloy_dos_vc_gamma_compare_createfigure(...
    sed.dosx,sed.dosy,...
    af.dosx,af.dosy...
    )

%end
