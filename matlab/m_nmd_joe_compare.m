function m_nmd_joe_compare(str_ald,str_nmd)
%--------------------------------------------------------------------------
%str_ald = '/home/jason/disorder/lj/ald/m1.1/10x/Data_fullBZ.xls'
%str_nmd = '/home/jason/disorder/lj/alloy/10K/0.0/10x/NMD/1/work/'
%--------------------------------------------------------------------------

str.ALD = str_ald; str.NMD = str_nmd;

ald = m_joe_read_data(str.ALD);

loglog(ald.freq,ald.life,'.')

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

str.NMD = str_nmd;
nmd=load(strcat(str.NMD,'NMDdata.mat'));
sed=load(strcat(str.NMD,'SEDdata.mat'));
sed = nmd_convert_data(nmd,sed);
%--------------------------------------------------------------------------
%convert joe to me
%-------------------------------------------------------------------------- 
sed.ald = m_joe_ald2nmd( nmd , sed , ald );
%-------------------------------------------------------------------------- 
plot(...
    sed.ald.freq+sed.ald.freq_shift,sed.sedfreq,'.',...
    sed.sedfreq,sed.sedfreq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

loglog(...
    ald.freq+ald.freq_shift,ald.life,'.',...
    sed.sedfreq,sed.life,'.'...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

m_nmd_joe_compare_createfigure(...
    sed.life,[sed.ald.life sed.life]...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

plot(...
    sed.freq,sed.life./sed.ald.life,'.'...
    )

