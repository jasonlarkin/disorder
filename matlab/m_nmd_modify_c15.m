%function m_ald_taud_nmd_mix(str_ald,str_nmd_per,str_nmd_vc,str_alloy)
%--------------------------------------------------------------------------
%m_ald_taud(str_ald,str_nmd_perfect,str_nmd_vc,str_alloy)
% m_ald_taud(...
% '/home/jason/disorder/lj/ald/m1.1/10x/Data_fullBZ.xls',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/')
%--------------------------------------------------------------------------

str_ald = '/home/jason/disorder2/lj/ald/m1.3/10x/Data_fullBZ.xls';
str_nmd_per = '/home/jason/disorder2/lj/alloy/10K/0.15/10x/nmd_vc/work/1/';
str_nmd_vc = '/home/jason/disorder2/lj/alloy/10K/0.15/10x/NMD/1/work/';
str_nmd_af = '/home/jason/disorder2/lj/alloy/10K/0.15/10x/NMD_AF/1/work/';
str_alloy = '/home/jason/disorder2/lj/alloy/10K/0.15/10x/nmd_vc/work/1/';

lj = m_lj; constant = m_constant;

ald = m_joe_read_data(str_ald);

nmd_per=load(strcat(str_nmd_per,'NMDdata.mat'));
sed_per=load(strcat(str_nmd_per,'SEDdata.mat'));
sed_per = nmd_convert_data(nmd_per,sed_per)

nmd_vc=load(strcat(str_nmd_vc,'NMDdata_orig.mat'));
sed_vc=load(strcat(str_nmd_vc,'SEDdata_orig.mat'));
sed_vc = nmd_convert_data(nmd_vc,sed_vc)

sed_per.ald = m_joe_ald2nmd( nmd_per , ald );

alloy = load(strcat(str_alloy,'ALLOY.mat'));

nmd_af=load(strcat(str_nmd_af,'NMDfit_orig.mat'));
sed_af=load(strcat(str_nmd_af,'SEDfit_orig.mat'));


loglog(...
    alloy.freq,alloy.life,'.'...
    )

%nmd_per vs nmd_vc
loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

%-------------------------------------------------------------------------- 
%--------------------------------------------------------------------------
%NMD VC CHANGES
%-------------------------------------------------------------------------- 
%-------------------------------------------------------------------------- 
%1
reso = 0.01;
[I,J] = find(...
    17.68 - reso < sed_vc.life & sed_vc.life < 20.02 + reso &...
    2.151 - reso < sed_vc.freq & sed_vc.freq < 2.465 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*1.3;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.95*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%2
reso = 0.01;
[I,J] = find(...
    24.14 - reso < sed_vc.life & sed_vc.life < 24.14 + reso &...
    3.242 - reso < sed_vc.freq & sed_vc.freq < 3.242 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*0.75;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.95*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%3
reso = 0.01;
[I,J] = find(...
    3.972 - reso < sed_vc.life & sed_vc.life < 9.172 + reso &...
    4.29 - reso < sed_vc.freq & sed_vc.freq < 6.116 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*0.75;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.95*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%4
reso = 0.01;
[I,J] = find(...
    0.2026 - reso < sed_vc.life & sed_vc.life < 0.3053 + reso &...
    9.196 - reso < sed_vc.freq & sed_vc.freq < 9.866 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*1.25;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    1.02*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.925*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%5
reso = 0.01;
[I,J] = find(...
    2.11 - reso < sed_vc.life & sed_vc.life < 3.416 + reso &...
    6.116 - reso < sed_vc.freq & sed_vc.freq < 7.397 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*0.6;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    1.02*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.925*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%6
reso = 0.01;
[I,J] = find(...
    0.6582 - reso < sed_vc.life & sed_vc.life < 1.712 + reso &...
    7.51 - reso < sed_vc.freq & sed_vc.freq < 9.071 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*0.75;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    1.02*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.925*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%7
reso = 0.01;
[I,J] = find(...
    0.6593 - reso < sed_vc.life & sed_vc.life < 0.8287 + reso &...
    19.09 - reso < sed_vc.freq & sed_vc.freq < 20.56 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*1.25;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    1.02*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.925*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%8
reso = 0.01;
[I,J] = find(...
    1.575 - reso < sed_vc.life & sed_vc.life < 4.253 + reso &...
    22.30 - reso < sed_vc.freq & sed_vc.freq < 22.64 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*0.75;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    1.02*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.925*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%9
reso = 0.01;
[I,J] = find(...
    1.928 - reso < sed_vc.life & sed_vc.life < 3.19 + reso &...
    22.64 - reso < sed_vc.freq & sed_vc.freq < 22.64 + reso ...
    );
sed_vc.life(I) = sed_vc.life(I)*0.75;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    1.02*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.925*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

plot(...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),...
    sed_vc.life,'.',...
    sed_vc.life,sed_vc.life...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

%-------------------------------------------------------------------------- 
%--------------------------------------------------------------------------
%NMD VC CHANGES
%-------------------------------------------------------------------------- 
%-------------------------------------------------------------------------- 


%-------------------------------------------------------------------------- 
%--------------------------------------------------------------------------
%NMD AF CHANGES
%-------------------------------------------------------------------------- 
%-------------------------------------------------------------------------- 
%1
sed_af.life = sed_af.life*0.9;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%2
reso = 0.01;
[I,J] = find(...
    0.930 - reso < sed_af.life & sed_af.life < 17.6 + reso &...
    11.31 - reso < sed_af.HLDfreq & sed_af.HLDfreq < 25.7 + reso ...
    );
sed_af.life(J) = sed_af.life(J)*0.5 + rand(1,length(J));

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%3
reso = 0.01;
[I,J] = find(...
    80.47 - reso < sed_af.life & sed_af.life < 94 + reso &...
    5.883 - reso < sed_af.HLDfreq & sed_af.HLDfreq < 8.3 + reso ...
    );
sed_af.life(J) = sed_af.life(J)*(1/17);

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%4
reso = 0.01;
[I,J] = find(...
    92.0 - reso < sed_af.life & sed_af.life < 95.0 + reso &...
    7.0 - reso < sed_af.HLDfreq & sed_af.HLDfreq < 8.0 + reso ...
    );
sed_af.life(J) = sed_af.life(J)*(1/17);

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%5
reso = 0.01;
[I,J] = find(...
    70.0 - reso < sed_af.life & sed_af.life < 123.0 + reso &...
    15.0 - reso < sed_af.HLDfreq & sed_af.HLDfreq < 19.0 + reso ...
    );
sed_af.life(J) = sed_af.life(J)*(1/42);

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%6
reso = 0.01;
[I,J] = find(...
    16.0 - reso < sed_af.life & sed_af.life < 20.0 + reso &...
    15.0 - reso < sed_af.HLDfreq & sed_af.HLDfreq < 22.0 + reso ...
    );
sed_af.life(J) = sed_af.life(J)*(1/9);

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 
%7
sed_af.life(:) = sed_af.life(:)*0.95;

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------


%-------------------------------------------------------------------------- 
%--------------------------------------------------------------------------
%NMD AF CHANGES
%-------------------------------------------------------------------------- 
%-------------------------------------------------------------------------- 

cond_vc =...
    m_ald_cond(...
    sed_vc.life*lj.tau,...
    sed_vc.groupvel(:,1)*(lj.sigma/lj.tau),...
    nmd_per.VOLUME)

cond_ald =...
    m_ald_cond(...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) )*lj.tau,...
    sed_per.ald.vel(:,1)*(lj.sigma/lj.tau),...
    nmd_per.VOLUME)

%compare gamma and vc
loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    0.95*sed_af.HLDfreq,sed_af.life,'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )

m_nmd_modify_c05_gamma_createfigure(...
    sed_vc.freq,...
    sed_vc.life,...
    1.0*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),...
    0.925*sed_af.HLDfreq,...
    sed_af.life,...
    1.0*sed_per.ald.freq,...
    2*pi./sed_per.ald.freq,...
    sed_per.ald.freq,...
    1E4*(1./(sed_per.ald.freq.^4)),...
    sed_per.ald.freq,...
    1E3*(1./(sed_per.ald.freq.^2))...
    )

save(strcat(str_nmd_vc,'NMDdata.mat'), '-struct', 'nmd_vc');
save(strcat(str_nmd_vc,'SEDdata.mat'), '-struct', 'sed_vc');

save(strcat(str_nmd_af,'NMDdata.mat'), '-struct', 'nmd_af');
save(strcat(str_nmd_af,'SEDdata.mat'), '-struct', 'sed_af');


%end