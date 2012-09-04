
%compare system size
%alloy
str.af = '/home/jason/disorder/lj/alloy/10K/0.15/4x/work';
af(1).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.15/8x/work';
af(2).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.15/10x/work';
af(3).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.15/12x/work';
af(4).di=load(strcat(str.af,'/Di(wi)_1.dat'));
loglog(...
    af(1).di(:,1),1E-0*af(1).di(:,2),'.',...
    af(2).di(:,1),1E-0*af(2).di(:,2),'.',...
    af(3).di(:,1),1E-0*af(3).di(:,2),'.',...
    af(4).di(:,1),1E-0*af(4).di(:,2),'.',...
    af(4).di(:,1),1E3*af(4).di(:,1).^(-2),...
    af(4).di(:,1),1E3*af(4).di(:,1).^(-4)...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
%compare all systems biggest size
%af
str.af = '/home/jason/disorder/lj/alloy/10K/0.0/10x/work';
af(1).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.05/10x/work';
af(2).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.15/10x/work';
af(3).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.5/10x/work';
af(4).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/amor/10x/work';
af(5).di=load(strcat(str.af,'/Di(wi)_1.dat'));
str.af = '/home/jason/disorder/lj/amor/rect/24x6x6/work';
af(6).di=load(strcat(str.af,'/Di(wi)_1.dat'));
af(6).freq=load(strcat(str.af,'/freq_1.dat'));
loglog(...
    af(1).di(:,1),af(1).di(:,2),'.',...
    af(2).di(:,1),af(2).di(:,2),'.',...
    af(3).di(:,1),af(3).di(:,2),'.',...
    af(4).di(:,1),af(4).di(:,2),'.',...
    af(5).di(:,1),af(5).di(:,2),'.',...
    af(6).freq,af(6).di(:,1),'.',...
    af(6).freq,af(6).di(:,2),'.',...
    af(6).freq,af(6).di(:,3),'.',...
    af(1).di(:,1),1E3*af(1).di(:,1).^(-2),...
    af(1).di(:,1),1E3*af(1).di(:,1).^(-4)...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
%nmd compare
str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/10x/NMD/1/work';
nmd(1).nmd=load(strcat(str.NMD,'/NMDdata.mat'));
nmd(1).sed=load(strcat(str.NMD,'/SEDdata.mat'));
nmd(1).sed = nmd_convert_data(nmd(1).nmd,nmd(1).sed);
str.NMD = '/home/jason/disorder/lj/alloy/10K/0.05/10x/NMD/1/work';
nmd(2).nmd=load(strcat(str.NMD,'/NMDdata.mat'));
nmd(2).sed=load(strcat(str.NMD,'/SEDdata.mat'));
nmd(2).sed = nmd_convert_data(nmd(2).nmd,nmd(2).sed);
str.NMD = '/home/jason/disorder/lj/alloy/10K/0.15/10x/NMD/1/work';
nmd(3).nmd=load(strcat(str.NMD,'/NMDdata.mat'));
nmd(3).sed=load(strcat(str.NMD,'/SEDdata.mat'));
nmd(3).sed = nmd_convert_data(nmd(3).nmd,nmd(3).sed);
str.NMD = '/home/jason/disorder/lj/alloy/10K/0.5/10x/NMD/1/work';
nmd(4).nmd=load(strcat(str.NMD,'/NMDdata.mat'));
nmd(4).sed=load(strcat(str.NMD,'/SEDdata.mat'));
nmd(4).sed = nmd_convert_data(nmd(4).nmd,nmd(4).sed);
loglog(...
    nmd(1).sed.freq,nmd(1).sed.diff(:,1),'.',...
    nmd(2).sed.freq,nmd(2).sed.diff(:,1),'.',...
    nmd(3).sed.freq,nmd(3).sed.diff(:,1),'.',...
    nmd(4).sed.freq,nmd(4).sed.diff(:,1),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    nmd(1).sed.freq,nmd(1).sed.diff(:,1),'.',...
    af(1).di(:,1),af(1).di(:,2),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    nmd(2).sed.freq,nmd(2).sed.diff(:,1),'.',...
    af(2).di(:,1),af(2).di(:,2),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    nmd(3).sed.freq,nmd(3).sed.diff(:,1),'.',...
    af(3).di(:,1),af(3).di(:,2),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    nmd(4).sed.freq,nmd(4).sed.diff(:,1),'.',...
    af(4).di(:,1),af(4).di(:,2),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

% %amor
% str.af = '/home/jason/disorder/lj/amor/4x/work';
% af(1).di=load(strcat(str.af,'/Di(wi)_1.dat'));
% str.af = '/home/jason/disorder/lj/amor/8x/work';
% af(2).di=load(strcat(str.af,'/Di(wi)_1.dat'));
% str.af = '/home/jason/disorder/lj/amor/10x/work';
% af(3).di=load(strcat(str.af,'/Di(wi)_1.dat'));
% str.af = '/home/jason/disorder/lj/amor/12x/work';
% af(4).di=load(strcat(str.af,'/Di(wi)_1.dat'));
% str.af = '/home/jason/disorder/lj/amor/rect/24x6x6/work';
% af(5).di=load(strcat(str.af,'/Di(wi)_1.dat'));
% af(5).freq=load(strcat(str.af,'/freq_1.dat'));
% 
% loglog(...
%     af(1).di(:,1),1E-12*af(1).di(:,2),'.',...
%     af(2).di(:,1),1E-9*af(2).di(:,2),'.',...
%     af(3).di(:,1),1E-6*af(3).di(:,2),'.',...
%     af(4).di(:,1),1E-3*af(4).di(:,2),'.',...
%     af(5).freq,af(5).di(:,1),'.',...
%     af(5).freq,af(5).di(:,2),'.',...
%     af(5).freq,af(5).di(:,3),'.'...
%     )



%compare predicted conductivities

af(1).size =[4;6;8;10;12 ];
af(1).cond =[3.23;21.44;79.66;137.29;217.34];

af(2).size =[4;6;8;10;12 ];
af(2).cond =[1.2759;1.567;1.746;1.7814;2.0066];

af(3).size =[4;6;8;10;12 ];
af(3).cond =[0.46701;0.578;0.7009;0.70392;0.80386];

af(4).size =[4;6;8;10;12 ];
af(4).cond =[0.24117;0.296;0.33957;0.34992;0.3567];

af(5).size =[4;6;8;10;12;24 ];
af(5).cond =[0.12999;0.131;0.13989;0.14576;0.13733;0.1398];


GK(1).conc =[...
    0
    0.05
    0.15
    0.5
    ];

GK(1).size =[...
    4
    6
    8
    10
    12
    ];

GK(1).cond =[...
    3.1306 0.0037589
    3.2109 0.025422
    3.2384 0.067034
    3.0827 0.006479
    3.2405 0.043893
    ];

GK(2).cond =[...
    0.73665 0.005542
    0.72065 0.010554
    0.6825 0.0084678
    0.69605 0.01531
    0.78346 0.025184
    ];

GK(3).cond =[...
    0.24852 0.0039003
    0.28899 0.0074171
    0.35572 0.005313
    0.31627 0.0047446
    0.34049 0.0052839
    ];

GK(4).cond =[...
    0.14186 0.0023053
    0.18193 0.0022253
    0.2153 0.0016606
    0.20421 0.0046493
    0.2367 0.0037717
    ];

GK(5).size =[...
    4
    8
    10
    12
    ];

GK(5).conc =[...
    0
    0.05
    0.15
    0.5
    ];

GK(5).cond =[...
    0.16656 0.033505
    0.16844 0.00038194
    0.17869 0.013507
    0.17053 0.0096541
    ];

VC.cond =[...
    3.4 0.4 
    0.83 0.08
    0.42 0.04
    0.28 0.05
    ];

plot(...
    GK(1).conc,...
    [...
    GK(1).cond(5,1)
    GK(2).cond(5,1)
    GK(3).cond(5,1)
    GK(4).cond(5,1) ],'.',...
    0.5,...
    GK(5).cond(4,1)/sqrt(2),...
    GK(1).conc,VC.cond(:,1),'.',...
    [GK(1).conc(2:4,1)],...
    [af(2).cond(5);af(3).cond(5);af(4).cond(5)],'.'...
    )
pause
%1/N 1/k
plot(...
    1./af(2).size,1./af(2).cond/max(1./af(2).cond),'.',...
    1./af(3).size,1./af(3).cond/max(1./af(3).cond),'.',...
    1./af(4).size,1./af(4).cond/max(1./af(4).cond),'.',...
    1./af(5).size,1./af(5).cond/max(1./af(5).cond),'.'...
    )

plot(...
    af(2).size,af(2).cond/max(af(2).cond),'.',...
    af(3).size,af(3).cond/max(af(3).cond),'.',...
    af(4).size,af(4).cond/max(af(4).cond),'.',...
    af(5).size,af(5).cond/max(af(5).cond),'.'...
    )

