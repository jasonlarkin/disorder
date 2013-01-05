function vc = m_lj_cond_alloy_nmd_vc_results
%vc = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------
lj = m_lj; constant = m_constant;

vc.m = [1;1.1;1.3;2]; 
vc.prefactor =...
    (3/2)*( (pi/6)^(1/3))*constant.kb*...
    lj.num_density^(2/3);
vc.high_scatter = [...
    vc.prefactor*lj.sound*(lj.sigma/lj.tau)/sqrt(vc.m(1))
    vc.prefactor*lj.sound*(lj.sigma/lj.tau)/sqrt(vc.m(2))
    vc.prefactor*lj.sound*(lj.sigma/lj.tau)/sqrt(vc.m(3))
    vc.prefactor*lj.sound*(lj.sigma/lj.tau)/sqrt(vc.m(4))
    ];
    
vc.size =[...
    4
    6
    8
    10
    ];

%vc.sed_paper.conv=[...
%1.103910
%0
%1.09190
%0
%];

vc.sed_paper.conv_20K=[...
1.103910
1.12
1.15
1.18
];

%vc.sed_paper.prim=[...
%0
%1.2415
%0
%1.31234
%];

vc.sed_paper.prim_20K=[...
1.11
1.13
1.14
1.17
];

vc.extrap =[...
    3.3298
    0.7550
    0.3614
    0.3058
    ];
vc.extrap_diff =[...
    0.0
    0.8002
    0.4464
    0.3497
    ];

vc.size2 =[...
    4
    6
    8
    10
    12
    ];

vc.cond_conv(:,1)=[...
3.05
3.13
3.20
3.24
3.26
];

vc.cond_prim(:,1)=[...
3.07
3.15
3.22
3.25
3.28
];

vc.cond2(:,2)=[...
0.644
0.701
0.713
0.724
0.73
];

vc.cond(:,2)=[...
0.684
0.701
0.713
0.7292
];
vc.cond_diff(:,2)=[...
0.704
0.724
0.744
0.7641
];

vc.cond(:,3)=[...
1
1
1
0.3619
];
vc.cond_diff(:,3)=[...
0.308
0.334
0.342
0.3994
];

vc.cond(:,4)=[...
0.198
0.221
0.234
0.2569
];
vc.cond_diff(:,4)=[...
0.253
0.283
0.295
0.3014
];

vc.perfect =[...
	3.3298
	3.1220
	2.8287
	];



end
