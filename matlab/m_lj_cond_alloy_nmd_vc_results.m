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
