function ald = m_lj_cond_alloy_ald_results_40K
%VC = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------
lj=m_lj;constant=m_constant;

ald.m = [1;1.1;1.3;2]; 
ald.prefactor =...
    (3/2)*( (pi/6)^(1/3))*constant.kb*...
    lj.num_density_40K^(2/3);
ald.high_scatter = [...
    ald.prefactor*lj.sound_40K*(lj.sigma/lj.tau)/sqrt(ald.m(1))
    ald.prefactor*lj.sound_40K*(lj.sigma/lj.tau)/sqrt(ald.m(2))
    ald.prefactor*lj.sound_40K*(lj.sigma/lj.tau)/sqrt(ald.m(3))
    ald.prefactor*lj.sound_40K*(lj.sigma/lj.tau)/sqrt(ald.m(4))
    ];

ald.prefactor_70K =...
    (3/2)*( (pi/6)^(1/3))*constant.kb*...
    lj.num_density_70K^(2/3);
ald.high_scatter_70K = [...
    ald.prefactor_70K*lj.sound_70K*(lj.sigma/lj.tau)/sqrt(ald.m(1))
    ald.prefactor_70K*lj.sound_70K*(lj.sigma/lj.tau)/sqrt(ald.m(2))
    ald.prefactor_70K*lj.sound_70K*(lj.sigma/lj.tau)/sqrt(ald.m(3))
    ald.prefactor_70K*lj.sound_70K*(lj.sigma/lj.tau)/sqrt(ald.m(4))
    ];

ald.conc =[...
    0
    0.05
    0.15
    0.5
    ];

ald.size =[...
    4
    8
    10
    12
    14
    ];

ald.extrap =[...
%0.558628724071911
0.63000000
0.179195578782107
0.117569026435618
0.104669595329213
    ];
ald.extrap_diff =[...
0 
0.223683382985014
0.166568633106425
0.142031693876666
    ];

ald.cond(:,1)=[...
0.570209
0.585133
0.59505
0.599732
0.606343
];

ald.cond(:,2)=[...
0.182506601728401
0.174970284103076
0.175862434643627
0.176184979438895
0.176856848630628
];
ald.cond_diff(:,2)=[...
0.229692628142856
0.222166843218315
0.222385419780018
0.222331671797894
0.223005887110998
];

ald.cond(:,3)=[...
0.111780072783673
0.109938003654929
0.111567316664371
0.112351753488220
0.113099209356144
];
ald.cond_diff(:,3)=[...
0.158905194727666
0.159012037437951
0.160468620121195
0.161355759798540
0.162221392695606
];

ald.cond(:,4)=[...
0.102260760557336
0.099546605449672
0.100500927893657
0.101078571531107
0.101747360368463
];
ald.cond_diff(:,4)=[...
0.139071906699021
0.137761914971645
0.138387374592878
0.138948962440123
0.139673321108783
];

ald.perfect =[...
	0.521330164130501
	0.497068651572859
	0.457236113196516
	0.368635803013114
	];

end
