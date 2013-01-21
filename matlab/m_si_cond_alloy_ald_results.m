function ald = m_si_cond_alloy_ald_results
%VC = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------
si=m_si; constant=m_constant;

ald.size =[...
    4
    6
    8
    10
    12
    14
    16
    18
    20
    22
    24
    26
    28
    30
    32
    38
    42
    46
    50
    54
    58
    62
    66
    ];
ald.conc =[...
    0.0
    0.05
    0.15
    0.5
    ];
%4
ald.cond(1,:) =[...
    0
    0
    0
    0
    ];
%6
ald.cond(2,:) =[...
    535.2363
    4.7755
    0
    1.9674
    ];
%8
ald.cond(3,:) =[...
    0
    0
    0
    0
    ];
%10
ald.cond(4,:) =[...
    474.2254
    13.0643
    0
    2.5001
    ];
%12
ald.cond(5,:) =[...
    0
    0
    0
    0
    ];
%14
ald.cond(6,:) =[...
    0
    0
    0
    0
    ];
%16
ald.cond(7,:) =[...
    484.7576
    15.2080
    0
    6.4401
    ];
%18
ald.cond(8,:) =[...
    0
    0
    0
    0
    ];
%20
ald.cond(9,:) =[...
    482.0568
    15.5285
    0
    6.6052
    ];
%22
ald.cond(10,:) =[...
    481.267
    0
    0
    0
    ];
%24
ald.cond(11,:) =[...
    488.9492
    15.5124
    0
    6.6201
    ];
%26
ald.cond(12,:) =[...
    0
    0
    0
    0
    ];
%28
ald.cond(13,:) =[...
    479.4841
    16.7795
    0
    7.6404
    ];
%30
ald.cond(14,:) =[...
    0
    0
    0
    0
    ];
%32
ald.cond(15,:) =[...
    481.3451
    16.0922
    0
    7.2521
    ];
%38
ald.cond(16,:) =[...
    483.2930
    15.4022
    0
    6.8915
    ];
%42
ald.cond(17,:) =[...
    0
    0
    0
    0
    ];
%46
ald.cond(18,:) =[...
    0
    0
    0
    0
    ];
%50
ald.cond(19,:) =[...
    0
    0
    0
    0
    ];
%54
ald.cond(20,:) =[...
    0
    0
    0
    0
    ];
%58
ald.cond(21,:) =[...
    0
    0
    0
    0
    ];
%62
ald.cond(22,:) =[...
    0
    0
    0
    0
    ];
%66
ald.cond(23,:) =[...
    0
    0
    0
    0
    ];


ald.extrap=[...
    503.88
    7.4089
    4.0519
    3.0607
    ];

ald.extrap_diff=[...
    0.0
    7.4089*1.0528
    4.0519*1.0580
    3.0607*1.0604
    ];

ald.m = [1;1.1;1.3;2]; 
ald.prefactor =...
    (3/2)*( (pi/6)^(1/3))*constant.kb*...
    si.num_density^(2/3);
ald.high_scatter = [...
    ald.prefactor*si.sound_used/sqrt(ald.m(1))
    ald.prefactor*si.sound_used/sqrt(ald.m(2))
    ald.prefactor*si.sound_used/sqrt(ald.m(3))
    ald.prefactor*si.sound_used/sqrt(ald.m(4))
    ];

    
end


