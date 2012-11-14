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
    ];
ald.conc =[...
    0.0
    0.05
    0.15
    0.5
    ];

ald.cond(1,:) =[...
    5.649245774329065e+02
    2.143079497782264
    0.986079180684062
    0.878562067149744
    ];

ald.cond(2,:) =[...
    5.352363447895174e+02
    2.166258831967184
    0.995941606289517
    0.886738584083702
    ];

ald.cond(3,:) =[...
    5.033323408221058e+02
    2.455751701116387
    1.130323109327582
    1.005029813906340
    ];

ald.cond(4,:) =[...
    4.730895365176177e+02
    6.035905092974989
    1.268797577459464
    1.127809095647059
    ];

ald.cond(5,:) =[...
    0
    6.556041670579716
    0
    2.6912
    ];

ald.cond(6,:) =[...
    0
    6.917705643973492
    0
    2.8390
    ];

ald.cond(7,:) =[...
    0
    7.112155150146815
    0
    2.9210
    ];

ald.cond(8,:) =[...
    0
    7.285434065223492
    0
    2.9923
    ];

ald.cond(9,:) =[...
    0
    7.2976
    0
    2.9990
    ];

ald.extrap=[...
    503.88
    8.5031
    4.0519
    3.6173
    ];

ald.extrap_diff=[...
    503.88
    8.5031*1.0528
    4.0519*1.0580
    3.6173*1.0604
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


