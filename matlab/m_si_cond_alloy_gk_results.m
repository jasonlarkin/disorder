function gk = m_si_cond_alloy_gk_results
%VC = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------

gk.size =[...
    4
    6
    8
    10
    ];
gk.conc =[...
    0.0
    0.05
    0.15
    0.5
    ];

gk.cond(1,:) =[...
    509.56
    5.1143
    2.498
    2.0
    ];

gk.cond(2,:) =[...
    492.41
    6.8646
    2.7766
    2.234
    ];

gk.cond(3,:) =[...
    316.69
    6.9093
    2.8383
    2.6747
    ];

gk.cond(4,:) =[...
    521.06
    6.7848
    3.0019
    2.7917
    ];

gk.extrap=[...
    507.67
    6.843
    3.019
    2.312
    ];




end
