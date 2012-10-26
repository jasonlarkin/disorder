function vc = m_si_cond_alloy_nmd_vc_results
%VC = m_si_cond_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-8x systems
%--------------------------------------------------------------------------

vc.size =[...
    4
    6
    8
    ];
vc.conc =[...
    0.0
    0.05
    0.15
    0.5
    ];

vc.cond(1,:) =[...
    4.561131237009280e+02
    1.89
    0.81
    0.86
    ];

vc.cond(2,:) =[...
    4.863966253187967e+02
    2.14
    0.924
    0.956
    ];

vc.cond(3,:) =[...
    4.713022983182889e+02
    2.521210921960882
    1.086860152000911
    1.083547681819946
    ];

vc.extrap=[...
    471.27
    6.023
    2.254
    1.901
    ];

%vc.perfect.cond =[...
%	3.3298
%	3.1220
%	2.8287
%	2.3
%	];


end
