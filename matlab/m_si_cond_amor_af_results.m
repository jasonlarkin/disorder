function af = m_si_cond_amor_gk_results
%VC = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------

gk.normand.size =[...
    2
    5
%    8
    ];
gk.normand.num_atoms =[...
    216
    1000
    4096
    ];
gk.normand.cond =[...
    1.1479
    1.2578
    ];

gk.donadio.size =[...
    4
%    8
    ];
gk.donadio.num_atoms =[...
    512
    ];
gk.donadio.cond =[...
    1.1479
    ];

gk.extrap=[...
    1
    ];

%0.05: plot(1./[26 34 42 10000000],1./[14.91 15.59 15.83 16.7],'.')
%0.5: plot(1./[26 34 42 10000000],1./[6.424 7.704 7.728 7.83],'.')


end
