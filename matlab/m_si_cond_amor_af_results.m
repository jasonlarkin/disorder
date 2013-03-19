function gk = m_si_cond_amor_gk_results
%VC = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------

gk.size =[...
    2
    5
%    8
    ];

gk.num_atoms =[...
    216
    1000
    4096
    ];

gk.cond =[...
    1.1479
    1.2578
    ];

gk.extrap=[...
    1
    ];

%0.05: plot(1./[26 34 42 10000000],1./[14.91 15.59 15.83 16.7],'.')
%0.5: plot(1./[26 34 42 10000000],1./[6.424 7.704 7.728 7.83],'.')


end
