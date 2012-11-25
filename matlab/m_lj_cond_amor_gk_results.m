function gk = m_lj_cond_amor_gk_results
%gk = m_lj_gk_alloy_results
%returns the gk thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------

gk.size =[...
    4
    8
    10
    12
    ];


gk.cond =[...
    0.16656 0.033505
    0.16844 0.00038194
    0.17869 0.013507
    0.17053 0.0096541
    ];

gk.extrap =[...
mean(gk.cond(:,1))
];


end



    
