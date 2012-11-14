function GK = m_lj_cond_alloy_gk_results_40K
%gk = m_lj_gk_alloy_results
%returns the gk thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------

%pressure correction
%0.5, 8x      0.2573 / 0.2153


GK.conc =[...
    0
    0.05
    0.15
    0.5
    ];

GK.size =[...
    4
    8
    10
    ];

GK.extrap =[...
0.4823
0.2930
0.2323
0.1890
];

GK.cond(:,1) =[...
    0.4684
    0.4949
    0.4837
    ];
%extrapolate 3.3
GK.cond(:,2) =[...
    0.2873
    0.2946
    0.2876
    ];
%extrapolate 0.75
GK.cond(:,3) =[...
    0.1855
    0.2017
    0.2145
    ];
%extrapolate 0.35
GK.cond(:,4) =[...
    0.1223
    0.1452
    0.1578
    ];
%extrapolate 0.28





end



    
