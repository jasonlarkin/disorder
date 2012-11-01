function ald = m_lj_cond_alloy_ald_results
%VC = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------

ald.size =[...
    4
    6
    8
    10
    12
    ];

ald.extrap =[...
    3.3 0.0 
    0.3313 0.00
    0.2248 0.00
    0.1812 0.00
    ];

ald.perfect =[...
	3.3298
	3.1220
	2.8287
	2.3
	];

end
