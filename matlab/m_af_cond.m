function cond = m_af_cond(freq,Di,VOLUME)
%--------------------------------------------------------------------------
%cond = m_ald_cond_freq(freq,life,groupvel,VOLUME)
%assumes quantities in mks units, not lj
%--------------------------------------------------------------------------

constant = m_constant; lj = m_lj;

    cond =... 
        (constant.kb/VOLUME)*sum( Di );
    

end



