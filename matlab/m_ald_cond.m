function cond = m_ald_cond(life,groupvel,VOLUME)
%--------------------------------------------------------------------------
%cond = m_ald_cond(life,groupvel,VOLUME)
%assumes quantities in mks units, not lj
%--------------------------------------------------------------------------

constant = m_constant; lj = m_lj;

cond = ...
    (constant.kb/VOLUME)*sum( life.*(groupvel.^2) );


end



