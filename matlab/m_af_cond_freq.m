function [bins cond] = m_af_cond_Di(freq,Di,Di_lo,Di_hi,VOLUME,NUM_BINS)
%--------------------------------------------------------------------------
%cond = m_ald_cond_freq(freq,life,groupvel,VOLUME)
%assumes quantities in mks units, not lj
%--------------------------------------------------------------------------

constant = m_constant; lj = m_lj;

bins = linspace( Di_lo , Di_hi , NUM_BINS); 

for ibin = 1:length(bins)
    I = find( Di > bins(ibin) & Di < bins(ibin)+bins(2) );
%     life(I)
%     groupvel(I)
    cond(ibin) =... 
        sum((constant.kb/VOLUME)*sum( Di(I) ));
%     pause    
end

end



