function [freq_bins cond_freq] = m_ald_cond_freq(freq,life,groupvel,VOLUME,NUM_BINS)
%--------------------------------------------------------------------------
%cond = m_ald_cond_freq(freq,life,groupvel,VOLUME)
%assumes quantities in mks units, not lj
%--------------------------------------------------------------------------

constant = m_constant; lj = m_lj;

freq_bins = linspace( 0 , max(freq) , NUM_BINS); 

max(freq)

%freq_bins

for ibin = 1:length(freq_bins)
    
    I = find( freq > freq_bins(ibin) & freq < freq_bins(ibin)+freq_bins(2) );
    
%     life(I)
%     groupvel(I)
    
    cond_freq(ibin) =... 
        sum((constant.kb/VOLUME)*sum( life(I).*(groupvel(I).^2) ) );
    
%     pause
    
end


end



