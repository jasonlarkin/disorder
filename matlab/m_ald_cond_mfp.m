function [mfp_bins cond_mfp] = m_ald_cond_mfp(freq,life,groupvel,VOLUME,NUM_BINS)
%--------------------------------------------------------------------------
%cond = m_ald_cond_mfp(freq,life,groupvel,VOLUME)
%assumes quantities in mks units, not lj
%--------------------------------------------------------------------------

min_mfp = 1E-9;

mfp = life.*sqrt(groupvel(:,1).^2 + groupvel(:,2).^2 + groupvel(:,3).^2 );
constant = m_constant; lj = m_lj;
mfp_bins = linspace( min_mfp , 1.1*max(mfp) , NUM_BINS); 

min(mfp)
max(mfp)

%freq_bins
for ibin = 2:length(mfp_bins)
    I = find( mfp > mfp_bins(ibin-1) & mfp < mfp_bins(ibin) );
    cond_mfp(ibin) =... 
        sum((constant.kb/VOLUME)*sum( life(I).*(groupvel(I,1).^2) ) );
end

end



