function [dosx dosy] = m_dos(freq, NUM_BINS , broaden, VOLUME)
%--------------------------------------------------------------------------
%[DOSX DOSY] = m_dos(freq, DOS_BIN, broaden)
%--------------------------------------------------------------------------

constant = m_constant; lj = m_lj;

% freq_bins = linspace( 0 , max(freq) , NUM_BINS); 
% [I,J] = sort(freq); freq_sorted = freq(J);
% 
% freq_sorted(1:10)
% dw_avg =...
%     real(mean(freq_sorted(2:length(freq))-freq_sorted(1:length(freq)-1)));
% dw_avg
% freq_bins
% for ibin = 1:length(freq_bins)
% %Lorentzian
% delwij = ...
%     freq_sorted - freq_bins(ibin) ;
% lor = (1.0/pi)*(dw_avg*broaden./( delwij.^2 + dw_avg^2 ) );
% dosy(ibin) = sum(lor.*freq_sorted);
% dosx(ibin) = freq_bins(ibin);
% end
% dosy = dosy/sum(dosy);

% %remove 0-freq
% I = find( freq~=0);
% size(I)
% [dosy dosx]= hist(freq(I),NUM_BINS);
% dosy = dosy/sum(dosy);

%remove 0-freq
[dosy dosx]= hist(freq,NUM_BINS);
dosy = dosy/sum(dosy);
dosx = dosx*(max(freq)/NUM_BINS)/VOLUME;

end







