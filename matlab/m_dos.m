function [DOSX DOSY] = m_dos(freq, DOS_BIN, NUM_ATOMS)
%--------------------------------------------------------------------------
%[DOSX DOSY] = m_dos(freq, DOS_BIN, NUM_ATOMS)
%--------------------------------------------------------------------------

I = find( freq~=0);

size(I)

[DOSY DOSX]= hist(freq(I),ceil(NUM_ATOMS/DOS_BIN));
DOSY = DOSY/sum(DOSY);

end







