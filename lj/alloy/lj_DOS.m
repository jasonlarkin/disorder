
function [DOSX DOSY] = lj_DOS(freq, DOS_BIN, NUM_ATOMS)

[DOSY DOSX]= hist(freq,ceil(NUM_ATOMS/DOS_BIN));

end







