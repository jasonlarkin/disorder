function data = m_joe_read_shift(str)

fid = fopen(str,'rt'); 
dummy = textscan(fid, '%f%f%f%f', 'HeaderLines',1); 
fclose(fid); 
%yourdata = indata{1};

data.freq = dummy{1}(:); data.freq_shift3 = dummy{2}(:);
data.freq_shift4 = dummy{3}(:); data.life = dummy{4}(:);

end