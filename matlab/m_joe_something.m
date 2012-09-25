function data = m_joe_read_shift(

fid = fopen('DQ20091222000002.txt','rt'); 
indata = textscan(fid, '%f', 'HeaderLines',1); 
fclose(fid); 
yourdata = indata{1};

end