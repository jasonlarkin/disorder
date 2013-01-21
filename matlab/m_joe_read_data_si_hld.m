function data = m_joe_read_data_si_hld(str)
%data = m_joe_read_data_hld(str)
%opens joes ald data, returns
%data.kpt(:,1:3)
%data.freq(1:NUM_MODES)
%data.freq_shift3(1:NUM_MODES)
%data.freq_shift4(1:NUM_MODES)
%data.life(1:NUM_MODES)

fid = fopen(str,'rt'); 
dummy = textscan(fid, '%f%f%f%f%f%f%f%f%f%f%f', 'HeaderLines',1); 
fclose(fid); 

lj = m_lj; constant = m_constant;

data.kpt(:,1) = dummy{1}(:); 
data.kpt(:,2) = dummy{2}(:); 
data.kpt(:,3) = dummy{3}(:);

data.freq = dummy{4}(:) / constant.s2ps; 

data.vel(:,1) = dummy{5}(:) ; 
data.vel(:,2) = dummy{6}(:) ;
data.vel(:,3) = dummy{7}(:) ;

end