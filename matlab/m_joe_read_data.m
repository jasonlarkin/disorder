function data = m_joe_read_data(str)
%data = m_joe_read_data(str)
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

data.freq = dummy{4}(:)*lj.tau/constant.s2ps; 
data.freq_shiftQ = dummy{5}(:)*lj.tau/constant.s2ps;
data.life = dummy{6}(:)/lj.tau*constant.s2ps;

data.kpt(:,1) = dummy{1}(:); 
data.kpt(:,2) = dummy{2}(:); 
data.kpt(:,3) = dummy{3}(:);

data.freq = dummy{4}(:)*lj.tau/constant.s2ps; 

data.freq_shift = dummy{7}(:)*lj.tau/constant.s2ps;
data.life = dummy{8}(:)/lj.tau*constant.s2ps;

data.vel(:,1) = dummy{9}(:) * lj.tau / lj.sigma; 
data.vel(:,2) = dummy{10}(:)* lj.tau / lj.sigma;
data.vel(:,3) = dummy{11}(:)* lj.tau / lj.sigma;

end