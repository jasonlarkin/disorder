clear
kpt(:,1) = linspace(0.0,0.5,100);
kpt(:,2) = 0;
kpt(:,3) = 0;

lj = m_lj; constant = m_constant;

str_main = '/home/jason/disorder/kevin/';
str_matlab = '/home/jason/disorder/matlab/';
name = 'gulp_disp_lj_conv.tmp';
NUM_ATOMS_UCELL = 4;
MASS = lj.mass*1.0 *constant.avog*constant.g2kg;


for ikpt = 1:size(kpt,1)
kpt(ikpt,:)
freq = gulp_lj_freq(kpt(ikpt,:),NUM_ATOMS_UCELL,MASS,str_main,str_matlab,name)

eig = gulp_lj_eig(kpt(ikpt,:),NUM_ATOMS_UCELL,MASS,str_main,str_matlab,name)

vel = gulp_lj_vel(kpt(ikpt,:),NUM_ATOMS_UCELL,MASS,str_main,str_matlab,name)
pause
end
