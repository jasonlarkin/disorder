
kpt(:,1) = linspace(0.0,0.5,100)
kpt(:,2) = 0
kpt(:,3) = 0i

for ikpt = 1:size(kpt,1)
gulp_lj_eig(kpt,NUM_ATOMS_UCELL,MASS,str_main,str_matlab,name)
end
