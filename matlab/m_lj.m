function lj = m_lj
% lj = m_lj returns an lj structrue with Ar parameters
%--------------------------------------------------------------------------
lj.eps = 1.67E-21;              
lj.sigma = 3.4E-10;                
lj.mass = 6.6326E-26; 
lj.mass_au = 39.948;
lj.tau = sqrt((lj.mass*(lj.sigma^2))/lj.eps); 

lj.alat = 1.5636;
lj.alat_40K = 1.58;
lj.alat_70K = 1.61;

vg_scaling = 0.75;
lj.sound =...
    ( (2/3)*(623.27) + (1/3)*(1412.11) )*(1/vg_scaling)...
    *lj.tau/lj.sigma...
    ;
lj.sound_40K =...
    ( (2/3)*(769.40) + (1/3)*(1386.24) )...
    *lj.tau/lj.sigma...
    ;
lj.sound_70K =...
    ( (2/3)*(769.40) + (1/3)*(1386.24) )...
    *lj.tau/lj.sigma*(lj.alat_70K/lj.alat_40K)*0.95...
    ;

lj.num_atom_ucell=4;
lj.num_density = (lj.num_atom_ucell / (lj.alat^3*lj.sigma^3));
lj.num_density_40K = (lj.num_atom_ucell / (lj.alat_40K^3*lj.sigma^3));
lj.num_density_70K = (lj.num_atom_ucell / (lj.alat_70K^3*lj.sigma^3));

lj.Tmelt = 82;

end
%--------------------------------------------------------------------------	


