function AF = m_af_load( str )
%af = m_af_load( x0 , eps , str_main , tf_eig )
%loads an af object with the following:
%af.

str.AF = str;
AF.freq = load(strcat(str.AF,'AF_freq_1.dat'));
AF.Di =...
    load(strcat(str.AF,'AF_Di(wi)_1.dat'));
AF.x0 = load(strcat(str.AF,'x0.data'));
AF.NUM_ATOMS = AF.x0(1,1); 
AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
AF.Lx = AF.x0(1,3); 
AF.Ly = AF.x0(1,4); 
AF.Lz = AF.x0(1,5);
AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; 
AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);
AF.x0 = AF.x0(2:size(AF.x0,1),:);

end
