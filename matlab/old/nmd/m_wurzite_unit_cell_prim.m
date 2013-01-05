function x0 = m_wurzite_unit_cell_prim( x0 )

%expects

x0.LJ.eps = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
x0.LJ.sigma = 3.4E-10;                 %Angstroms 3.4E-10 meters
x0.LJ.mass = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
x0.LJ.tau = sqrt((x0.LJ.mass*(x0.LJ.sigma^2))/x0.LJ.eps);
x0.constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
x0.constant.hbar = 1.054E-34;                %J/s
x0.constant.i = sqrt(-1);
x0.constant.c = 29979245800.00019;      %cm/s
x0.constant.s2ps = 1E-12;
x0.constant.ang2m = 1E-10;
x0.constant.eV2J = 1.60217646E-19;

x0.latvec = 	[1.0 0.0  0.0 
                    0.0  1.0 0.0 
                    0.0  0.0  1.0 ];
                
x0.latvec_int = 	[1.0 0.0  0.0 
                    0.0  1.0 0.0 
                    0.0  0.0  1.0 ];
            
x0.ucell.direct = 	[0.0  0.0  0.0
                        0.5  0.5  0.0
                        0.5  0.0  0.5
                        0.0  0.5  0.5];

x0.latvec = bsxfun(@times,x0.latvec,x0.alat);

x0.ucell.cart = m_direct2cart( x0.ucell.direct , x0.latvec );

x0.latvec_rec = [1.0 0.0  0.0 
                    0.0  1.0 0.0 
                    0.0  0.0  1.0];

end
