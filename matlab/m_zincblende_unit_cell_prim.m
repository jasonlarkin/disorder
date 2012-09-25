function ucell = m_zincblende_unit_cell_prim( a )
%m_wurzite_unit_cell_prim  
%   x0 = f( a ) where a is the lattice constant
%
%   See also m_diamond_unit_cell_prim
                
ucell.latvec_int = 	[   0.0     0.5     0.5 
                        0.5     0.0     0.5 
                        0.5     0.5     0.0     ];
            
ucell.direct = 	[   0.0     0.0     0.0
                        0.25    0.25    0.25];
ucell.type = [1;2];

ucell.latvec = bsxfun(@times,ucell.latvec_int,a);

ucell.cart = m_direct2cart( ucell.direct , ucell.latvec );

% x0.latvec_rec = [1.0 0.0  0.0 
%                     0.0  1.0 0.0 
%                     0.0  0.0  1.0];

end
