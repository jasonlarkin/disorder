function x_super = m_create_supercell( x_ucell, latvec, N1, N2, N3 )
%m_wurzite_unit_cell_prim  
%   x0 = f( a ) where a is the lattice constant
%
%   See also m_diamond_unit_cell_prim

%build supercell
N_cnt = 1;
for iN1 = 0:N1-1
    for iN2 = 0:N2-1
        for iN3 = 0:N3-1
x_super( (N_cnt-1)*size(x_ucell,1)+1:(N_cnt)*size(x_ucell,1) ,1) =...
        x_ucell(:,1) + iN1*latvec(1,1) +...
            iN2*latvec(2,1) + iN3*latvec(3,1); 
x_super( (N_cnt-1)*size(x_ucell,1)+1:(N_cnt)*size(x_ucell,1) ,2) =...
        x_ucell(:,2) + iN1*latvec(1,2) +...
            iN2*latvec(2,2) + iN3*latvec(3,2);
x_super( (N_cnt-1)*size(x_ucell,1)+1:(N_cnt)*size(x_ucell,1) ,3) =...
        x_ucell(:,3) + iN1*latvec(1,3) +...
            iN2*latvec(2,3) + iN3*latvec(3,3);
        N_cnt =N_cnt+1;
        end
    end
end


end
