function x_super = m_build_supercell( x_ucell, latvec, Nx, Ny, Nz )

%build supercell
N_cnt = 1;
for iNx = 0:Nx-1
    for iNy = 0:Ny-1
        for iNz = 0:Nz-1
x_super( (N_cnt-1)*size(x_ucell,1)+1:(N_cnt)*size(x_ucell,1) ,1) =...
        x_ucell(:,1) + iNx*latvec(1,1) +...
            iNy*latvec(2,1) + iNz*latvec(3,1); 
x_super( (N_cnt-1)*size(x_ucell,1)+1:(N_cnt)*size(x_ucell,1) ,2) =...
        x_ucell(:,2) + iNx*latvec(1,2) +...
            iNy*latvec(2,2) + iNz*latvec(3,2);
x_super( (N_cnt-1)*size(x_ucell,1)+1:(N_cnt)*size(x_ucell,1) ,3) =...
        x_ucell(:,3) + iNx*latvec(1,3) +...
            iNy*latvec(2,3) + iNz*latvec(3,3);
        N_cnt =N_cnt+1;
        end
    end
end


end
