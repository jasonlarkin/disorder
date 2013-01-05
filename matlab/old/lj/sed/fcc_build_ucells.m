function [x_ucell] = fcc_build_ucells (alat,num_cells)
% x_ucell = coordinates of the unit cell images
% index = number of unit cell images produced

        x=[ 0.000000   0.000000   0.000000
            0.5000     0.5000     0.000000
            0.5000     0.000000   0.5000
            0.000000   0.5000     0.5000];

A = size(x);
index=1;
NUM_ATOMS = 4*(num_cells^3);

x_ucell(

for i=-num_cells:1:num_cells   
%for i=0:1:num_cells 
    for j=-num_cells:1:num_cells 
    %for j=0:1:num_cells 
        for k=-num_cells:1:num_cells
        %for k=0:1:num_cells
            x_ucell( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 1 ) = x(:,1) + i*alat(1,1) ;
            x_ucell( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 2 ) = x(:,2) + j*alat(1,2) ;
            x_ucell( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 3 ) = x(:,3) + k*alat(1,3) ;
            x_ucell( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 4 ) = index;
            index=index+1;
        end
    end
end

index=1;

for i=0:num_cells    
    for j=0:num_cells        
        for k=0:num_cells
            x_ucell2( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 1 ) = x(:,1) + i*alat(1,1) ;
            x_ucell2( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 2 ) = x(:,2) + j*alat(1,2) ;
            x_ucell2( ((index-1)*A(1,1)+1):((index)*A(1,1)) , 3 ) = x(:,3) + k*alat(1,3) ;
            index=index+1;
        end
    end
end