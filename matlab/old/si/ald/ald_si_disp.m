
    str.main=strcat('/home/jason/Documents/ald/si/');

%INITIAL POSITIONS: Set initial positions for id matching
    str.read=strcat(str.main,'si_prim_uc.txt');
    dummy = load(str.read);
%Define box size and conventional cell lattice parameters
    LD.latvec(1,1) = dummy(1,1); LD.latvec(1,2) = dummy(1,2); 
    LD.latvec(1,3) = dummy(1,3);
    LD.latvec(2,1) = dummy(2,1); LD.latvec(2,2) = dummy(2,2); 
    LD.latvec(2,3) = dummy(2,3);    
    LD.latvec(3,1) = dummy(3,1); LD.latvec(3,2) = dummy(3,2);
    LD.latvec(3,3) = dummy(3,3);
%first 3 rows are the lattice vectors
    LD.x.central.direct = dummy(4:length(dummy),:);
    
    LD.x.central.cart(:,1) = LD.x.central.direct(:,1)*LD.latvec(1,1) + ...
    LD.x.central.direct(:,2)*LD.latvec(2,1) + ...
    LD.x.central.direct(:,3)*LD.latvec(3,1) ;
	LD.x.central.cart(:,2) = LD.x.central.direct(:,1)*LD.latvec(1,2) + ...
    LD.x.central.direct(:,2)*LD.latvec(2,2) + ...
    LD.x.central.direct(:,3)*LD.latvec(3,2) ;
	LD.x.central.cart(:,3) = LD.x.central.direct(:,1)*LD.latvec(1,3) + ...
    LD.x.central.direct(:,2)*LD.latvec(2,3) + ...
    LD.x.central.direct(:,3)*LD.latvec(3,3) ;
%define parameters
LD.NUM_ATOMS_UCELL = size(LD.x.central.cart,1);

%number of unit cell copies in the directions    
    LD.Nx = 1; LD.Ny = 1; LD.Nz = 1;

    pause
    
%cut-off radiu
    LD.cutoff = 3.0;    %ang
    LD.disp = 10E-3     %ang

    N_cnt = 1;
    for iNx = -LD.Nx:1:LD.Nx
        for iNy = -LD.Ny:1:LD.Ny
            for iNz = -LD.Nz:1:LD.Nz
                LD.x.SC( (N_cnt-1)*size(LD.x.central.direct,1)+1:(N_cnt)*size(LD.x.central.direct,1) ,1) = ...
                    LD.x.central.cart(:,1) + iNx * LD.latvec(1,1) + iNy*LD.latvec(2,1) + iNz*LD.latvec(3,1); 
                LD.x.SC( (N_cnt-1)*size(LD.x.central.direct,1)+1:(N_cnt)*size(LD.x.central.direct,1) ,2) = ...
                    LD.x.central.cart(:,2) + iNx * LD.latvec(1,2) + iNy*LD.latvec(2,2) + iNz*LD.latvec(3,2);
                LD.x.SC( (N_cnt-1)*size(LD.x.central.direct,1)+1:(N_cnt)*size(LD.x.central.direct,1) ,3) = ... 
                    LD.x.central.cart(:,3) + iNx * LD.latvec(1,3) + iNy*LD.latvec(2,3) + iNz*LD.latvec(3,3);
                N_cnt = N_cnt+1;
            end
        end
    end

%remove central cell
    %[TF,LOC] = ismember(LD.x.central.cart, LD.x.cloud,'rows');
%    [TF,LOC] = setdiff(LD.x.SC,LD.x.central.cart,'rows');
%    LD.x.cloud = LD.x.SC(sort(LOC),:);
    LD.x.cloud = LD.x.SC;
    
%apply the cutoff radius
    iatom = 1;
    LD.cloud.rij = sqrt( ...
    bsxfun(@minus,LD.x.cloud(:,1),LD.x.central.cart(iatom,1)).^2 + ...
    bsxfun(@minus,LD.x.cloud(:,2),LD.x.central.cart(iatom,2)).^2 + ... 
    bsxfun(@minus,LD.x.cloud(:,3),LD.x.central.cart(iatom,3)).^2        ); 
    %[I.(:,1),J] = find(LD.cloud.rij<LD.cutoff);

for iatom = 2:LD.NUM_ATOMS_UCELL
    LD.cloud.rij = sqrt( ...
    bsxfun(@minus,LD.x.cloud(:,1),LD.x.central.cart(iatom,1)).^2 + ...
    bsxfun(@minus,LD.x.cloud(:,2),LD.x.central.cart(iatom,2)).^2 + ... 
    bsxfun(@minus,LD.x.cloud(:,3),LD.x.central.cart(iatom,3)).^2        ); 
    [I.cloud(:,2),J] = find(LD.cloud.rij<LD.cutoff);
    [TF,LOC] = ismember(I(:,2), I(:,1),'rows');
end
pause
%    LD.x.cloud = LD.x.cloud(I,:);

%plot the central cell and the cloud
    plot3(LD.x.cloud(:,1),LD.x.cloud(:,2),LD.x.cloud(:,3),'.', ... 
    LD.x.central.cart(:,1),LD.x.central.cart(:,2),LD.x.central.cart(:,3),'.','markersize',20)

    pause
            

%     str.write=strcat(str.main,'x0.data');
%     output = [size(LD.SC.x,1) size(LD.x.cart,1) LD.latvec(1,1)*(LD.Nx+1) LD.latvec(2,2)*(LD.Ny+1) LD.latvec(3,3)*(LD.Nz+1) ];
%     dlmwrite(str.write,output,'-append','delimiter',' ');
%     dlmwrite(str.write,LD.SC.x,'-append','delimiter',' ');

            