%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%INPUT
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
NMD.LJ.eps = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
NMD.LJ.sigma = 3.4E-10;                 %Angstroms 3.4E-10 meters
NMD.LJ.mass = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
NMD.LJ.tau = sqrt((NMD.LJ.mass*(NMD.LJ.sigma^2))/NMD.LJ.eps);
NMD.constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
NMD.constant.hbar = 1.054E-34;                %J/s
NMD.constant.i = sqrt(-1);
NMD.constant.c = 29979245800.00019;      %cm/s
NMD.constant.s2ps = 1E-12;
NMD.constant.ang2m = 1E-10;
NMD.constant.eV2J = 1.60217646E-19;

%--------------------------------------------------------------------------
    [tmp,NMD.str.main]=system('pwd');
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
    NMD.alloy_conc = 0.0;
%--------------------------------------------------------------------------
    NMD.m(1) = 1.0; NMD.m(2) = 3.0; NMD.NUM_ATOMS_TYPE = 2;
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
    NMD.Nx = 8; NMD.Ny = 8; NMD.Nz = 8;
%--------------------------------------------------------------------------
    NMD.seed.alloy = 1;
    NMD.seed.initial = 1:10;
%--------------------------------------------------------------------------

% alat = 5.269/3.4        %0K 
NMD.alat = 5.290/3.4;       %10K
% alat = 5.315/3.4;       %20K
% alat = 5.341/3.4;       %30K
% alat = 5.370/3.4;       %40K
% alat = 5.401/3.4;       %50K
% alat = 5.436/3.4;       %60K
% alat = 5.476/3.4;       %70K
% alat = 5.527/3.4;       %80K
%--------------------------------------------------------------------------

%Unit cell and lattice vectors
dummy = [   1.0 0.0  0.0 
            0.0  1.0 0.0 
            0.0  0.0  1.0
            0.0  0.0  0.0
            0.5  0.5  0.0
            0.5  0.0  0.5
            0.0  0.5  0.5];
    
%Define box size and conventional cell lattice parameters
    NMD.latvec(1,1) = dummy(1,1); NMD.latvec(1,2) = dummy(1,2); 
    NMD.latvec(1,3) = dummy(1,3);
    NMD.latvec(2,1) = dummy(2,1); NMD.latvec(2,2) = dummy(2,2); 
    NMD.latvec(2,3) = dummy(2,3);    
    NMD.latvec(3,1) = dummy(3,1); NMD.latvec(3,2) = dummy(3,2); 
    NMD.latvec(3,3) = dummy(3,3);
    
    NMD.latvec = NMD.alat*NMD.latvec;
    
NMD.latvec_rec = [   1.0 0.0  0.0 
                    0.0  1.0 0.0 
                    0.0  0.0  1.0];
    
%first 3 rows are the lattice vectors
    NMD.x.direct = dummy(4:length(dummy),:);
    
    NMD.x.cart(:,1) = NMD.x.direct(:,1)*NMD.latvec(1,1) +...
        NMD.x.direct(:,2)*NMD.latvec(2,1) +...
        NMD.x.direct(:,3)*NMD.latvec(3,1) ;
	NMD.x.cart(:,2) = NMD.x.direct(:,1)*NMD.latvec(1,2) +...
        NMD.x.direct(:,2)*NMD.latvec(2,2) +...
        NMD.x.direct(:,3)*NMD.latvec(3,2) ;
	NMD.x.cart(:,3) = NMD.x.direct(:,1)*NMD.latvec(1,3) +...
        NMD.x.direct(:,2)*NMD.latvec(2,3) +...
        NMD.x.direct(:,3)*NMD.latvec(3,3) ;

%--------------------------------------------------------------------------

%build supercell
N_cnt = 1;
for iNx = 0:NMD.Nx-1
    for iNy = 0:NMD.Ny-1
        for iNz = 0:NMD.Nz-1
NMD.x0( (N_cnt-1)*size(NMD.x.direct,1)+1:(N_cnt)*size(NMD.x.direct,1) ,3) =...
        NMD.x.cart(:,1) + iNx * NMD.latvec(1,1) +...
            iNy*NMD.latvec(2,1) + iNz*NMD.latvec(3,1); 
NMD.x0( (N_cnt-1)*size(NMD.x.direct,1)+1:(N_cnt)*size(NMD.x.direct,1) ,4) =...
        NMD.x.cart(:,2) + iNx * NMD.latvec(1,2) +...
            iNy*NMD.latvec(2,2) + iNz*NMD.latvec(3,2);
NMD.x0( (N_cnt-1)*size(NMD.x.direct,1)+1:(N_cnt)*size(NMD.x.direct,1) ,5) =...
        NMD.x.cart(:,3) + iNx * NMD.latvec(1,3) +...
            iNy*NMD.latvec(2,3) + iNz*NMD.latvec(3,3);
        N_cnt =N_cnt+1;
        end
    end
end

NMD.NUM_ATOMS = size(NMD.x0,1);


%create kptlist in integers
cnt=1;
for ix=(-(NMD.Nx/2)+1):1:(NMD.Nx/2)
    for iy=(-(NMD.Ny/2)+1):1:(NMD.Ny/2)
        for iz=(-(NMD.Nz/2)+1):1:(NMD.Nz/2)            
        NMD.kpt.integer(cnt,:) = [ix iy iz];
        NMD.kpt.cart(cnt,1:3) = ix/NMD.Nx*NMD.latvec_rec(1,:) +...
            iy/NMD.Ny*NMD.latvec_rec(2,:) +...
            iz/NMD.Nz*NMD.latvec_rec(3,:) ;
        NMD.kpt.NUM_KPTS = cnt;
        cnt=cnt+1;
        end
    end
end
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%INPUT
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
