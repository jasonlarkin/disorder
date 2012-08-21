clear
%--------------INPUT-------------------------------------------------------
%LJ Potential and Material Parameters
LJ.eps = 1.67E-21;              
LJ.sigma = 3.4E-10;                
LJ.mass = 6.6326E-26;               
LJ.tau = sqrt((LJ.mass*(LJ.sigma^2))/LJ.eps);
%potential cut off
LD.a2 = 2.5^2;
%Lorentzian cutoff
LD.deltaL = 1.e-2;

constant.kb = 1.3806E-23;                  
constant.hbar = 1.054E-34;      
constant.i = sqrt(-1);
constant.c = 29979245800.00019;      %cm/s
constant.s2ps = 1E-12;
constant.ang2m = 1E-10;
constant.eV2J = 1.60217646E-19;

%--------------------------------------------------------------------------
iseed = 1;
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
LD.alloy_conc = 0.0;
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
LD.eps = 1.0;
%--------------------------------------------------------------------------

%MASSES
%--------------------------------------------------------------------------
LD.mass(1) = 1.0; LD.mass(2) = 3.0;
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
[tmp,str.main] = system('pwd'); str.main_write = str.main;

% str.main = strcat('/home/jason/lammps/LJ/alloy/10K/0.5/8x/NMD_AF/');
%     dummy = dlmread(strcat(str.main,'x0_0.5_1.data'));

str.main = strcat('/home/jason/lammps/LJ/amorphous/12x/prepare/');
    dummy = dlmread(strcat(str.main,'LJ_amor_1.pos'));

%--------------------------------------------------------------------------
    LD.param = dummy(1,:);
    LD.id = dummy(2:size(dummy,1),1);
    LD.m = dummy(2:size(dummy,1),2);
%replace double alloy
    I1 = find(LD.m ==1); I2 = find(LD.m ==2);
    LD.m(I1) = LD.mass(1); LD.m(I2) = LD.mass(2);

    LD.x = dummy(2:size(dummy,1),3);
    LD.y = dummy(2:size(dummy,1),4);
    LD.z = dummy(2:size(dummy,1),5);
%change param(1) so NUM_ATOMS==NUM_ATOMS_UCELL
    LD.NUM_ATOMS_UCELL = LD.param(1);
    LD.NUM_MODES = LD.param(1)*3;
    
    LD.Lx = LD.param(3); LD.Ly = LD.param(4); LD.Lz = LD.param(5);
    LD.VOLUME = LD.Lx*LD.Ly*LD.Lz;
    
    AF.param = LD.param; AF.id = LD.id; AF.m = LD.m;
    AF.NUM_ATOMS_UCELL = LD.NUM_ATOMS_UCELL; AF.NUM_MODES = LD.NUM_MODES;
    AF.Lx = LD.Lx; AF.Ly = LD.Ly; AF.Lz = LD.Lz; AF.VOLUME = LD.VOLUME;
    
    AF.x = LD.x; AF.y = LD.y; AF.z = LD.z;
%--------------------------------------------------------------------------


%--------------------------------------------------------------------------
AF.eps = 1.0;

AF.num_dk = 20;

AF.dk = 0.1; 

AF.DK(:,1) = (0:(AF.dk/AF.num_dk):AF.dk)';
AF.DK(:,2) = 0;
AF.DK(:,3) = 0;


%--------------------------------------------------------------------------


for idk = 1:size(AF.DK,1)

    idk
    
%--------------------------------------------------------------------------
%BEGIN: DYNAM
%--------------------------------------------------------------------------
AF.timing.dynam.tstart = tic;
%--------------------------------------------------------------------------
%i,j pairs and find rij
    LD.tempx = repmat(LD.x',size(LD.x',2),1);
    LD.tempy = repmat(LD.y',size(LD.y',2),1);
    LD.tempz = repmat(LD.z',size(LD.z',2),1);
%--------------------------------------------------------------------------
    LD.rijx = bsxfun(@minus,LD.tempx,LD.x);
    LD.rijy = bsxfun(@minus,LD.tempy,LD.y);
    LD.rijz = bsxfun(@minus,LD.tempz,LD.z);
%--------------------------------------------------------------------------
LD=rmfield(LD,'tempx'); LD=rmfield(LD,'tempy'); LD=rmfield(LD,'tempz');
%--------------------------------------------------------------------------
%APPLY BC  
%-------------------------------------------------------------------------- 
    [I] = find( LD.rijx < -(LD.Lx/2) ); LD.rijx(I) = LD.rijx(I) + LD.Lx;
    clear I 
    [I] = find( LD.rijy < -(LD.Ly/2) ); LD.rijy(I) = LD.rijy(I) + LD.Ly;
    clear I 
    [I] = find( LD.rijz < -(LD.Lz/2) ); LD.rijz(I) = LD.rijz(I) + LD.Lz;
    clear I 
%--------------------------------------------------------------------------
%other side
%--------------------------------------------------------------------------    
    [I] = find( LD.rijx > (LD.Lx/2) ); LD.rijx(I) = LD.rijx(I) - LD.Lx;
    clear I 
    [I] = find( LD.rijy > (LD.Ly/2) ); LD.rijy(I) = LD.rijy(I) - LD.Ly;
    clear I 
    [I] = find( LD.rijz > (LD.Lz/2) ); LD.rijz(I) = LD.rijz(I) - LD.Lz;
    clear I 
%--------------------------------------------------------------------------
%PHI
%--------------------------------------------------------------------------    
%find the sqaure displacements
    LD.r2 = LD.rijx.^2 + LD.rijy.^2 + LD.rijz.^2;
%apply cut-off
    [I] = find(LD.a2<LD.r2);    LD.r2(I) = 0.0;
%calculate the phi1 and phi2    
    LD.phi1 =...
        LD.eps * (-48 * (1./(LD.r2.^7)) + 24 * (1./(LD.r2.^4)) ) ;
    LD.phi2 =...
        LD.eps * ( (4*156) * (1./(LD.r2.^7)) - (42*4) * (1./(LD.r2.^4)) ) ;
%--------------------------------------------------------------------------
%DYNAM
%--------------------------------------------------------------------------     
%Calculate the 9 parts of phi to dynam       
LD.Dynam = zeros(LD.NUM_MODES,LD.NUM_MODES);
%1
    LD.Phixx = (LD.rijx.*LD.rijx./LD.r2).*(LD.phi2 - LD.phi1) + LD.phi1;
    Inan = isnan(LD.Phixx); I = Inan==1; LD.Phixx(I) = 0.0;
    LD.Dynam(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) =...
        -LD.Phixx.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phixx');
%2
    LD.Phixy = (LD.rijx.*LD.rijy./LD.r2).*(LD.phi2 - LD.phi1);
    Inan = isnan(LD.Phixy); I = Inan==1; LD.Phixy(I) = 0.0;
    LD.Dynam(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) =...
        -LD.Phixy.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phixy');
%3
    LD.Phixz = (LD.rijx.*LD.rijz./LD.r2).*(LD.phi2 - LD.phi1);
    Inan = isnan(LD.Phixz); I = Inan==1; LD.Phixz(I) = 0.0; 
    LD.Dynam(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) =...
        -LD.Phixz.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phixz');
%4
    LD.Phiyx = (LD.rijy.*LD.rijx./LD.r2).*(LD.phi2 - LD.phi1);
    Inan = isnan(LD.Phiyx); I = Inan==1; LD.Phiyx(I) = 0.0;
    LD.Dynam(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) =...
        -LD.Phiyx.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phiyx'); 
%5
    LD.Phiyy = (LD.rijy.*LD.rijy./LD.r2).*(LD.phi2 - LD.phi1) + LD.phi1;
    Inan = isnan(LD.Phiyy); I = Inan==1; LD.Phiyy(I) = 0.0;
    LD.Dynam(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) =...
        -LD.Phiyy.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phiyy'); 
%6
    LD.Phiyz = (LD.rijy.*LD.rijz./LD.r2).*(LD.phi2 - LD.phi1);
    Inan = isnan(LD.Phiyz); I = Inan==1; LD.Phiyz(I) = 0.0;
    LD.Dynam(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) =...
        -LD.Phiyz.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phiyz');
%7      
    LD.Phizx = (LD.rijz.*LD.rijx./LD.r2).*(LD.phi2 - LD.phi1);
    Inan = isnan(LD.Phizx); I = Inan==1; LD.Phizx(I) = 0.0;
    LD.Dynam(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) =...
        -LD.Phizx.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phizx'); 
%8
    LD.Phizy = (LD.rijz.*LD.rijy./LD.r2).*(LD.phi2 - LD.phi1);
    Inan = isnan(LD.Phizy); I = Inan==1; LD.Phizy(I) = 0.0;
    LD.Dynam(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) =...
        -LD.Phizy.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phizy'); 
%9
    LD.Phizz = (LD.rijz.*LD.rijz./LD.r2).*(LD.phi2 - LD.phi1) + LD.phi1;
    Inan = isnan(LD.Phizz); I = Inan==1; LD.Phizz(I) = 0.0;  
    LD.Dynam(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) =...
        -LD.Phizz.* 1./ sqrt(LD.m*LD.m');
    LD=rmfield(LD,'Phizz');
%--------------------------------------------------------------------------
    LD=rmfield(LD,'phi1');LD=rmfield(LD,'phi2');LD=rmfield(LD,'r2');
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%SELF-TERM
%--------------------------------------------------------------------------  

%--------------------------------------------------------------------------
LD.Phisum = zeros(LD.NUM_MODES,LD.NUM_MODES);
%--------------------------------------------------------------------------
%sum h horizontal rows    
    LD.Dynam_sumhx = sum(-1*LD.Dynam(:,1:3:LD.NUM_MODES),2);
    LD.Dynam_sumhy = sum(-1*LD.Dynam(:,2:3:LD.NUM_MODES),2);
    LD.Dynam_sumhz = sum(-1*LD.Dynam(:,3:3:LD.NUM_MODES),2);
%horizontal
    LD.Phisum(:,1:3:LD.NUM_MODES) = LD.Phisum(:,1:3:LD.NUM_MODES) +...
        repmat(LD.Dynam_sumhx,1,LD.NUM_MODES/3);
    LD=rmfield(LD,'Dynam_sumhx');
    LD.Phisum(:,2:3:LD.NUM_MODES) = LD.Phisum(:,2:3:LD.NUM_MODES) +...
        repmat(LD.Dynam_sumhy,1,LD.NUM_MODES/3);
    LD=rmfield(LD,'Dynam_sumhy');
    LD.Phisum(:,3:3:LD.NUM_MODES) = LD.Phisum(:,3:3:LD.NUM_MODES) +...
        repmat(LD.Dynam_sumhz,1,LD.NUM_MODES/3);
    LD=rmfield(LD,'Dynam_sumhz');
%make a mask for the on diagonal 3x3s
    A(1:3,1:3) = 1;
    MASK = kron(eye(LD.NUM_MODES/3),A);
    LD.Dynam = LD.Dynam + LD.Phisum.*MASK; 
    
LD=rmfield(LD,'Phisum');    
%--------------------------------------------------------------------------
%find eigenvalues and eigenvectors
%--------------------------------------------------------------------------


LD.RIJX = zeros(LD.NUM_MODES,LD.NUM_MODES);
LD.RIJY = zeros(LD.NUM_MODES,LD.NUM_MODES);
LD.RIJZ = zeros(LD.NUM_MODES,LD.NUM_MODES);
%X
LD.RIJX(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

LD.RIJX(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

LD.RIJX(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

%Y
LD.RIJY(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijy;

LD.RIJY(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijy;

LD.RIJY(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijy;

%Z
LD.RIJZ(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijz;

LD.RIJZ(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijz;

LD.RIJZ(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijz;


LD.Dynam =...
    bsxfun(...
    @times,...
    LD.Dynam,...
    exp(...
    2*pi*1i*(...
    AF.DK(idk,1)*LD.RIJX/LD.Lx +...
    AF.DK(idk,2)*LD.RIJY/LD.Lx +...
    AF.DK(idk,3)*LD.RIJZ/LD.Lx      ) )...
    );

LD.Dynam = (1/2)*(LD.Dynam + LD.Dynam' );


[LD.eigvec LD.eigval] = eig(LD.Dynam);
LD.freq = sqrt(diag(LD.eigval));

LD=rmfield(LD,'eigval');

%--------------------------------------------------------------------------
AF.timing.dynam.telapsed = toc(AF.timing.dynam.tstart);
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%END: DYNAM
%--------------------------------------------------------------------------


    AF.freq(:,idk) = LD.freq;
    AF.constant = constant;
    AF.LJ = LJ;
    


% str.write =...
%     strcat(str.main,'AF_eigvec_',int2str(iseed),'_',int2str(idk),'.dat');
% dlmwrite(str.write,LD.eigvec,'delimiter',' ');

%--------------------------------------------------------------------------
%toc
%--------------------------------------------------------------------------  

end

save(...
    strcat(str.main,'AFdk_',int2str(iseed),'.mat')...
    , '-struct', 'AF');
