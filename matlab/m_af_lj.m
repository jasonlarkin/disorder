function [freq Di kappa] = m_af_lj( x0 , eps , str_main , tf_eig )
%[freq Di kappa] = m_af_lj( x0 , eps , str_main , tf_eig )

%--------------------------------------------------------------------------
%AF settings
%--------------------------------------------------------------------------
%potential cut off
LD.a2 = 2.5^2;
%Lorentzian cutoff
LD.deltaL = 1.e-2;
%--------------------------------------------------------------------------
%potential
%--------------------------------------------------------------------------
LD.eps = eps;
%--------------------------------------------------------------------------
%input
%--------------------------------------------------------------------------
    LD.id = x0.id; LD.m = x0.m;
%input structure positions
    LD.x = x0.x; LD.y = x0.y; LD.z = x0.z;
%change param(1) so NUM_ATOMS==NUM_ATOMS_UCELL
    LD.NUM_ATOMS_UCELL = size(x0.x,1);
    LD.NUM_MODES = 3*size(x0.x,1);
%box size
    LD.Lx = x0.Lx;  LD.Ly = x0.Ly; LD.Lz = x0.Lz; 
    LD.VOLUME = LD.Lx*LD.Ly*LD.Lz;

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
    LD=rmfield(LD,'phi1');LD=rmfield(LD,'phi2');
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
LD.Dynam = (1/2)*(LD.Dynam' + LD.Dynam);
[LD.eigvec LD.eigval] = eig(LD.Dynam);
LD.freq = sqrt(diag(LD.eigval));
%--------------------------------------------------------------------------
%IMPORTANT
%--------------------------------------------------------------------------
%get rid of small imag for first 3 acoustic modes
LD.freq(1:3) = 0;

LD=rmfield(LD,'eigval');

%--------------------------------------------------------------------------
AF.timing.dynam.telapsed = toc(AF.timing.dynam.tstart);
AF.timing.dynam.telapsed
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%END: DYNAM
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%BEGIN: AF
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
AF.timing.af.tstart = tic;
%--------------------------------------------------------------------------

%Lorentzian
LD.dw_avg = real(mean(LD.freq(2:LD.NUM_MODES)-LD.freq(1:LD.NUM_MODES-1)));
LD.delwij = ...
    repmat(LD.freq,1,LD.NUM_MODES) - repmat(LD.freq',LD.NUM_MODES,1 ) ;
LD.lor = (1.0/pi)*(LD.dw_avg./( LD.delwij.^2 + LD.dw_avg^2 ) );
%apply lor cutoff
I = find(LD.deltaL>LD.lor); LD.lor(I) = 0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'delwij');
%--------------------------------------------------------------------------
%X
LD.RIJX = zeros(LD.NUM_MODES,LD.NUM_MODES);

LD.RIJX(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

LD.RIJX(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

LD.RIJX(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;
%--------------------------------------------------------------------------
LD.vij_x =...
    ((1/2)./sqrt(LD.freq*LD.freq')).*...
    (LD.eigvec'*bsxfun(@times,LD.Dynam,LD.RIJX)*LD.eigvec);
Inan = isnan(LD.vij_x); I = Inan==1; LD.vij_x(I) = 0.0;
Iinf = isinf(LD.vij_x); I = Iinf==1; LD.vij_x(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'RIJX'); LD=rmfield(LD,'rijx');
%--------------------------------------------------------------------------
LD.Sij_x =...
    1/(2)*LD.vij_x.*...
    (repmat(LD.freq',LD.NUM_MODES,1) + repmat(LD.freq,1,LD.NUM_MODES));
%set diagonal to 0
LD.Sij_x(1:LD.NUM_MODES+1:LD.NUM_MODES*LD.NUM_MODES) = 0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'vij_x');
%--------------------------------------------------------------------------
%apply lor width
LD.Sij_x = LD.lor.*( abs(LD.Sij_x).^2 );
%add its part to Di
LD.Di = (pi./(3*LD.freq.^2) ).* ( sum(LD.Sij_x,2) );
Inan = isnan(LD.Di); I = Inan==1; LD.Di(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'Sij_x');
%--------------------------------------------------------------------------
%Y
LD.RIJY = zeros(LD.NUM_MODES,LD.NUM_MODES);

LD.RIJY(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijy;

LD.RIJY(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijy;

LD.RIJY(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijy;
LD.RIJY(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijy;

LD.vij_y =...
    ((1/2)./sqrt(LD.freq*LD.freq')).*...
    (LD.eigvec'*bsxfun(@times,LD.Dynam,LD.RIJY)*LD.eigvec);
Inan = isnan(LD.vij_y); I = Inan==1; LD.vij_y(I) = 0.0;
Iinf = isinf(LD.vij_y); I = Iinf==1; LD.vij_y(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'RIJY'); LD=rmfield(LD,'rijy'); 
%--------------------------------------------------------------------------
LD.Sij_y =...
    1/(2)*LD.vij_y.*...
    (repmat(LD.freq',LD.NUM_MODES,1) + repmat(LD.freq,1,LD.NUM_MODES));
%set diagonal to 0
LD.Sij_y(1:LD.NUM_MODES+1:LD.NUM_MODES*LD.NUM_MODES) = 0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'vij_y');
%--------------------------------------------------------------------------
%apply lor width
LD.Sij_y = LD.lor.*( abs(LD.Sij_y).^2 );
%add its part to Di
LD.Di = LD.Di + (pi./(3*LD.freq.^2) ).* ( sum(LD.Sij_y,2) );
Inan = isnan(LD.Di); I = Inan==1; LD.Di(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'Sij_y');
%--------------------------------------------------------------------------
%Z
LD.RIJZ = zeros(LD.NUM_MODES,LD.NUM_MODES);

LD.RIJZ(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijz;

LD.RIJZ(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijz;

LD.RIJZ(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijz;
LD.RIJZ(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijz;
%--------------------------------------------------------------------------
LD.vij_z =...
    ((1/2)./sqrt(LD.freq*LD.freq')).*...
    (LD.eigvec'*bsxfun(@times,LD.Dynam,LD.RIJZ)*LD.eigvec);
Inan = isnan(LD.vij_z); I = Inan==1; LD.vij_z(I) = 0.0;
Iinf = isinf(LD.vij_z); I = Iinf==1; LD.vij_z(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'RIJZ'); LD=rmfield(LD,'rijz'); 
%--------------------------------------------------------------------------
LD.Sij_z =...
    1/(2)*LD.vij_z.*...
    (repmat(LD.freq',LD.NUM_MODES,1) + repmat(LD.freq,1,LD.NUM_MODES));
%set diagonal to 0
LD.Sij_z(1:LD.NUM_MODES+1:LD.NUM_MODES*LD.NUM_MODES) = 0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'vij_z');
%--------------------------------------------------------------------------
%apply lor width
LD.Sij_z = LD.lor.*( abs(LD.Sij_z).^2 );
%add its part to Di
LD.Di = LD.Di + (pi./(3*LD.freq.^2) ).* ( sum(LD.Sij_z,2) );
Inan = isnan(LD.Di); I = Inan==1; LD.Di(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'Sij_z');
%--------------------------------------------------------------------------
LD=rmfield(LD,'lor');
%--------------------------------------------------------------------------
LD.kappa =...
    sum(LD.Di)*...
    ((x0.lj.sigma^2)/x0.lj.tau)*x0.constant.kb/(LD.VOLUME*x0.lj.sigma^3);
%--------------------------------------------------------------------------
freq = LD.freq; Di = LD.Di; kappa = LD.kappa;
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
AF.freq = LD.freq;
AF.constant = x0.constant;
AF.Di = LD.Di;
AF.kappa = LD.kappa;
AF.x0 = x0;
%--------------------------------------------------------------------------

%output
output = [LD.freq LD.Di];
str.write = strcat(str_main,'AF_Di(wi).dat');
dlmwrite(str.write,output,'delimiter',' ');

str.write = strcat(str_main,'AF_freq.dat');
dlmwrite(str.write,LD.freq,'delimiter',' ');

output = [LD.kappa];
str.write = strcat(str_main,'AF_kappa.dat');
dlmwrite(str.write,output,'-append','delimiter',' ');

%save AF
save( strcat(str_main,'/AF.mat') , '-struct', 'AF');

%possibly save eigvec
if strcmp(tf_eig,'true')
str.write =...
    strcat(str_main,'AF_eigvec.dat');
dlmwrite(str.write,LD.eigvec,'delimiter',' ');
end
%--------------------------------------------------------------------------
AF.timing.af.telapsed = toc(AF.timing.af.tstart);
AF.timing.af.telapsed
%--------------------------------------------------------------------------

end
