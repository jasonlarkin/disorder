
%--------------------------------------------------------------------------
str.main = '/home/jason/lammps/Si/amor/prepare/4x/dynam/';
%--------------------------------------------------------------------------

dummy = dlmread(strcat(str.main,'LJ_amor_1.pos'));

%--------------------------------------------------------------------------
%AF settings
%--------------------------------------------------------------------------
%Lorentzian cutoff
LD.deltaL = 0;
%---------------------------------Si-----------------------------------------
constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
constant.hbar = 1.054E-34;                %J*s
%constant.hbar2 = 6.58211928E-4;                %eV*ps
constant.i = sqrt(-1);
constant.c = 29979245800.00019;      %cm/s
constant.s2ps = 1E-12;
constant.ang2m = 1E-10;
constant.eV2J = 1.60217646E-19;

    
    LD.freq = (dlmread([str.main 'freq.dat']))'; % * constant.s2ps;
    
    LD.eigvec = dlmread([str.main 'eigvec.dat']);
    
LD.Dynam = dlmread([str.main 'phi.dat']) * (1E4/1.0363) / constant.s2ps^2 ;
    
%--------------------------------------------------------------------------
%input
%--------------------------------------------------------------------------

    LD.param = dummy(1,:);
    LD.id = dummy(2:size(dummy,1),1);
    LD.m = dummy(2:size(dummy,1),2);
    
    LD.x = dummy(2:size(dummy,1),3) * constant.ang2m;
    LD.y = dummy(2:size(dummy,1),4) * constant.ang2m;
    LD.z = dummy(2:size(dummy,1),5) * constant.ang2m;

%change param(1) so NUM_ATOMS==NUM_ATOMS_UCELL
    LD.NUM_ATOMS_UCELL = LD.param(1);
    LD.NUM_MODES = LD.param(1)*3;
    
    LD.Lx = LD.param(3); LD.Ly = LD.param(4); LD.Lz = LD.param(5);
    LD.VOLUME = LD.Lx*LD.Ly*LD.Lz*(constant.ang2m^3);
    

%CREATE i,j pairs and find rij
    LD.tempx = repmat(LD.x',size(LD.x',2),1);
    LD.tempy = repmat(LD.y',size(LD.y',2),1);
    LD.tempz = repmat(LD.z',size(LD.z',2),1);

    LD.rijx = bsxfun(@minus,LD.tempx,LD.x);
    LD.rijy = bsxfun(@minus,LD.tempy,LD.y);
    LD.rijz = bsxfun(@minus,LD.tempz,LD.z);
    
LD=rmfield(LD,'tempx'); LD=rmfield(LD,'tempy'); LD=rmfield(LD,'tempz');
    
%--------------------------------------------------------------------------
%pause
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
    ((1i/2)./sqrt(LD.freq*LD.freq')).*...
    (LD.eigvec'*bsxfun(@times,LD.Dynam,LD.RIJX)*LD.eigvec);
Inan = isnan(LD.vij_x); I = Inan==1; LD.vij_x(I) = 0.0;
Iinf = isinf(LD.vij_x); I = Iinf==1; LD.vij_x(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'RIJX'); LD=rmfield(LD,'rijx');
%--------------------------------------------------------------------------
LD.Sij_x =...
    constant.hbar/(2*LD.VOLUME)*LD.vij_x.*...
    (repmat(LD.freq',LD.NUM_MODES,1) + repmat(LD.freq,1,LD.NUM_MODES));
%set diagonal to 0
LD.Sij_x(1:LD.NUM_MODES+1:LD.NUM_MODES*LD.NUM_MODES) = 0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'vij_x');
%--------------------------------------------------------------------------
%apply lor width
LD.Sij_x = LD.lor.*( abs(LD.Sij_x).^2 );
%add its part to Di
LD.Di =...
    ( LD.VOLUME^2*pi./(3*constant.hbar^2*LD.freq.^2) ).*...
    ( sum(LD.Sij_x,2) );
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
    ((1i/2)./sqrt(LD.freq*LD.freq')).*...
    (LD.eigvec'*bsxfun(@times,LD.Dynam,LD.RIJY)*LD.eigvec);
Inan = isnan(LD.vij_y); I = Inan==1; LD.vij_y(I) = 0.0;
Iinf = isinf(LD.vij_y); I = Iinf==1; LD.vij_y(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'RIJY'); LD=rmfield(LD,'rijy'); 
%--------------------------------------------------------------------------
LD.Sij_y =...
    constant.hbar/(2*LD.VOLUME)*LD.vij_y.*...
    (repmat(LD.freq',LD.NUM_MODES,1) + repmat(LD.freq,1,LD.NUM_MODES));
%set diagonal to 0
LD.Sij_y(1:LD.NUM_MODES+1:LD.NUM_MODES*LD.NUM_MODES) = 0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'vij_y');
%--------------------------------------------------------------------------
%apply lor width
LD.Sij_y = LD.lor.*( abs(LD.Sij_y).^2 );
%add its part to Di
LD.Di =...
    LD.Di +...
    ( LD.VOLUME^2*pi./(3*constant.hbar^2*LD.freq.^2) ).*...
    ( sum(LD.Sij_y,2) );

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
    ((1i/2)./sqrt(LD.freq*LD.freq')).*...
    (LD.eigvec'*bsxfun(@times,LD.Dynam,LD.RIJZ)*LD.eigvec);
Inan = isnan(LD.vij_z); I = Inan==1; LD.vij_z(I) = 0.0;
Iinf = isinf(LD.vij_z); I = Iinf==1; LD.vij_z(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'RIJZ'); LD=rmfield(LD,'rijz'); 
%--------------------------------------------------------------------------
LD.Sij_z =...
    constant.hbar/(2*LD.VOLUME)*LD.vij_z.*...
    (repmat(LD.freq',LD.NUM_MODES,1) + repmat(LD.freq,1,LD.NUM_MODES));
%set diagonal to 0
LD.Sij_z(1:LD.NUM_MODES+1:LD.NUM_MODES*LD.NUM_MODES) = 0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'vij_z');
%--------------------------------------------------------------------------
%apply lor width
LD.Sij_z = LD.lor.*( abs(LD.Sij_z).^2 );
%add its part to Di
LD.Di =...
    LD.Di +...
    ( LD.VOLUME^2*pi./(3*constant.hbar^2*LD.freq.^2) ).*...
    ( sum(LD.Sij_z,2) );

Inan = isnan(LD.Di); I = Inan==1; LD.Di(I) = 0.0;
%--------------------------------------------------------------------------
LD=rmfield(LD,'Sij_z');
%--------------------------------------------------------------------------
LD=rmfield(LD,'lor');
%--------------------------------------------------------------------------

LD.kappa =...
    sum(LD.Di)*...
    constant.kb / ( LD.VOLUME );

% loglog(LD.freq,LD.Di)


%--------------------------------------------------------------------------
AF.freq = LD.freq;
AF.Di = LD.Di;
AF.kappa = LD.kappa;

%--------------------------------------------------------------------------
%save AF
save( strcat(str.main,'AF.mat') , '-struct', 'AF');




