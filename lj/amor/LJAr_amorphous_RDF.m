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
[tmp,str.main] = system('pwd'); str.main_write = str.main;
str.main = strcat(str.main,'/8x/RDF/');

%--------------------------------------------------------------------------
iseed = 1; LD.SEED = [1 2 4 5]; LD.NUM_SEEDS = size(LD.SEED,2);
%--------------------------------------------------------------------------

%MASSES
%--------------------------------------------------------------------------
LD.mass(1) = 1.0; LD.mass(2) = 1.5;
%--------------------------------------------------------------------------

LD.NUM_BIN = 200;

%--------------------------------------------------------------------------
tic
%--------------------------------------------------------------------------

%read data
    dummy = dlmread(strcat(str.main,'LJ_amor_',int2str(LD.SEED(iseed)),'.pos'));
    LD.param = dummy(1,:);
    LD.id = dummy(2:size(dummy,1),1);
    LD.m = LD.mass(dummy(2:size(dummy,1),2))';
    LD.x = dummy(2:size(dummy,1),3);
    LD.y = dummy(2:size(dummy,1),4);
    LD.z = dummy(2:size(dummy,1),5);
    
    LD.NUM_ATOMS = LD.param(1);
    LD.NUM_ATOMS_UCELL = LD.param(2);
    LD.NUM_MODES = LD.param(2)*3;
    
    LD.Lx = LD.param(3); LD.Ly = LD.param(4); LD.Lz = LD.param(5);
    LD.VOLUME = LD.Lx*LD.Ly*LD.Lz;

    LD.rmin = 0.5; LD.rmax = LD.Lx/2; 
    LD.dr = (LD.rmax - LD.rmin)/LD.NUM_BIN;

LD.rdf_avg(1:LD.NUM_BIN) = 0;

for iseed = 1:LD.NUM_SEEDS

%read data
    dummy = dlmread(strcat(str.main,'LJ_amor_',int2str(LD.SEED(iseed)),'.pos'));
    LD.param = dummy(1,:);
    LD.id = dummy(2:size(dummy,1),1);
    LD.m = LD.mass(dummy(2:size(dummy,1),2))';
    LD.x = dummy(2:size(dummy,1),3);
    LD.y = dummy(2:size(dummy,1),4);
    LD.z = dummy(2:size(dummy,1),5);
    
    LD.NUM_ATOMS = LD.param(1);
    LD.NUM_ATOMS_UCELL = LD.param(2);
    LD.NUM_MODES = LD.param(2)*3;
    
    LD.Lx = LD.param(3); LD.Ly = LD.param(4); LD.Lz = LD.param(5);
    LD.VOLUME = LD.Lx*LD.Ly*LD.Lz;
%--------------------------------------------------------------------------
%pause
%-------------------------------------------------------------------------- 
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
%APPLY BC    
    [I] = find( LD.rijx < -(LD.Lx/2) ); LD.rijx(I) = LD.rijx(I) + LD.Lx;
    clear I 
    [I] = find( LD.rijy < -(LD.Ly/2) ); LD.rijy(I) = LD.rijy(I) + LD.Ly;
    clear I 
    [I] = find( LD.rijz < -(LD.Lz/2) ); LD.rijz(I) = LD.rijz(I) + LD.Lz;
    clear I 
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------    
%other side
    [I] = find( LD.rijx > (LD.Lx/2) ); LD.rijx(I) = LD.rijx(I) - LD.Lx;
    clear I 
    [I] = find( LD.rijy > (LD.Ly/2) ); LD.rijy(I) = LD.rijy(I) - LD.Ly;
    clear I 
    [I] = find( LD.rijz > (LD.Lz/2) ); LD.rijz(I) = LD.rijz(I) - LD.Lz;
    clear I 
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------    
%find the sqaure displacements
    LD.r2 = LD.rijx.^2 + LD.rijy.^2 + LD.rijz.^2;
    
    LD.r = sqrt(LD.r2);
   
    
for ibin = 1:LD.NUM_BIN
    I = find( (LD.rmin + LD.dr*(ibin-1)) < LD.r &...
        LD.r < (LD.rmin + LD.dr*ibin) );
    LD.rdf(ibin) = size(I,1) / LD.NUM_ATOMS /...
        (LD.NUM_ATOMS/LD.VOLUME*4*pi*LD.dr*(LD.rmin + LD.dr*(ibin-1))^2) ;  
end

%plot(LD.rmin:LD.dr:(LD.rmax-LD.dr),LD.rdf)

LD.rdf_avg = LD.rdf_avg + LD.rdf;

end

LD.rdf_avg= LD.rdf_avg/LD.NUM_SEEDS;

plot(LD.rmin:LD.dr:(LD.rmax-LD.dr),LD.rdf_avg)

output = [(LD.rmin:LD.dr:(LD.rmax-LD.dr))' LD.rdf_avg'];

dlmwrite(strcat(str.main,'RDF_avg.dat'),output,'delimiter', ' ');

    