clear
%--------------------------------------------------------------------------
str.main='/home/jason/disorder2/si/amor/normand/perfect2/anneal_900K/gk/';
%str.main='/home/jason/disorder2/si/amor/prepare/8x/gk/';
%--------------------------------------------------------------------------
GK.Nx = 5;
GK.Lx = GK.Nx*5.43; GK.Ly = GK.Nx*5.43; GK.Lz = GK.Nx*5.43;
% GK.Nx = 2.0;
% GK.Lx = GK.Nx*21.2; GK.Ly = GK.Nx*21.2; GK.Lz = GK.Nx*21.2;

GK.VOLUME = GK.Lx*GK.Lx*GK.Lx;
%--------------------------------------------------------------------------
GK.SEEDS=[1:6];
%42x
%GK.SEEDS=[1 2 3:8];
%--------------------------------------------------------------------------
GK.NUM_SEEDS=size(GK.SEEDS,2);
%--------------------------------------------------------------------------
GK.Tset = [300];
GK.NUM_TEMPS=size(GK.Tset,2);

GK.p = 250000; GK.s = 10; GK.d = GK.p*GK.s;
GK.total_steps = 2500000;
GK.dt = 0.0005; 

% GK.p = 250000; GK.s = 10; GK.d = GK.p*GK.s;
% GK.total_steps = 2500000;
% GK.dt = 0.0005; 

%--------------------------------------------------------------------------

%--------------constants---------------------------------------------------
con.kb = 1.3806E-23;                  
con.hbar = 1.054E-34;      
con.i = sqrt(-1);
con.c = 29979245800.00019;      %cm/s
con.s2ps = 1E-12;
con.ang2m = 1E-10;
con.eV2J = 1.60217646E-19;

%--------------GK----------------------------------------------------------
%Average over seeds
GK.JJ(1:GK.p,1:2)=0;
GK.JJ(:,1) = (0:(size(GK.JJ,1)-1)')*GK.dt*GK.s;

for iseed = 1:GK.NUM_SEEDS
%grep the JJ dump
    str.cmd =...
        ['grep -A ' int2str(GK.p) ' "'...
        int2str(GK.total_steps) ' '...
        int2str(GK.p) '" ' ...
        str.main 'J0Jt_' int2str(GK.SEEDS(iseed)) '.dat > ' ...
        str.main 'J0Jt_'  int2str(GK.SEEDS(iseed)) 'grep.dat'];
    system(str.cmd);
%average the grep JJ                 
    str.read = [str.main 'J0Jt_' int2str(GK.SEEDS(iseed)) ...
        'grep.dat'];
    dummy=dlmread(str.read);

   GK.JJ(:,2) = GK.JJ(:,2) +...
       ((dummy(2:length(dummy),4)+dummy(2:length(dummy),5)...
       +dummy(2:length(dummy),6))/3);
%system volume
end
    GK.JJ(:,2) = GK.JJ(:,2)/GK.NUM_SEEDS;
%this is needed if you don't divide by vol in lammps    
GK.JJ(:,2) = GK.JJ(:,2)/(GK.VOLUME^2);

plot(GK.JJ(:,1),GK.JJ(:,2))

%--------------------------------------------------------------------------
%pause
%-------------------------------------------------------------------------- 

%--------------scale-------------------------------------------------------
GK.scaleJ = (con.eV2J)/((con.ang2m^2)*con.s2ps);
GK.VOLUME = GK.VOLUME*con.ang2m^3;
%convert to real units
GK.JJ(:,2) = GK.JJ(:,2)*(GK.scaleJ^2) ;
GK.JJ(:,1) = GK.JJ(:,1)*con.s2ps;

plot(GK.JJ(:,1),GK.JJ(:,2))
%--------------------------------------------------------------------------
%pause
%-------------------------------------------------------------------------- 
                    
GK.intJJ( 1:size(GK.JJ(:,1)) ) = 0;    

for itemp=1:GK.NUM_TEMPS

    GK.intJJ(:) = cumtrapz(GK.JJ(:,1),GK.JJ(:,2))*...
        (GK.VOLUME/(con.kb*(GK.Tset^2)));

    plot(GK.JJ(:,1)/con.s2ps,GK.intJJ(:))
    xlabel('t (ps)','FontSize',24); 
    ylabel('\kappa (W/m-K)' ,'FontSize',24);
            
plot(GK.intJJ(:))

left = input('left ');
right = input('right ');

GK.kappa(1) = mean(GK.intJJ(left:right));
GK.kappa(2) = std(GK.intJJ(left:right));

%output JJavg
str.write = strcat(str.main,'JJavg.dat');
output = [GK.JJ(:,1) GK.JJ(:,2)];
dlmwrite(str.write,output,'delimiter',' ');

end

%output kappa

str.write = strcat(str.main,'kappa(T).dat');
output = [GK.kappa];
dlmwrite(str.write,output,'delimiter',' ');


