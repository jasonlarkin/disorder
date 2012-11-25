clear

%--------------------------------------------------------------------------
%     [tmp,str.main]=system('pwd');
str.main='/home/jason/disorder2/lj/amor/4x/prepare/tmp/';
%--------------------------------------------------------------------------

%--------------LJ----------------------------------------------------------
LJ.eps = 1.67E-21;              
LJ.sigma = 3.4E-10;                
LJ.mass = 6.6326E-26;               
LJ.tau = sqrt((LJ.mass*(LJ.sigma^2))/LJ.eps);
%--------------------------------------------------------------------------

GK.dt_lj = 0.002; 
GK.sample_rate = 5;
GK.dt = GK.dt_lj*LJ.tau;
%--------------------------------------------------------------------------
GK.SEEDS=[1,2,3,4,5];
%--------------------------------------------------------------------------
GK.NUM_SEEDS=size(GK.SEEDS,2);
%--------------------------------------------------------------------------
GK.Tset = [2.5 5 10 15];
GK.NUM_TEMPS=size(GK.Tset,2);

GK.p = 5; GK.s = 1000; GK.d = GK.p*GK.s;
GK.total_steps = 2000000;




%--------------constants---------------------------------------------------
con.kb = 1.3806E-23;                  
con.hbar = 1.054E-34;      
con.i = sqrt(-1);
con.c = 29979245800.00019;      %cm/s
con.s2ps = 1E-12;
con.ang2m = 1E-10;
con.eV2J = 1.60217646E-19;
%--------------GK----------------------------------------------------------
GK.scaleJ = (LJ.eps)/((LJ.sigma^2)*LJ.tau);
%this includes the factor of 1/V


%--------------------------------------------------------------------------
    [tmp,str.main]=system('pwd');
%--------------------------------------------------------------------------


%Average over seeds

GK.JJ(1:GK.s,1:2,1:GK.NUM_TEMPS)=0;
GK.JJ(:,1,1) = (0:(size(GK.JJ,1)-1)')*GK.dt*GK.sample_rate;
GK.VOLUME(1:GK.NUM_TEMPS) = 0;

for itemp=1:GK.NUM_TEMPS
    for iseed = 1:GK.NUM_SEEDS
%grep the JJ dump
        str.cmd = ['grep -A ' int2str(GK.s) ' "'...
            int2str(GK.total_steps) ' '...
            int2str(GK.s) '" J0Jt_' int2str(GK.SEEDS(iseed)) '_' ...
            int2str(itemp) '.dat > J0Jt_'  int2str(GK.SEEDS(iseed)) '_' ...
            int2str(itemp) '_grep.dat'];
        system(str.cmd);
%average the grep JJ                 
        str.read = ['J0Jt_' int2str(GK.SEEDS(iseed)) '_' int2str(itemp)...
            '_grep.dat'];
        dummy=dlmread(str.read);
      
       GK.JJ(:,2,itemp) = GK.JJ(:,2,itemp) +...
           ((dummy(2:length(dummy),4)+dummy(2:length(dummy),5)...
           +dummy(2:length(dummy),6))/3);
%average volume using the Lx dump       
       str.read = ['Lx_' int2str(GK.SEEDS(iseed)) '_' int2str(itemp)...
            '.profile'];
       [dummy]=importdata(str.read);
       GK.VOLUME(itemp) = GK.VOLUME(itemp)...
       + dummy.data(size(dummy.data,1),2)^3;
    end
    GK.JJ(:,2,itemp) = GK.JJ(:,2,itemp)/GK.NUM_SEEDS;
    GK.VOLUME(itemp) = (GK.VOLUME(itemp)/GK.NUM_SEEDS);
%this is needed if you don't divide by vol in lammps    
    GK.JJ(:,2,itemp) = GK.JJ(:,2,itemp)/(GK.VOLUME(itemp)^2);
    
end


%convert to real units
GK.VOLUME =GK.VOLUME*LJ.sigma^3;
GK.JJ(:,2,:) = GK.JJ(:,2,:)*GK.scaleJ^2;

plot(GK.JJ(:,1,1),GK.JJ(:,2,1))
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------        
                    

GK.intJJ(1:size(GK.JJ(:,1)),1:GK.NUM_TEMPS) = 0;    

for itemp=1:GK.NUM_TEMPS

            
    GK.intJJ(:,itemp) = cumtrapz(GK.JJ(:,1,1),GK.JJ(:,2,itemp))*...
        (GK.VOLUME(itemp)/(con.kb*(GK.Tset(itemp)^2)));

    plot(GK.JJ(:,1)/con.s2ps,GK.intJJ(:,itemp))
    xlabel('t (ps)','FontSize',24); 
    ylabel('\kappa (W/m-K)' ,'FontSize',24);
            
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------   

plot(GK.intJJ(:,itemp))

left = input('left ');
right = input('right ');

GK.kappa(itemp,1) = mean(GK.intJJ(left:right,itemp));
GK.kappa(itemp,2) = std(GK.intJJ(left:right,itemp));

%output JJavg
str.write = strcat(str.main,'/JJavg_',int2str(itemp),'.dat');
output = [GK.JJ(:,1,1) GK.JJ(:,2,itemp)];
dlmwrite(str.write,output,'delimiter',' ');

%output kappa


end

%output kappa

str.write = strcat(str.main,'/kappa(T).dat');
output = [GK.kappa];
dlmwrite(str.write,output,'delimiter',' ');


