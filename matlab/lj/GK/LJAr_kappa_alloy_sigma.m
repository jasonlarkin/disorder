%Define Boltzmann's constant
%kb = 8.6170e-005; %in eV/K
kb = 1.380e-23;  %in J/K
ps=10^-12;
%evToj=1.6022*10^(-19);
%psTos=10^(-12);
%angTom=10^(-10);
mass_Ar=6.63*10^(-26); eps_Ar=1.67*10^(-21); sigma_Ar=3.4*10^(-10); tau_Ar=2.1423*10^(-12);
%Calculate cut-off frequency for filtering
step_size = 0.002; sample_rate = 10;
dt = step_size*sample_rate*tau_Ar;
%sample freq: this should be 2 Thz
%f = 1/dt;
%calculate filter mask: use 0.2 THz as a guess for now
%fNorm = 0.1 / (f/2);
%[b,a] = butter(10, fNorm, 'low');

%Calcaulte scale factor for J
scaleJ = (eps_Ar/(tau_Ar*(sigma_Ar^2)));

%----------------------------------4_1.05----------------------------------
%0.1
%Tset = [0.0215*(eps_Ar/kb),0.064*(eps_Ar/kb),0.162*(eps_Ar/kb),0.58*(eps_Ar/kb)];
%0.5
%Tset = [0.021*(eps_Ar/kb),0.063*(eps_Ar/kb),0.1725*(eps_Ar/kb),0.57*(eps_Ar/kb)];
%0.9
%Tset = [0.0205*(eps_Ar/kb),0.0625*(eps_Ar/kb),0.1675*(eps_Ar/kb),0.625*(eps_Ar/kb)];
%0.1
%volset = [9.734339287e-27,9.808891425e-27,9.988426959e-27,1.094888832e-26];
%0.5
%volset = [1.060925263e-26,1.067627556e-26,1.084655477e-26,1.178447256e-26];
%0.9
%volset = [1.093501741e-26,1.099342112e-26,1.117567882e-26,1.230621569e-26];

%----------------------------------4_1.1-----------------------------------
%0.1
%Tset = [0.0215*(eps_Ar/kb),0.063*(eps_Ar/kb),0.165*(eps_Ar/kb),0.58*(eps_Ar/kb)];
%0.5
%Tset = [0.019*(eps_Ar/kb),0.0635*(eps_Ar/kb),0.168*(eps_Ar/kb),0.59*(eps_Ar/kb)];
%0.9
%Tset = [0.019*(eps_Ar/kb),0.0625*(eps_Ar/kb),0.165*(eps_Ar/kb),0.57*(eps_Ar/kb)];
%0.1
%volset = [1.024424012e-26,1.030474528e-26,1.050451933e-26,1.154698767e-26];
%0.5
%volset = [1.197071114e-26,1.204832749e-26,1.224747693e-26,1.353041326e-26];
%0.9
%volset = [1.256689002e-26,1.264911504e-26,1.284481564e-26,1.416114372e-26];

%----------------------------------4_1.2-----------------------------------
%0.1
%Tset = [0.0195*(eps_Ar/kb),0.065*(eps_Ar/kb),0.175*(eps_Ar/kb),0.575*(eps_Ar/kb)];
%0.5
%Tset = [0.0205*(eps_Ar/kb),0.062*(eps_Ar/kb),0.1725*(eps_Ar/kb),0.59*(eps_Ar/kb)];
%0.9
Tset = [0.0198*(eps_Ar/kb),0.0675*(eps_Ar/kb),0.1725*(eps_Ar/kb),0.59*(eps_Ar/kb)];
%0.1
%volset = [1.142615694e-26,1.150931752e-26,1.172907e-26,1.346731983e-26];
%0.5
%volset = [1.529993563e-26,1.540627983e-26,1.570855454e-26,1.86137134e-26];
%0.9
volset = [1.642539414e-26,1.651180301e-26,1.680279161e-26,1.839319208e-26];


num_run=1;
num_runs=1;
runs=[1];
t_limit=50000;

for i2=1:length(Tset)
                T = Tset(i2);
                volume = volset(i2);
            for i1=1:num_runs
                str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\alloy\sigma\1.2\0.9\');
                   str=strcat(str_main,'J0Jt_',int2str(i1),'_',int2str(i2),'_grep.dat');
                   A=dlmread(str);
                   if i1==1
                   JJ(1:length(A)-1,1:2)=0;
                   end
                   JJ(:,1) = A(2:length(A),2)*dt;
                   JJ(:,2) = JJ(:,2) + (A(2:length(A),4)+A(2:length(A),5)+A(2:length(A),6))/3;  %Jx
            end
            JJ(:,2) = JJ(:,2)/num_runs;

            cnt=1;
            for i3=2:100:t_limit;
            Z(cnt,2) = trapz(JJ(1:i3,1),JJ(1:i3,2));
            Z(cnt,1) = JJ(i3,1);
            cnt=cnt+1;
            end
            
%            pause

            k(:,2) = (volume/(3*kb*(T^2))) * Z(:,2);
            JJ(:,1) = JJ(:,1)/ps;
            k(:,1) = Z(:,1)/ps;

            num_to_plot=t_limit;
            
%            pause

            %subplot(2,1,1), plot(JJ(1:t_limit,1),JJ(1:t_limit,2)/JJ(1,2));
            %plot(JJ(1:t_limit,1),JJ(1:t_limit,2)/JJ(1,2));
            %axis([0 max(JJ(1:1000,1)) min((JJ(1:1000,2)/JJ(1,2))) 1]);
             %   title(strcat('T = 10K'),'FontSize',24);
             %   xlabel('t (ps)','FontSize',24); 
             %   ylabel('\langle S(t)S(0) \rangle/\langle S(0)S(0) \rangle' ,'FontSize',24);
            %[I,J,V] = find(k~=0); k2(:,1) = k(I,1); k2(:,2) = k(I,2);
            %subplot(2,1,2), plot(1:length(k2),k2);
            plot(1:length(k),k(:,2),'.');
            %axis([0 max(JJ(1:1000,1)) 0 max(k)+0.5]);
                xlabel('t (ps)','FontSize',24); 
                ylabel('\kappa (W/m-K)' ,'FontSize',24);
                length(k)
                start = input('start ');
                stop = input('stop ');
               

            kappa(i2,1) = Tset(i2);

            kappa(i2,2) = mean(k(start:stop,2));
            
            kappa(i2,3) = std(k(start:stop,2));

            str=strcat(str_main,'JJ_',int2str(i2),'.dat');
            dlmwrite(str,JJ);
            str=strcat(str_main,'kappa_',int2str(i2),'.dat');
            dlmwrite(str,k);

end

            str=strcat(str_main,'kappa(T).dat');
            dlmwrite(str,kappa);
            
            errorbar(kappa(:,1),kappa(:,2),kappa(:,3))
