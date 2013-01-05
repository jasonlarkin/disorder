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
dt = step_size*tau_Ar;
%sample freq: this should be 2 Thz
%f = 1/dt;
%calculate filter mask: use 0.2 THz as a guess for now
%fNorm = 0.1 / (f/2);
%[b,a] = butter(10, fNorm, 'low');

%Calcaulte scale factor for J
scaleJ = (eps_Ar)/((sigma_Ar^2)*tau_Ar);

Tset = [2.5,5,10];

%Tset = [0.0200*(eps_Ar/kb),0.064401693*(eps_Ar/kb),0.16112514*(eps_Ar/kb),0.58*(eps_Ar/kb)];

%SIGMA 1.05
%4_1
%volset = [240.5318077*(sigma_Ar^3),242.1622736*(sigma_Ar^3),245.4570139*(sigma_Ar^3)];
%4_3
%volset = [242.5800164*(sigma_Ar^3),244.3919438*(sigma_Ar^3),247.9126409*(sigma_Ar^3)];
%6_9
%volset = [811.9871769*(sigma_Ar^3),816.6950467*(sigma_Ar^3),828.7585513*(sigma_Ar^3)];
%8_20
%volset = [1924.089193*(sigma_Ar^3),1937.127469*(sigma_Ar^3),1964.64357*(sigma_Ar^3)];

%SIGMA 1.1
%0.01
%8_20
%volset = [1930.027324*(sigma_Ar^3),1936.337492*(sigma_Ar^3),1949.288939*(sigma_Ar^3)];
%0.05
%8_20
%volset = [2006.922205*(sigma_Ar^3),2014.110447*(sigma_Ar^3),2028.170365*(sigma_Ar^3)];
%0.1
%8_20
volset = [2086.551932*(sigma_Ar^3),2093.630762*(sigma_Ar^3),2107.535167*(sigma_Ar^3)];
%0.5
%8_20
%volset = [2442.691137*(sigma_Ar^3),2450.06878*(sigma_Ar^3),2466.122756*(sigma_Ar^3)];



%SIGMA 1.15
%4_1
%volset = [240.5318077*(sigma_Ar^3),242.1622736*(sigma_Ar^3),245.4570139*(sigma_Ar^3)];
%4_3
%volset = [242.5800164*(sigma_Ar^3),244.3919438*(sigma_Ar^3),247.9126409*(sigma_Ar^3)];
%6_9
%volset = [825.1736778*(sigma_Ar^3),830.7674613*(sigma_Ar^3),843.3271494*(sigma_Ar^3)];
%8_20
%volset =[1952.391447*(sigma_Ar^3),1965.416344*(sigma_Ar^3),1993.554861*(sigma_Ar^3)];


num_run=1;
num_runs=10;
runs=[1,2,3,4,5,6,7,8,9,10];
t_limit=100000;

for i2=1:length(Tset)
                T = Tset(i2);
                volume = volset(i2);
            for i1=1:num_runs
                str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\alloy\sigma\1.1\0.1\');
                   str=strcat(str_main,'J0Jt_',int2str(i1),'_',int2str(i2),'_grep.dat');
                   A=dlmread(str);
                   if i1==1
                   JJ(1:length(A)-1,1:2)=0;
                   end
                   JJ(:,1) = A(2:length(A),2)*dt;
                   JJ(:,2) = JJ(:,2) + (A(2:length(A),4)+A(2:length(A),5)+A(2:length(A),6))/3;  %Jx
            end
            JJ(:,2) = JJ(:,2)/num_runs;
            JJ(:,2) = JJ(:,2)*(scaleJ^2);

            JJ2(:,1) = JJ(1:100:t_limit,1);
            JJ2(:,2) = JJ(1:100:t_limit,2);
            
            cnt=1;
            for i3=2:length(JJ2);
            Z(cnt,2) = trapz(JJ2(1:i3,1),JJ2(1:i3,2));
            Z(cnt,1) = JJ2(i3,1);
            cnt=cnt+1;
            end
            
%            pause

            k(:,2) = Z(:,2)*(volume/(kb*(T^2)));
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
            
            clear JJ JJ2 A Z k

end

            str=strcat(str_main,'kappa(T).dat');
            dlmwrite(str,kappa);
            
            errorbar(kappa(:,1),kappa(:,2),kappa(:,3))
