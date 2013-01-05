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

Tset = [2.5,7.5,20,70];
%4_1
%volset = [9.485306458e-27,9.600254801e-27,9.762507193e-27,9.916736609e-27,1.00502924e-26,1.026701388e-26,1.058969023e-26,1.084473696e-26];
%6_1
%volset = [3.198183373e-26,3.24411101e-26,3.292503829e-26,3.343304189e-26,3.401211716e-26,3.459282477e-26,3.554119199e-26,3.698437416e-26];
%6_3
%volset = [3.234100086e-26,3.278254744e-26,3.330436933e-26,3.384293562e-26,3.437204932e-26,3.512807754e-26,3.581092032e-26,3.696440073e-26];
%9_4
%volset = [1.079557619e-25,1.094146842e-25,1.112124154e-25,1.132835657e-25,1.149198387e-25,1.172091925e-25,1.198361318e-25,1.229319312e-25];
%11_1
%volset = [1.971057963e-25,1.998852168e-25,2.028311616e-25,2.060027398e-25,2.098713417e-25,2.137393126e-25,2.188334498e-25,2.252928777e-25];
%12_4
%for 10K-80K
%volset = [2.55906938e-25,2.59504428e-25,2.634820501e-25,2.675032975e-25,2.72419265e-25,2.777613458e-25,2.834152952e-25,2.924939544e-25];
%for 1K-70K
%volset = [2.534571759e-25,2.542089228e-25,2.559071251e-25,2.675032975e-25,2.72419265e-25,2.777613458e-25,2.834152952e-25,2.924939544e-25];

%8
%8
volset = [7.509589599e-26,7.557599406e-26,7.688928652e-26,8.419235803e-26];


num_run=1;
num_runs=3;
runs=[1,2,3];
t_limit=50000;

for i2=1:length(Tset)
                T = Tset(i2);
                volume = volset(i2);
            for i=1:num_runs
                %Average J from 10 NVE lammps runs
                file_index=1;
                str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\substitution\vary_mass\8\8\');
                   %str=strcat(str_main,'log_kappa',int2str(i),'.lammps');
                   str=strcat(str_main,'J0Jt_',int2str(i),'_',int2str(i2),'_grep.dat');
                   A=dlmread(str);
                   if i==1
                   JJ(1:length(A)-1,1:2)=0;
                   end
                   JJ(:,1) = A(2:length(A),2)*dt;
                   JJ(:,2) = JJ(:,2) + (A(2:length(A),3)+A(2:length(A),4)+A(2:length(A),5))/3;  %Jx
            end
            JJ(:,2) = JJ(:,2)/num_runs;

            cnt=1;
            for i=2:100:t_limit;
            Z(i,2) = trapz(JJ(1:i,1),JJ(1:i,2));
            Z(i,1) = JJ(i,1);
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
            [I,J,V] = find(k~=0); k2(:,1) = k(I,1); k2(:,2) = k(I,2);
            %subplot(2,1,2), plot(1:length(k2),k2);
            plot(k2(:,1),k2(:,2),'.');
            %axis([0 max(JJ(1:1000,1)) 0 max(k)+0.5]);
                xlabel('t (ps)','FontSize',24); 
                ylabel('\kappa (W/m-K)' ,'FontSize',24);
                length(k2)
                start = input('start ');
                stop = input('stop ');
               

            kappa(i2,1) = Tset(i2);

            kappa(i2,2) = mean(k2(start:stop,2));
            
            kappa(i2,3) = std(k2(start:stop,2));

            str=strcat(str_main,'JJ_',int2str(i2),'.dat');
            dlmwrite(str,JJ);
            str=strcat(str_main,'kappa_',int2str(i2),'.dat');
            dlmwrite(str,k2);

end

            str=strcat(str_main,'kappa(T).dat');
            dlmwrite(str,kappa);
            
            errorbar(kappa(:,1),kappa(:,2),kappa(:,3))
