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
                
                    str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\alloy\m3\0.5\');
                    str=strcat(str_main,'JJ_',int2str(i2),'.dat');
                    JJ(:,:,i2)=dlmread(str);
                    str=strcat(str_main,'kappa_',int2str(i2),'.dat');
                    kappa(:,:,i2) = dlmread(str);
end  

subplot(2,1,1), plot(JJ(1:3000,1,1),JJ(1:3000,2,1)/JJ(1,2,1),JJ(1:3000,1,2),JJ(1:3000,2,2)/JJ(1,2,2),JJ(1:3000,1,3),JJ(1:3000,2,3)/JJ(1,2,3))
                    %title(strcat('T = 10K'),'FontSize',24);
                    xlabel('t (ps)','FontSize',24); 
                    ylabel('\langle S(t)S(0) \rangle/\langle S(0)S(0) \rangle','FontSize',24);
                    hleg1 = legend('T=2.5K','T=5K','T=10K');

subplot(2,1,2), plot(kappa(1:30,1,1),kappa(1:30,2,1),kappa(1:30,1,2),kappa(1:30,2,2),kappa(1:30,1,3),kappa(1:30,2,3))
                    xlabel('t (ps)','FontSize',24); 
                    ylabel('\kappa (W/m-K)' ,'FontSize',24);
                    hleg2 = legend('T=2.5K','T=5K','T=10K');
                    
                    %start = input('start ');
                    %stop = input('stop ');
