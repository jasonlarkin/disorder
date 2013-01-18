%Define Boltzmann's constant
%kb = 8.6170e-005; %in eV/K
kb = 1.380e-23;  %in J/K
ps=10^-12;
%evToj=1.6022*10^(-19);
%psTos=10^(-12);
%angTom=10^(-10);
mass_Ar=6.63*10^(-26); eps_Ar=1.67*10^(-21); sigma_Ar=3.4*10^(-10); tau_Ar=2.1423*10^(-12);
%Calculate cut-off frequency for filtering
step_size = 0.002; sample_rate = 5;
dt = step_size*sample_rate*tau_Ar;
%sample freq: this should be 2 Thz
%f = 1/dt;
%calculate filter mask: use 0.2 THz as a guess for now
%fNorm = 0.1 / (f/2);
%[b,a] = butter(10, fNorm, 'low');

%Calcaulte scale factor for J
scaleJ = (eps_Ar/(tau_Ar*(sigma_Ar^2)));
AvgT=0;
num_runs=1;

num_run=1;
for i=1:num_runs
    %Average J from 10 NVE lammps runs
    file_index=1;
    str_main='E:\CMU\work\Phonons\LJArgon\Solid\crystal\substitution\500_atoms\1.4_2atoms\3\';
       %str=strcat(str_main,'log_kappa',int2str(i),'.lammps');
       str=strcat(str_main,'log_kappa',int2str(num_run),'.lammps');
       A=dlmread(str);
       J(:,1)=A(:,6)*scaleJ;  %Jx
       J(:,2)=A(:,7)*scaleJ;  %Jy
       J(:,3)=A(:,8)*scaleJ;  %Jz
       T = mean(A(:,2))*eps_Ar/kb
       P = mean(A(:,3))
       AvgT = AvgT + T;
    if i==1
        Savg(1:length(J),1:4)=0;
    end
    %Grab volume from tenth file
    %volume = A(1,5);
    volume = A(1,4)*(sigma_Ar^3);

    %Calcualte HCACF with xcorr
    %avgH(:,1)=zeros(2*(length(J))-1,1);
    %avgH(:,2)=zeros(2*(length(J))-1,1);
    %avgH(:,3)=zeros(2*(length(J))-1,1);

    H(:,1) = xcorr(J(:,1),'unbiased');
    H(:,2) = xcorr(J(:,2),'unbiased');
    H(:,3) = xcorr(J(:,3),'unbiased');

    %avgH(:,1)=H(:,1,1)+avgH(:,1);
    %avgH(:,2)=H(:,2,1)+avgH(:,2);
    %avgH(:,3)=H(:,3,1)+avgH(:,3);

    %avgH=avgH/5;
    %H = autocorr(AvgJ);
    [I,K]=max(H(:,1));
    %cut HCACF in half to get rid of -Tau measurement
    S(:,2) = H(floor(length(H)/2)+1:length(H(:,1)),1);
    S(:,1) = (1:length(S))*dt;
    S(:,3) = H(floor(length(H)/2)+1:length(H(:,1)),2);
    S(:,4) = H(floor(length(H)/2)+1:length(H(:,1)),3);

    Savg(:,1)=S(:,1);
    Savg(:,2:4)=Savg(:,2:4)+S(:,2:4);

end

clear S
Savg(:,2:4)=Savg(:,2:4)/i; S=Savg;
plot(S(:,1,1),S(:,2,1),S(:,1,1),S(:,3,1),S(:,1,1),S(:,4,1));

t_limit=20000;

for i=2:t_limit
Z1(i,1)=trapz(S(1:i,1),S(1:i,2));
Z2(i,1)=trapz(S(1:i,1),S(1:i,3));
Z3(i,1)=trapz(S(1:i,1),S(1:i,4));
end
Zavg=(Z1+Z2+Z3)/3;
Savg(:,1,1)=S(:,1,1); Savg(:,2,1)=(S(:,2,1)+S(:,3,1)+S(:,4,1))/3; 

%calculate k for Z1 and Z2

%k = (volume/(kb*(T^2))) * Zavg * (eps_Ar/(tau_Ar*(sigma_Ar^2)));
k = (volume/(kb*(T^2))) * Zavg;
Savg(:,1)=Savg(:,1)/ps;

num_to_plot=t_limit;

subplot(2,1,1), plot(Savg(1:1000,1),Savg(1:1000,2)/Savg(1,2));
axis([0 max(Savg(1:1000,1)) min(Savg(1:1000,2)/Savg(1,2)) 1]);
    title(strcat('T = 80K'),'FontSize',24);
    xlabel('t (ps)','FontSize',24); 
    ylabel('\langle S(t)S(0) \rangle/\langle S(0)S(0) \rangle' ,'FontSize',24);
subplot(2,1,2), plot(Savg(1:1000,1),k(1:1000));
axis([0 max(Savg(1:1000,1)) 0 max(k)+0.5]);
    xlabel('t (ps)','FontSize',24); 
    ylabel('\kappa (W/m-K)' ,'FontSize',24);

%pause
    
str=strcat(str_main,'Savg_',int2str(num_run),'.dat');
dlmwrite(str,Savg(1:t_limit,:));
str=strcat(str_main,'kappa_',int2str(num_run),'.dat');
dlmwrite(str,k(1:t_limit));

