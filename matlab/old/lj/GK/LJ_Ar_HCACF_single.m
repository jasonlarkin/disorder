%Define Boltzmann's constant
%kb = 8.6170e-005; %in eV/K
kb = 1.380e-23;  %in J/K
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

%Define average variables
AvgT = 0.0;
%AvgJ = zeros(1001,1);

num_runs=5;
Savg=0;
for i=1:num_runs
    %Average J from 10 NVE lammps runs
    file_index=1;
       str=strcat('D:\CMU\work\Phonons\LJArgon\Solid\10K',int2str(i),'\kappa\log_matlab.lammps');
       A=dlmread(str);
       J(:,1)=A(:,14)*scaleJ;  %Jx
       J(:,2)=A(:,15)*scaleJ;  %Jy
       J(:,3)=A(:,16)*scaleJ;  %Jz
       T = mean(A(:,2))*eps_Ar/kb;
       AvgT = AvgT + T;
       std(A(:,2))

    %Grab volume from tenth file
    %volume = A(1,5);
    volume = A(1,7)*(sigma_Ar^3);

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
    S(:,2,1) = H(floor(length(H)/2)+1:length(H(:,1)),1);
    S(:,1,1) = (1:length(S))*dt;
    S(:,3,1) = H(floor(length(H)/2)+1:length(H(:,1)),2);
    S(:,4,1) = H(floor(length(H)/2)+1:length(H(:,1)),3);

    Savg(:,1)=S(:,1,1);
    Savg(:,2:4)=Savg(:,2:4)+S(:,2:4);

end

clear S
Savg(:,2:4)=Savg(:,2:4)/i; S=Savg;
plot(S(:,1,1),S(:,2,1),S(:,1,1),S(:,3,1),S(:,1,1),S(:,4,1));

for i=2:10000
Z1(i)=trapz(S(1:i,1),S(1:i,2));
Z2(i)=trapz(S(1:i,1),S(1:i,3));
Z3(i)=trapz(S(1:i,1),S(1:i,4));
end
Zavg=(Z1+Z2+Z3)/3;
Savg(:,1,1)=S(:,1,1); Savg(:,2,1)=(S(:,2,1)+S(:,3,1)+S(:,4,1))/3; 

%calculate k for Z1 and Z2

%k = (volume/(kb*(T^2))) * Zavg * (eps_Ar/(tau_Ar*(sigma_Ar^2)));
k = (volume/(kb*(T^2))) * Zavg;

plot(S(1:1000,1,1),Savg(1:1000,2,1)/Savg(1,2));
plot(S(1:10000,1,1),k);