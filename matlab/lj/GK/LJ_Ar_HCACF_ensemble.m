%Define Boltzmann's constant
kb = 8.6170e-005; %in eV/K
%kb = 1.380e-23;  %in J/K
evToj=1.6022*10^(-19);
psTos=10^(-12);
angTom=10^(-10);

%Calculate cut-off frequency for filtering
step_size = 0.002; sample_rate = 10;
dt = step_size*sample_rate;
%sample freq: this should be 2 Thz
f = 1/dt;
%calculate filter mask: use 0.2 THz as a guess for now
fNorm = 0.1 / (f/2);
[b,a] = butter(10, fNorm, 'low');

%Define average variables
AvgT = 0.0;
%AvgJ = zeros(1001,1);

%Average J from 10 NVE lammps runs
for j=1:1,
   str=strcat('D:\CMU\work\Phonons\LJArgon\Solid\80K\kappa\log_matlab',int2str(j),'.lammps');
   A=dlmread(str);
   J(:,j,1)=A(:,14);  %Jx
   J(:,j,2)=A(:,15);  %Jy
   J(:,j,3)=A(:,16);  %Jz
   T = mean(A(:,2));
   AvgT = AvgT + T;
   std(A(:,2))
end
%AvgT = AvgT/5;

%Grab volume from tenth file
%volume = A(1,5);
volume = A(1,7);

%Calcualte HCACF with xcorr
avgH(:,1)=zeros(2*(length(J))-1,1);
avgH(:,2)=zeros(2*(length(J))-1,1);
avgH(:,3)=zeros(2*(length(J))-1,1);
for i=1:j
H(:,1,i) = xcorr(J(:,i,1),'unbiased');
H(:,2,i) = xcorr(J(:,i,2),'unbiased');
H(:,3,i) = xcorr(J(:,i,3),'unbiased');

avgH(:,1)=H(:,1,1)+avgH(:,1);
avgH(:,2)=H(:,2,1)+avgH(:,2);
avgH(:,3)=H(:,3,1)+avgH(:,3);
end
%avgH=avgH/5;
%H = autocorr(AvgJ);
[I,K]=max(avgH(:,1));
%cut HCACF in half to get rid of -Tau measurement
S(:,2,1) = avgH(floor(length(avgH)/2)+1:length(avgH(:,1)),1);
S(:,1,1) = (1:length(S))*dt;
S(:,3,1) = avgH(floor(length(avgH)/2)+1:length(avgH(:,1)),2);
S(:,4,1) = avgH(floor(length(avgH)/2)+1:length(avgH(:,1)),3);
for i=1:j
    S(:,2,i+1) = H(floor(length(avgH)/2)+1:length(avgH(:,1)),1,i);
    S(:,1,i+1) = (1:length(S))*dt;
    S(:,3,i+1) = H(floor(length(avgH)/2)+1:length(avgH(:,1)),2,i);
    S(:,4,i+1) = H(floor(length(avgH)/2)+1:length(avgH(:,1)),3,i);
end
%filter
%for i=1:j+1
%Sf(:,1,i) = filtfilt(b, a, S(:,2,i));
%Sf(:,2,i) = filtfilt(b, a, S(:,3,i));
%Sf(:,3,i) = filtfilt(b, a, S(:,4,i));
%end
%plot before and after
%figure(1);
%plot(S(:,1,1),S(:,2,1),S(:,1,1),Sf(:,1,1));
%axis([-1 alat -1 alat -1 alat])
%figure(2);
%plot(S(:,1,1),S(:,3,1),S(:,1,1),Sf(:,2,1));
%figure(3);
%plot(S(:,1,1),S(:,4,1),S(:,1,1),Sf(:,3,1));

%here is name of function to do double exp fit
%xdata = S(:,1);
%ydata = Sf;
%x = lsqcurvefit(@(x,xdata) x(1)*exp(-xdata/x(2)) + x(3)*exp(-xdata/x(4)), [1 0.5 1 1], xdata, ydata) %-- better guesses?

%Plot fitted double exponential
%figure(2);
%plot(xdata, x(1)*exp(-xdata/x(2)) + x(3)*exp(-xdata/x(4)), S(:,1), Sf);

%Integration: this just does a crude trapezoidal (direct) integration of J
%Z1 is before filtering, Z2 after
for i=1:j+1
Z1(i)=trapz(S(1:length(S)/2,1),S(1:length(S)/2,1,i));
Z2(i)=trapz(S(1:length(S)/2,1),S(1:length(S)/2,2,i));
Z3(i)=trapz(S(1:length(S)/2,1),S(1:length(S)/2,3,i));
end

%Z2=trapz(S(:,1),Sf);
%Z3=trapz(xdata, x(1)*exp(-xdata/x(2)) + x(3)*exp(-xdata/x(4)));

%calculate k for Z1 and Z2
for i=1:j+1
k1(i) = (volume/(3*kb*(T^2)))*Z1(i)*((evToj)/(psTos*angTom));
k2(i) = (volume/(3*kb*(T^2)))*Z2(i)*((evToj)/(psTos*angTom));
k3(i) = (volume/(3*kb*(T^2)))*Z3(i)*((evToj)/(psTos*angTom));
end

mean(k1)
std(k1)
mean(k2)
std(k2)
mean(k3)
std(k3)
