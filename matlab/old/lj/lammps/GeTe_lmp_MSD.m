
%Number of Particles
numparticles=540;

dt = 0.001;


%MSD File Location
str = strcat('D:\CMU\vasp\Juarez\Sb2Te3\MD\Shaun\B\npt\dumpSb.msd');
data1= readmsd_Sb(str);

str = strcat('D:\CMU\vasp\Juarez\Sb2Te3\MD\Shaun\B\npt\dumpTe1.msd');
data2= readmsd_Te1(str);

str = strcat('D:\CMU\vasp\Juarez\Sb2Te3\MD\Shaun\B\npt\dumpTe2.msd');
data3= readmsd_Te2(str);

%Set times
MSD(1:length(data1.timestep),1) = data1.timestep(1,:)*dt;

%Calcualte MSD
for i=1:length(data1.timestep)
MSD(i,2) = mean(data1.atom_data(:,i));  
MSD(i,3) = mean(data2.atom_data(:,i));
MSD(i,4) = mean(data3.atom_data(:,i));
end

plot(MSD(:,1),MSD(:,2),'.',MSD(:,1),MSD(:,3),'.')

pause

dlmwrite('D:\CMU\vasp\Juarez\GeTe\GeTe_Pot_Structures\DRS_ZB_DRSVaryCellSize\MD\32\5000K\nvt\1000K\nve_msd\MSD_matlab.dat',MSD);