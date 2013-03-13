clear
%x = load(['/home/jason/disorder/si/amor/normand/','init216.conf.mol'])

str.main = '/home/jason/disorder/si/amor/normand/';

fid = fopen([str.main,'perfect3_keating.matlab'],'r');  % Open text file
data = textscan(fid, '%s%n%n%n');

plot3(data{2},data{3},data{4},'.')
pause
x0.x = data{2} - min(data{2});
x0.y = data{3} - min(data{3});
x0.z = data{4} - min(data{4});
plot3(x0.x,x0.y,x0.z,'.')

x0.Nx = 5;

x0.Lx = x0.Nx*5.43; x0.Ly = x0.Lx; x0.Lz = x0.Lx;
x0.NUM_ATOMS = length(x0.x); x0.NUM_ATOMS_UCELL = length(x0.x);

x0.id = (1:length(x0.x))'; x0.m = ones(length(x0.x),1);

%output relaxed
output = [x0.NUM_ATOMS x0.NUM_ATOMS_UCELL x0.Lx x0.Lx x0.Lx];
x =...
    [ x0.id x0.m x0.x x0.y x0.z ];
name = 'x0.data';
dlmwrite(strcat(str.main,name),...
    output ,'-append','delimiter',' ');
dlmwrite(strcat(str.main,name),...
    x ,'-append','delimiter',' ');