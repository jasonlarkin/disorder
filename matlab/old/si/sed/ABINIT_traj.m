
%Number of Particles
numparticles=64;
numatoms=64;
%Cell Vectors Matrix
vector =[20.2    0.000000    0.000000
        0.000000   20.2    0.000000
        0.000000    0.000000   20.2];
    alat=vector(1,1);
volume=alat^3; %this needs to be specified since the cell is non-cubic.
%----------------------Units------------------------------------------
dt = 20.0;
au = 2.42E-17; %s 
Si_mass = 4.66E-26; %kg/atom
ang2bohr = 1.889726;
kb = 1.38E-23; %J/K


%Trajector File Location
str = strcat('E:\CMU\work\Si\ABINIT\MD\64_atoms\Liquid\short\3000K\coords.txt');
%[A(:,1),A(:,2),A(:,3),A(:,4)] = textread(str,'Atoms','%f%f%f%f');
fid=fopen(str);
A = textscan(fid,'%f%f%f','Delimiter','\t','commentStyle', '--');
%A = textscan(fid,'%f%f%f','Delimiter','  ','TreatAsEmpty',{'--'});
%A = textscan(fid,'%*[^=]%*[=]%f%*[m]','TreatAsEmpty',{'--'});

str = strcat('E:\CMU\work\Si\ABINIT\MD\64_atoms\Liquid\short\3000K\vel.txt');
%[A(:,1),A(:,2),A(:,3),A(:,4)] = textread(str,'Atoms','%f%f%f%f');
fid=fopen(str);
B = textscan(fid,'%f%f%f','Delimiter','\t','commentStyle', '--');

buf=0;
cnt=1; dt=1; RDF=0; rdf_cnt=1; spacing=10;

for i=(buf+1):(numparticles+buf)*spacing:length(A{1}(:))
    
    %Calculate initial positions
    if cnt==1
        xi(:,1) = A{1}(i:i+numparticles-1);
        xi(:,2) = A{2}(i:i+numparticles-1);
        xi(:,3) = A{3}(i:i+numparticles-1);
        %Update previous xp to check if atom crosses boundary
        xf=xi; xp=xi;
        
        vi(:,1) = B{1}(i:i+numparticles-1)*(10^-10)/(au*ang2bohr);    %convert to m/s
        vi(:,2) = B{2}(i:i+numparticles-1)*(10^-10)/(au*ang2bohr);
        vi(:,3) = B{3}(i:i+numparticles-1)*(10^-10)/(au*ang2bohr);
        
        T(cnt,1) = cnt*dt*au; 
        T(cnt,2) = (mean(sqrt(vi(:,1).^2 + vi(:,2).^2 + vi(:,3).^2))^2)*Si_mass /(3*kb)
        
    end
    
    x(:,1) = A{1}(i:numparticles+i-1);
    x(:,2) = A{2}(i:numparticles+i-1);
    x(:,3) = A{3}(i:numparticles+i-1);
    
    v(:,1) = B{1}(i:numparticles+i-1)*(10^-10)/(au*ang2bohr);
    v(:,2) = B{2}(i:numparticles+i-1)*(10^-10)/(au*ang2bohr);
    v(:,3) = B{3}(i:numparticles+i-1)*(10^-10)/(au*ang2bohr);
    
    T(cnt,1) = cnt*dt*au; 
    T(cnt,2) = (mean(sqrt(v(:,1).^2 + v(:,2).^2 + v(:,3).^2))^2)*Si_mass /(3*kb);
    
    T(cnt,2)
    
    MSD(cnt,1) = cnt*dt*au; 
    MSD(cnt,2) = mean( (x(:,1)-xi(:,1)).^2 + (x(:,2)-xi(:,2)).^2 + (x(:,3)-xi(:,3)).^2);
    
    %Plot Trajectories
    axis([min(min(x)) max(max(x)) min(min(x)) max(max(x)) min(min(x)) max(max(x))])
    view([120.0,45.0])
    hand=plot3(x(:,1),x(:,2),x(:,3),'.');
    set(hand, 'MarkerSize', 30);
%    axis([-1 alat -1 alat -1 alat])
    %view([45.0,45.0,45.0]);
    title(strcat('t =',int2str(cnt*10),' ps'),'FontSize',24);
    xlabel(char(197),'FontSize',24); 
    ylabel(char(197),'FontSize',24);
    zlabel(char(197),'FontSize',24);
%    hleg1 = legend('Sb','Te','Te');
    %view([90.0,90.0])
%    saveas(gcf, strcat('output',int2str(cnt)), 'jpg')
%    x;
    cnt=cnt+1;
    pause
    
 
    
end

fclose(fid);

plot(T(:,1),T(:,2))
plot(MSD(:,1),MSD(:,2))

