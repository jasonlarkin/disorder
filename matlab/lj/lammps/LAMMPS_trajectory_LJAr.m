
%Number of Particles
numparticles=256;
numatoms=256;
%Cell Vectors Matrix
vector =[40.0    0.000000    0.000000
        0.000000   40.0    0.000000
        0.000000    0.000000   40.0];
    alat=vector(1,1);
volume=alat^3; %this needs to be specified since the cell is non-cubic.
dt = 0.002;
%Trajector File Location
str = strcat('C:\Documents and Settings\Jason\Desktop\npt_heat_1.dump');
%[A(:,1),A(:,2),A(:,3),A(:,4)] = textread(str,'Atoms','%f%f%f%f');
fid=fopen(str);
A = textscan(fid,'%f%f%f%f','Delimiter',' ','TreatAsEmpty',{'Atoms'});

buf=2;
cnt=1; dt=1; RDF=0; rdf_cnt=1;

for i=(buf+1):numparticles+buf:length(A{1}(:))
    
    %Calculate initial positions
    if cnt==1
        xi(:,1) = A{1}(i:i+numparticles-1);
        xi(:,2) = A{2}(i:i+numparticles-1);
        xi(:,3) = A{3}(i:i+numparticles-1);
        xi(:,4) = A{4}(i:i+numparticles-1);
        %Update previous xp to check if atom crosses boundary
        xf=xi; xp=xi;
    end
    
    x(:,1) = A{1}(i:numparticles+i-1);
    x(:,2) = A{2}(i:numparticles+i-1);
    x(:,3) = A{3}(i:numparticles+i-1);
    x(:,4) = A{4}(i:numparticles+i-1);
    
    
    %Plot Trajectories
    axis([min(min(x)) max(max(x)) min(min(x)) max(max(x)) min(min(x)) max(max(x))])
    view([120.0,45.0])
    hand=plot3(x(:,2),x(:,3),x(:,4),'.');
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

