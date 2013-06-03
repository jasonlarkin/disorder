clear
x0 = m_x0_read(...
    '/home/jason/disorder2/sio2/alan/576a/tile/anneal/emin/x0.data');

x0n.x = []; x0n.y = []; x0n.z = []; x0n.id = []; x0n.m = [];
icnt=1;
for i1=0:1
    for i2=0:1
        for i3=0:1
            x0n.x = [x0n.x;(x0.x + i1*x0.Lx)];
            x0n.y = [x0n.y;(x0.y + i2*x0.Ly)];
            x0n.z = [x0n.z;(x0.z + i3*x0.Lz)];
            x0n.m = [x0n.m;(x0.m)];
            if icnt==1
                x0n.id = [x0.id];
            else
            x0n.id = [x0n.id;((1:length(x0.id))+max(x0n.id))'];
            end
            icnt = icnt +1;
        end
    end
end

plot3(x0n.x,x0n.y,x0n.z,'.')

pause

formats = '%2.6f';
output = [x0n.id x0n.m x0n.x x0n.y x0n.z];
str.write=...
    ['/home/jason/disorder2/sio2/alan/576a/tile/anneal/emin/tile/' 'lmp.in.x0'];
fileID = fopen(str.write,'a');
for iout = 1:length(output)
fprintf(fileID,'%5d \t %5d \t %7.7f \t %7.7f \t %7.7f\n',output(iout,:));
end