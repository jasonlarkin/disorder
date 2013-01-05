NUM_CELLS=4;
max = 1.0;
min = -1.0;
dk = (max-min)/(2*NUM_CELLS);
index=1;

a1=[1 1 0];
a2=[1 0 1];
a3=[0 1 1];
a2xa3 = cross(a2,a3);
a3xa1 = cross(a3,a1);
a1xa2 = cross(a1,a2);
a1dota2xa3 = dot(a1,cross(a2,a3))/2;

b1 = a2xa3/a1dota2xa3; b2 = a3xa1/a1dota2xa3; b3 = a1xa2/a1dota2xa3;


for i1=min:dk:max
    for i2=min:dk:max
        for i3=min:dk:max
            kptlist(index,1:3) = i1*b1+i2*b2+i3*b3;
            index=index+1;
        end
    end
end
            
plot3(kptlist(:,1),kptlist(:,2),kptlist(:,3),'.')