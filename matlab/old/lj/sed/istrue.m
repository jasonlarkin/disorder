function istrue=issym(kptlist,kpt2)
istrue=0.0;
sizekpt1 = size(kpt1);
    for i1=1:sizekpt1(1,1)
        %kpt1 = kptlist(i1,1:3);
        for i2=-1:2:1
            for i3=-1:2:1
                for i4=-1:2:1
                    
                    temp(1) = i2*kpt2(1); temp(2) = i3*kpt2(2); temp(3) = i4*kpt2(3);
                    
                    tmp1 = find(temp(1)==kpt1); tmp2 = find(temp(2)==kpt1); tmp3 = find(temp(3)==kpt1);
                    
                    if tmp1~=0.0 & tmp2~=0.0 & tmp3~=0.0
                        istrue=1;
                    end
                end
            end
        end
    end
            
            
