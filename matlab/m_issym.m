function istrue = m_issym(kpt1,kpt2)
istrue=0.0;
istrue1=0.0; istrue2=0.0;
        for i2=-1:2:1
            for i3=-1:2:1
                for i4=-1:2:1
                    
                    temp(1) = i2*kpt2(1); temp(2) = i3*kpt2(2); temp(3) = i4*kpt2(3);

                    if kpt1(1) == temp(1) && kpt1(2) == temp(2) && kpt1(3) == temp(3)
                        istrue=1.0;
                    elseif kpt1(1) == temp(3) && kpt1(2) == temp(1) && kpt1(3) == temp(2)
                        istrue=1.0;
                    elseif kpt1(1) == temp(2) && kpt1(2) == temp(3) && kpt1(3) == temp(1)
                        istrue=1.0;
                    elseif kpt1(1) == temp(1) && kpt1(2) == temp(3) && kpt1(3) == temp(2)
                        istrue=1.0;
                    elseif kpt1(1) == temp(3) && kpt1(2) == temp(2) && kpt1(3) == temp(1)
                        istrue=1.0;
                    elseif kpt1(1) == temp(2) && kpt1(2) == temp(1) && kpt1(3) == temp(3)
                        istrue=1.0;
                    end
                end
            end
        end
end
