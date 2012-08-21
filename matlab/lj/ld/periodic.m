function r=periodic(r,L)
%determine periodic boundary conditions
            if r > (L/2)          %periodic boundaries
                r = r - L;
            end
            if r < (-L/2)         %periodic boundaries
                r = r + L;
            end

