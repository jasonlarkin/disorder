function phi=pe(r2)
%FUNCTION: evaluate the potential energy for a given separation r2
    inv_r6 = 1./(r2*r2*r2);
    phi = 4.*(inv_r6*inv_r6 - inv_r6);
end