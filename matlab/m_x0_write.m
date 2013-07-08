function m_x0_write(x0,str_name)
% m_x0_write(x0,str,name) writes a structure file x0
%--------------------------------------------------------------------------

if exist(strcat(str_name))
    disp([str_name ' exists']);
    system(['rm ' str_name]);
end

%output relaxed
output = [x0.NUM_ATOMS x0.NUM_ATOMS_UCELL x0.Lx x0.Lx x0.Lx];
x =...
    [ x0.id x0.m x0.x x0.y x0.z ];
dlmwrite(strcat(str_name),...
    output ,'-append','delimiter',' ');
dlmwrite(strcat(str_name),...
    x ,'-append','delimiter',' ');
    
end
%--------------------------------------------------------------------------	


