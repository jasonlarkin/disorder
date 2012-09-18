function m_x0_write(x0,str,name)
% m_x0_write(x0,str,name) writes a structure file x0
%--------------------------------------------------------------------------
    
%output relaxed
    output = [x0.NUM_ATOMS x0.NUM_ATOMS x0.Lx x0.Lx x0.Lx];

    x =...
        [ x0.id x0.m x0.x x0.y x0.z ];
    
    dlmwrite(strcat(str,name),...
        output ,'-append','delimiter',' ');
    dlmwrite(strcat(str,name),...
        x ,'-append','delimiter',' ');
    
end
%--------------------------------------------------------------------------	


