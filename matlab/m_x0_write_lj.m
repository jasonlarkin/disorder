function m_x0_write_lj(x0,str_main,name)
% x0 = m_x0_lmp_write_lj(str) writes a structure file x0
%--------------------------------------------------------------------------
    
%-------------------------------------------------------------------------- 

%output relaxed
    output = [x0.NUM_ATOMS x0.NUM_ATOMS x0.Lx x0.Lx x0.Lx];

    x =...
        [ x0.id x0.m x0.x x0.y x0.z ];
    
    dlmwrite(strcat(str_main,name),...
        output , 'delimiter',' ');
    dlmwrite(strcat(str_main,name),...
        x ,'-append','delimiter',' ');
    
end
%--------------------------------------------------------------------------	


