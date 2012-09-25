function nmd_gulp_run( NMD )

    if  strcmp( NMD.type(2).str , 'DISP' )
        
        gulp_disp( NMD.gulp, NMD.x0 );
        
        nmd_gulp_submit
        
    elseif strcmp( NMD.type(2).str , 'GAMMA' )
   
    end
    
end