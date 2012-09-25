function nmd_gulp_submit( NMD )


    if  strcmp( NMD.type(2).str , 'DISP' )
        
        gulp_disp_crystal( NMD.x0 );
        
        
    elseif strcmp( NMD.type(2).str , 'GAMMA' )
        
        
        
    end
    
     
end