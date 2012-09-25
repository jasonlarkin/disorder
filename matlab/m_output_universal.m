function output_universal( NMD )

%output universal

if exist(...
strcat(NMD.str.main,'/x0_',num2str(NMD.alloy_conc),'_',int2str(NMD.seed.alloy),'.data'), 'file')~=0
system(['rm -f ./x0_' num2str(NMD.alloy_conc) '_' int2str(NMD.seed.alloy) '.data']);
end  

    str.write=strcat(...
        NMD.str.main,'/x0_',num2str(NMD.alloy_conc),'_',int2str(NMD.seed.alloy),'.data');
    output = [size(NMD.x0,1) size(NMD.x.cart,1) NMD.latvec(1,1)*(NMD.Nx)...
        NMD.latvec(2,2)*(NMD.Ny) NMD.latvec(3,3)*(NMD.Nz) ];
    dlmwrite(str.write,output,'-append','delimiter',' ');
    dlmwrite(str.write,NMD.x0,'-append','delimiter',' ');

end
