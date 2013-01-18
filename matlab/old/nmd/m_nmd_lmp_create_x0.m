
function nmd_lmp_create_x0( NMD )




if strcmp( NMD.system(2).str , 'alloy' )
	
    if NMD.alloy_conc == 0.0
	NMD = nmd_lmp_create_x0_alloy_single_stringchanges( NMD );
	else
	NMD = nmd_lmp_create_x0_alloy_binary_stringchanges( NMD );	
    end
    
elseif strcmp( NMD.system(2).str , 'superlattice' )

elseif strcmp( NMD.system(2).str , 'whatever' )

end



lmp.x0.alloy.single.temp

lmp.x0.alloy.binary.temp


str(1).orig = [

str(1).change = {




%output lammps    
        str.orig = 'NUM_ATOMS';
        str.change = [int2str(NMD.NUM_ATOMS)];
        str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
        if NMD.alloy_conc == 0.0
        str.orig = 'NUM_ATOMS_TYPE';
        str.change = [int2str(NMD.NUM_ATOMS_TYPE-1)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        else
        str.orig = 'NUM_ATOMS_TYPE';
        str.change = [int2str(NMD.NUM_ATOMS_TYPE)];
        str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        end
        
        str.orig = 'LX';
        str.change = [num2str((NMD.Nx)*NMD.latvec(1,1))];
        str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LY';
        str.change = [num2str((NMD.Ny)*NMD.latvec(2,2))];
        str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'LZ';
        str.change = [num2str((NMD.Nz)*NMD.latvec(3,3))];
        str.cmd5 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        str.orig = 'ATOM_MASS_1';
        str.change = ['1 ' num2str(NMD.m(1))];
        str.cmd6 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
        
    if NMD.alloy_conc == 0.0
        str.mass2 ='';
    else
    str.orig = 'ATOM_MASS_2';
    str.change = ['2 ' num2str(NMD.m(2))];
    str.mass2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    end
        
    str.cmd8 =...
        ['lmp.in.x0.temp > ./' int2str(NMD.seed.alloy) '/lmp.in.x0.' int2str(NMD.seed.alloy)];
        
        str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5...
            str.cmd6 str.mass2 str.cmd8];
        
        system(str.cmd);
        
        output = [NMD.x0(:,1:5)];
    str.write=strcat(NMD.str.main,['/' int2str(NMD.seed.alloy) '/lmp.in.x0.' int2str(NMD.seed.alloy)]);
        dlmwrite(str.write,output,'-append','delimiter','\t');





end