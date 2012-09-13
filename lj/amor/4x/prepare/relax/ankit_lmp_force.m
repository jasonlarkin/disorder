
function force = ankit_lmp_force(LD)

%--------------------------------------------------------------------------
%output lammps  
%--------------------------------------------------------------------------    

format long
  
    str.orig = 'NUM_ATOMS';
    str.change = [int2str(size(LD.pos,1))];
    str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    
    str.orig = 'LX';
    str.change = [num2str( LD.Nx*LD.cell(1) )];
    str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'LY';
    str.change = [num2str( LD.Ny*LD.cell(2) )];
    str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'LZ';
    str.change = [num2str( LD.Nz*LD.cell(3) )];
    str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    
    str.cmd5 =...
    ['si.xyz > lmp.in.x0'];

    str.cmd =...
        ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5 ];

    system(str.cmd);
    
%insert pos_tmp which has the dx displacement    
    output = [LD.id LD.type LD.pos_tmp(:,:)];
    str.write=strcat('./lmp.in.x0');
    dlmwrite(str.write,output,'-append','delimiter','\t',...
        'precision',strcat('%10.',LD.precision,'f'));
    
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
    str.cmd =...
        ['lmp_serial < in.force' ];
    system(str.cmd);
%--------------------------------------------------------------------------
%GREP FORCE----------------------------------------------------------------
%--------------------------------------------------------------------------
    str.str1 = 'grep -A '; 
	str.str2 = int2str(size(LD.pos,1));
	str.str3 = ' "ITEM: ATOMS fx fy fz " ';
	str.str4= strcat('./dump.force');
    str.str5 = strcat(' > ./lmp.force');
	str.str_cmd = [str.str1 str.str2 str.str3 str.str4 str.str5];
%--------------------------------------------------------------------------	
%pause
%--------------------------------------------------------------------------	
	[status,tmp] = system(str.str_cmd);

	str.str1 = 'sed ''s/ITEM: ATOMS fx fy fz //g'' ';
	str.str2 = strcat('./lmp.force');
	str.str3 = strcat(' > ./lmp.force2');
	str.str_cmd = [str.str1 str.str2 str.str3];
%--------------------------------------------------------------------------	
%pause
%--------------------------------------------------------------------------	
	[status,tmp] = system(str.str_cmd);

    fid=fopen(strcat('./lmp.force2'));
    dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
    fclose(fid);

	force(:,1) = dummy{1}(1:size(LD.pos,1),:);
	force(:,2) = dummy{2}(1:size(LD.pos,1),:); 
	force(:,3) = dummy{3}(1:size(LD.pos,1),:); 
%--------------------------------------------------------------------------	
%pause
%--------------------------------------------------------------------------	

end