function m_change_file_strings( oldfilename, orig, newfilename, change)

fidin = fopen(oldfilename);
fidout = fopen(newfilename, 'w');

tline = fgets(fidin);
cnt=1;
while ischar(tline)
    str(cnt).line = (tline);
%make first change
    str(cnt).newline =...
    regexprep(str(cnt).line, orig(1).str, change(1).str);
%make the other changes
	for ichange = 2:size(orig,2)
	str(cnt).newline =...
    regexprep(str(cnt).newline, orig(ichange).str, change(ichange).str);
    end
    
fprintf(fidout,[str(cnt).newline]);

tline = fgets(fidin);
cnt = cnt+1;

end

fclose(fidin); fclose(fidout);

end
