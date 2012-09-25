function gilgamesh_qdel( id_start, id_stop)
%gilgamesh_qdel( id_start, id_stop)

for i=id_start:id_stop
str = ['qdel ' int2str(i)];
disp(str)
end

end
