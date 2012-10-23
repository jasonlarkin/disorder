function m_qdel(id_start,id_stop)
a='';
for i=id_start:id_stop
a = [a;'qdel ' int2str(i)]
end

end