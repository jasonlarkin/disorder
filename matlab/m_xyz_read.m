function x0 = m_xyz_read(str_name)

fid = fopen(str_name);
x = textscan(fid, '%s%f%f%f','HeaderLines',2);
fclose(fid);
x0.atom = x{1}(:); x0.x = x{2}(:); x0.y = x{3}(:); x0.z = x{4}(:);
end