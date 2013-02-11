function m_xyz2lmp(element,str)
varname = genvarname(element);
varname = 1;

a = load(str);

x=(1:size(a,1),1) = 1:size(a,1);
x(1:size(a,1),2)) = element;

x(1:512,1)=1:512;
x(1:512,2)=1;
x(1:512,3:5)=a(:,2:4);

end

