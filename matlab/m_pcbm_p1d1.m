clear
str = '/home/jason/disorder/pcbm/pcbm_pdb/';

x0.start = m_x0_read([str 'p1d1_1a1b1c.data']);

x0.bond = m_xyz_read([str 'gulp_bond.xyz']);

olps = m_pcbm_olps;

[I.CF J.CF] = find( x0.start.m==olps.type.CF);
[I.CA1 J.CA1] = find( x0.start.m==olps.type.CA1);
[I.CA2 J.CA2] = find( x0.start.m==olps.type.CA2);
[I.C J.C] = find( x0.start.m==olps.type.C);
[I.CT1 J.CT1] = find( x0.start.m==olps.type.CT1);
[I.CT2 J.CT2] = find( x0.start.m==olps.type.CT2);
[I.CT3 J.CT3] = find( x0.start.m==olps.type.CT3);
[I.O J.O] = find( x0.start.m==olps.type.O);
[I.OS J.OS] = find( x0.start.m==olps.type.OS);
[I.HA J.HA] = find( x0.start.m==olps.type.HA);
[I.HC J.HC] = find( x0.start.m==olps.type.HC);

%c60
plot3(...
    x0.start.x(I.CF),x0.start.y(I.CF),x0.start.z(I.CF),'.',...
    x0.bond.x(I.CF),x0.bond.y(I.CF),x0.bond.z(I.CF),'.'...
    )
pause
plot3(...
    x0.start.x(I.CA1),x0.start.y(I.CA1),x0.start.z(I.CA1),'.',...
    x0.bond.x(I.CA1),x0.bond.y(I.CA1),x0.bond.z(I.CA1),'.'...
    )
pause
plot3(...
    x0.start.x(I.CA1),x0.start.y(I.CA1),x0.start.z(I.CA1),'.',...
    x0.bond.x(I.CA1),x0.bond.y(I.CA1),x0.bond.z(I.CA1),'.'...
    )


