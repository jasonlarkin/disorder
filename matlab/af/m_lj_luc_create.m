function x0 = m_lj_luc_create( x0 )

if strcmp(x0.luc.type,'random')
    x0 = m_lj_luc_create_random( x0 );
else strcmp(x0.luc.type,'ordered')
end

end