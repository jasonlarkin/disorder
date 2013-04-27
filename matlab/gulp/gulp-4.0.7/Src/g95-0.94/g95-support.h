#ifndef F95_SUPPORT_H
#define F95_SUPPORT_H

tree convert_and_check PARAMS((tree, tree));
void overflow_warning PARAMS((tree));
void unsigned_conversion_warning PARAMS((tree, tree));
bool g95_mark_addressable PARAMS((tree));

void g95_init_c_decl_hacks(void);

#endif
