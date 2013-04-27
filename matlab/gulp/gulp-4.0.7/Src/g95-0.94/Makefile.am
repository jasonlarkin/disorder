
if GCC

bin_PROGRAMS = g95

extra_PROGRAMS = f951
extradir = @DEST_LIBDIR@

f951_SOURCES = arith.c array.c bbt.c bigint.c bigreal.c check.c		\
convert.c data.c decl.c dump.c entry.c error.c expr.c f95-lang.c	\
forall.c format.c g95.h g95-support.h imodule.c interface.c		\
intrinsic.c intrinsic.h io.c iresolve.c kinds.c lang-options.c match.c	\
matchexp.c misc.c module.c parse.c primary.c resolve.c scanner.c	\
scalarize.c select.c show.c simplify.c specs.h st.c symbol.c trans.h	\
trans-array.c trans-decl.c trans-intrinsic.c trans-stmt.c trans.c	\
trans-common.c trans-const.c trans-expr.c trans-io.c trans-types.c


f951_LDADD = options.o opts.o version.o ggc-page.o                      \
             @GCC_DIR@/g95/gcc/main.o @GCC_DIR@/g95/gcc/libbackend.a	\
             @GCC_DIR@/g95/libcpp/libcpp.a				\
	     @GCC_DIR@/g95/libiberty/libiberty.a			\
             @LDADD0@


g95_SOURCES = g95spec.c

g95_CFLAGS = -DTARGET_NAME=\"@host@\"					\
	     -DSTANDARD_EXEC_PREFIX=\"@DEST_LIBDIR@\"			\
	     -DTARGET_SYSTEM_ROOT=\"@TARGET_SYSTEM_ROOT@\"		\
	     -DDEFAULT_TARGET_VERSION=\"@DEFAULT_TARGET_VERSION@\"	\
	     -DDEFAULT_TARGET_MACHINE=\"@DEFAULT_TARGET_MACHINE@\"      \
	     -DTOOLDIR_BASE_PREFIX=\"\"					\
	     -DSTANDARD_STARTFILE_PREFIX=\"\"				\
	     -DSTANDARD_STARTFILE_PREFIX_2=\"@STARTFILE_PREFIX_2@\"	\
	     -DSTANDARD_EXEC_PREFIX=\"@DEST_LIBDIR@\"			\
	     -DSTANDARD_BINDIR_PREFIX=\"@PREFIX@/bin\"			\
	     -DSTANDARD_LIBEXEC_PREFIX=\"@DEST_LIBDIR@\"		\
	     -DG95_SPEC=\"@G95_SPEC@\"                              \
             @G95_CFLAGS@


g95_LDADD = @GCC_DIR@/g95/gcc/prefix.o gcc.o version.o			 \
	    @GCC_DIR@/g95/gcc/intl.o @GCC_DIR@/g95/libiberty/libiberty.a \
	    @LDADD0@

g95_DATA = @PARTS@ @GCC_DIR@/g95/gcc/libgcc.a

g95dir = @DEST_LIBDIR@

else

bin_PROGRAMS = g95

g95_SOURCES = arith.c array.c bbt.c bigint.c bigreal.c check.c decl.c	\
dump.c entry.c error.c expr.c forall.c format.c g95.h imodule.c		\
interface.c intrinsic.c intrinsic.h io.c iresolve.c kinds.c		\
lang-options.c match.c matchexp.c misc.c module.c parse.c primary.c	\
resolve.c scalarize.c scanner.c select.c show.c simplify.c st.c		\
symbol.c

endif

EXTRA_DIST = MANIFEST CONTRIB BUGS lang.opt autogen.sh G95Manual.pdf COPYING \
ggc-page40.c ggc-page41.c


gcc.o: @GCC_DIR@/gcc options.h
	$(CC) -c $(CFLAGS) $(g95_CFLAGS) -I. $(CPPFLAGS) @GCC_DIR@/gcc/gcc.c

# Build our own version.o

version.o: @GCC_DIR@/gcc/version.c gcc_version
	echo "const char version_string[] = \"`cat gcc_version` (g95 @VERSION@!) \" __DATE__;" > version.c
	echo 'const char bug_report_url[] = "http://www.g95.org or mail andyv@firstinter.net";' >> version.c

	$(CC) $(CFLAGS) -c version.c


clean-local:
	rm -f options.tmp options.c options.h version.c


install-exec-hook:
	mv $(DESTDIR)$(bindir)/g95$(EXEEXT) $(DESTDIR)$(bindir)/$(host)-g95$(EXEEXT)


install-data-hook:
	-ln -sf f951$(EXEEXT) $(DESTDIR)@DEST_LIBDIR@/cc1
	if test "`uname`" = "Darwin" ; then \
	  ar x /usr/lib/libgcc.a darwin-fpsave.o ; \
	  ar rs $(DESTDIR)@DEST_LIBDIR@/libgcc.a darwin-fpsave.o ; \
	  rm ./darwin-fpsave.o ; \
	  ranlib $(DESTDIR)@DEST_LIBDIR@/libgcc_eh.a ; \
	  ranlib $(DESTDIR)@DEST_LIBDIR@/libgcc.a ; \
	fi
	cp INSTALL $(DESTDIR)@DEST_LIBDIR@/../../../..
	cp G95Manual.pdf $(DESTDIR)@DEST_LIBDIR@/../../../..
	if test -n "`ls @GCC_DIR@/g95/gcc/libgcc_s.*`" ; then \
	   (cd @GCC_DIR@/g95/gcc; tar cf - libgcc_s.*) | \
           (cd $(DESTDIR)@DEST_LIBDIR@; tar xf -) ; \
	fi
	if test -n "`ls @GCC_DIR@/g95/gcc/libunwind.*`" ; then \
	   (cd @GCC_DIR@/g95/gcc; tar cf - libunwind.*) | \
	   (cd $(DESTDIR)@DEST_LIBDIR@; tar xf -) ; \
	fi
	if test -n "`ls @GCC_DIR@/g95/gcc/libgcc_eh.*`" ; then \
	   (cd @GCC_DIR@/g95/gcc; tar cf - libgcc_eh.*) | \
	   (cd $(DESTDIR)@DEST_LIBDIR@; tar xf -) ; \
	fi


ggc-page.o: $(srcdir)/ggc-page40.c $(srcdir)/ggc-page41.c
if GCC41
	$(CC) -c -g -o ggc-page.o -I. $(CPPFLAGS) -DIN_GCC $(srcdir)/ggc-page41.c
else
	$(CC) -c -g -o ggc-page.o -I. $(CPPFLAGS) -DIN_GCC $(srcdir)/ggc-page40.c
endif



options.h options.o: $(srcdir)/lang.opt
	$(AWK) -f @GCC_DIR@/gcc/opt-functions.awk \
	       -f @GCC_DIR@/gcc/opt-gather.awk    \
     @GCC_DIR@/gcc/common.opt @EXTRA_OPTS@ $(srcdir)/lang.opt > options.tmp

if GCC41
	$(AWK) -f @GCC_DIR@/gcc/opt-functions.awk \
	       -f @GCC_DIR@/gcc/optc-gen.awk \
	       -v header_name="config.h system.h coretypes.h tm.h" \
	          options.tmp > options.c
else
	$(AWK) -f @GCC_DIR@/gcc/opt-functions.awk \
	       -f @GCC_DIR@/gcc/optc-gen.awk options.tmp > options.c
endif

	$(AWK) -f @GCC_DIR@/gcc/opt-functions.awk \
	       -f @GCC_DIR@/gcc/opth-gen.awk options.tmp > options.h

	rm -f @GCC_DIR@/g95/gcc/options.h

	$(CC) -c $(CFLAGS) -I. $(CPPFLAGS) @GCC_DIR@/gcc/opts.c
	$(CC) -c $(CFLAGS) -I. $(CPPFLAGS) options.c
