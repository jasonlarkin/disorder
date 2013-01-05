################################################################################
#
#  mc2arc
#
################################################################################
F90    = $(RUNF90) $(OPT) $(FFLAGS) $(BAGGER) -c
F90O1  = $(RUNF90) $(OPT1) $(FFLAGS) $(BAGGER) -c

FOBJECTS= datatypes.o \
          mc2arc.o elements.o label.o uncell.o

OBJ = $(FOBJECTS) 

mc2arc : *.f *.F   ; /bin/sh getmachine mc2arc
mc2arc_: $(OBJ)    ; $(RUNF90) $(LDFLAGS) $(BAGGER) $(OBJ) $(LIBS) -o mc2arc
clean  :           ; /bin/sh getmachine clean
clean_debug:     ; /bin/sh getmachine -debug clean
clean_:          ; rm -f $(OBJ) ; rm ../gulp

datatypes.o    : ../datatypes.F       ; $(F90) ../datatypes.F
#
elements.o     : ../elements.f     ; $(F90) ../elements.f
mc2arc.o       : ../mc2arc.f       ; $(F90) ../mc2arc.f
label.o        : ../label.f        ; $(F90) ../label.f
uncell.o       : ../uncell.f       ; $(F90) ../uncell.f
