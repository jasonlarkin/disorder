################################################################################
#
#  GULP4.0
#
# Makefile for GULP4.0, simply type make on OSX, AIX, IRIX, OSF1, HP-UX, SunOS 
# or Linux to build the programme. Type make gulp_debug to build programme with 
# debugging on, type make clean to remove all objects or make clean_debug to 
# remove all objects from version with debug information. Type make tar to 
# create gzipped tar file of source code, documentation and examples. Type make 
# html to convert help.txt to help.html.
#
#  Original makefile by J-R. Hill, adapted for GULP4.0 by JDG
#
#  MPI additions by Ivan Walton, HPCI Southampton
#
################################################################################

default: gulp

F90    = $(RUNF90) $(OPT) $(FFLAGS) $(BAGGER) $(DEFS) -c
F90FOX = $(RUNF90) $(OPT) $(FFLAGS) $(BAGGER) $(DEFS) -DFC_HAVE_ABORT -DFC_HAVE_FLUSH -c
F90O1  = $(RUNF90) $(OPT1) $(FFLAGS) $(BAGGER) -c
F90O2  = $(RUNF90) $(OPT2) $(FFLAGS) $(BAGGER) -c
F90O0  = $(RUNF90) $(FFLAGS) $(BAGGER) -c
CC     = $(RUNCC)  $(CFLAGS) $(BAGGER) $(PLUMEDDEFS) -c

COBJECTS= 
HOBJECT=  metadyn.h
LOBJECTS= $(LAPACK) $(BLAS)
POBJECTS= hills.o biasexchange.o restraint_alpharmsd.o restraint_energy.o restraint_rmsdtor.o \
          restraint_angle.o restraint_hbonds.o restraint_spath.o metadyn.o restraint_antibetarmsd.o \
          restraint_helix.o restraint_torsion.o restraint_coord.o restraint_mindist.o restraint_waterbridge.o \
          ptmetad.o restraint_dihcor.o restraint_parabetarmsd.o	restraint_zpath.o read_restraint.o \
          restraint_dipole.o restraint_position.o testderivatives.o restraint.o	restraint_dist.o restraint_puckering.o \
          restraint_alfabeta.o restraint_elstpot.o restraint_rgyr.o images.o
FOBJECTS= datatypes.o modules.o iomod.o itcom.o reallocate.o pr_random.o m_pr.o m_simplex.o m_conjgr.o \
          m_pdfvariables.o m_pdfneutron.o m_pdf.o gulpcml.o gulpcml_md.o gulpcml_phonon.o gulpcml_props.o \
          addframe2arc.o addshell.o angle.o aocn2.o banner.o bcgsolve.o binary.o bond.o bonded.o \
          bonded2donor.o bonded3.o bondpot.o boword.o bondorder.o cellcheck.o \
          bondordermd.o bondordersd2.o bondorder12.o bonded3c.o boself.o boselftrial.o bondq.o bondqtrial.o \
          bondint.o brenner.o brennermd.o brennersd2.o brenner12.o buck4.o cell.o cellfhr.o celltype.o centre.o \
          background.o borncharge.o calcpht.o celldrv.o channels.o changemaxat.o changemaxatot.o \
          changemaxatompernode.o changemaxcellpernode.o changemaxregion.o changemaxbondvec.o changemaxmcswapspec.o \
          changemaxnboa.o changemaxnbor.o changemaxbond.o changemaxcfg.o changemaxcon.o changemaxcontot.o changemaxpotsites.o \
          changemaxnbopot.o changemaxnboq.o changemaxconnect.o changemaxneamfnnumeric.o changemaxnebreplicatot.o \
          changemaxdcon.o changemaxdef.o changemaxdis.o changemaxfcon.o changemaxsymop.o changemaxccspec.o \
          changemaxfgrad.o changemaxfit.o changemaxfor.o changemaxkpt.o changemaxtrialatom.o changemaxbondq.o \
          changemaxfstress.o changemaxkvec.o changemaxlib.o changemaxlist3.o changemaxlist4.o changemaxlist6.o \
          changemaxmany.o changemaxmol.o changemaxnobo.o changemaxobs.o changemaxiltor.o changemaxfkpt.o changemaxskpt.o \
          changemaxone.o changemaxfreqat.o changemaxobsmode.o changemaxtdfield.o \
          changemaxpot.o changemaxppt.o changemaxproj.o changemaxproji.o changemaxqatoms.o changemaxqatoms2.o \
          changemaxpts.o changemaxr1at.o changemaxr2at.o changemaxspcell.o changemaxspec.o changemaxuffspec.o \
          changemaxeamspec.o changemaxeamfnspec.o changemaxthb.o changemaxtitle.o changemaxvacint.o changemaxvar.o \
          changemaxword.o changemaxd1.o changemaxd2.o changemaxnboq0.o changemaxsix.o \
          changemaxn3a.o changemaxd2q.o changemaxn3f.o changemaxeamden.o changemaxnspcellattot.o changemaxnspcellattotbo.o \
          changemaxgbcfg.o changemaxgcmcmol.o changemaxreaxffspec.o changemaxreaxffval3.o cart2frac.o closestimage.o changemaxreaxfffixqspec.o \
          changemaxatompernodebo.o changemaxcellpernodebo.o changemaxspcellbo.o changemaxplanepot.o changemaxvectorpair.o \
          changemaxndistancetotal.o changemaxextrapol.o checkdefshspec.o changemaxn3bondnono.o changemaxsa.o \
          changemaxhold.o changemaxqvector.o changemaxnqstepfit.o changemaxnwstepfit.o changemaxedipspec.o \
          copycfg.o chvo.o collect.o costfn.o cputime.o create.o cscheck.o defect.o defener.o \
          checkpot.o chemshellenergy.o connectwraptrial.o cutscheck.o \
          compressd1.o compressd2.o compressd3.o connectwrap.o connectunwrap.o datetime.o \
          defequ.o defequ2a.o deffreq.o deffun.o defoin.o defopt.o d2charge3p.o \
          defout.o defpot.o defsec.o defsym.o deftmat.o defword.o derfc.o destroy.o d1charge3.o d2charge3.o \
          density0.o density3.o density3s.o densitysd.o density2a.o density11s2.o density112.o \
          density12a2.o density12as2.o dynamicn_defect.o endstring.o \
          dcharge.o dcharges.o d1charge.o d1charges.o d2charge.o d2chargep.o d2charge2D.o d2chargep2D.o \
          diagdyn.o dipole3D.o dipole3Dtrial.o dipolesd.o distance.o dpoccon.o dspecial.o dump34.o dlamch.o \
          dumpbo.o dumpdur.o dumpgen.o dumppot.o dutils.o dynamic.o dynamicn.o eamfnderv.o eamscalescrho.o electricfieldtrial.o \
          eem.o electricfield.o einstein.o einsteintrial.o einsteinsd2.o eispack.o emfunc.o energy.o epot.o epslon.o equpos.o exchng.o \
          eamrho.o edipmd.o edipcn.o edip2body.o edip3body.o electricfieldpotl.o electricfieldparts.o \
          fefunct.o fefunctn.o fitfunct.o fenergy0.o fenergy3.o force.o forcetrial.o four0d3.o \
          four3d3.o fourbody.o fourmc.o fourmd.o fourmds.o finddef.o findrdcmax.o finish.o fit.o \
          fitbfgs.o fitcon.o firstpass.o fitfun.o fitgrad.o fitmin.o fitword.o \
          formula.o fournos.o four12.o fourlist.o fourp.o foursd.o \
          fouroop.o fouroopp.o fouroopmc.o fouroopmd.o fouroopmds.o fouroopsd.o fouroop12.o \
          fouroopsd2.o foursd2.o fourstrterms.o ftow.o functn.o functneb.o functsync.o functnf.o \
          funct.o gabcfg.o gacreate.o gacross.o gaexpd.o gafit.o gamaxmin.o gamuta.o gamma.o gammasm.o getvibmode.o \
          gaopt.o garepro.o gasort.o gasteiger.o gasubs.o gauss.o gaword.o genpot.o genword.o getkeywrd.o getcoordno.o \
          getbocharge.o getbochargetrial.o getbonds.o getderv1.o getdipole.o gtheta.o gthetabo.o gettraceinertia.o \
          getnmr.o getphi.o getneighbour.o getpotsymbol.o help.o hfunc.o lmatch.o lmatchany.o lmatchpair.o ind1toijk.o \
          indsft.o indtoijk.o initial.o init_ins.o intochar.o inword.o initarc.o initdervs.o initktrm.o init2bodydefaults.o \
          initqmatrix.o istoan.o itow.o kindex.o kvector.o kvectorf.o label.o labellc.o lambda.o init1bodydefaults.o \
          initmaxcfgdefaults.o  initmaxatdefaults.o initmaxatotdefaults.o initmaxbonddefaults.o \
          initmaxbondqdefaults.o initmaxccspecdefaults.o initmaxconnectdefaults.o initmaxdefdefaults.o \
          initmaxeamdendefaults.o initmaxeamfnspecdefaults.o initmaxeamspecdefaults.o initmaxfgraddefaults.o \
          initmaxfitdefaults.o initmaxfordefaults.o initmaxfstressdefaults.o initmaxgcmcmoldefaults.o \
          initmaxlibdefaults.o initmaxmcswapspec.o initmaxmoldefaults.o initmaxnboadefaults.o initmaxnbopotdefaults.o \
          initmaxnboq0defaults.o initmaxnboqdefaults.o initmaxnbordefaults.o initmaxnebreplicatotdefaults.o \
          initmaxnppadefaults.o initmaxnptsdefaults.o initmaxobsdefaults.o initmaxplanepotdefaults.o \
          initmaxr1atdefaults.o initmaxreaxffspecdefaults.o initmaxreaxffval3defaults.o initmaxregiondefaults.o \
          initmaxsixdefaults.o initmaxspcellbodefaults.o initmaxspcelldefaults.o initmaxspecdefaults.o \
          initmaxthbdefaults.o initmaxtitledefaults.o initmaxedipspecdefaults.o insertatom.o \
          latpot.o lmbfgs.o linepro.o linpack.o lintoijk.o local.o lower.o \
          ltont.o matinv.o mdcellind.o mdcorrect.o mdfunct.o mdpredict.o \
          many.o manysd.o many0d.o many12.o manyp.o mdke.o mdkeatomicstress.o mdshscale.o \
          manysd2.o manymd0.o manymd3.o manymd3s.o manymi3.o mcstorecfg.o mcenergy.o \
          mccreate.o mctrialfunction.o mcdestroy.o mcmolinsert.o mcmove.o mcrotate.o mcstrain.o mcswap.o mddistconpost.o \
          mdgcorrect.o mdvvcorrect.o mdlfvcorrect.o mdkestrain.o mdtemperature.o \
          mdprop.o mdreset.o mdrestart.o mdscale.o mdstop.o mdvelcor.o \
          mcword.o mdword.o mdwrite.o methodok.o mindtoijk.o minimise.o molind.o molindfix.o meamsymdensity.o meamscreen.o \
          meamfnderv.o meamrho.o meamtotalrho.o meamtotalrhoderv.o meamtotalrhocrossderv.o molindtrial.o molindfixtrial.o \
          meamtotalrhoscreenderv.o moltype.o move2a1.o mxmb.o nagamma.o nearest.o nebforce.o nebword.o nrhess.o nrstep.o numhess.o \
          nudge.o okspec.o optim.o optin.o options.o optout.o optshe.o outerror.o outadvice.o \
          omegaproperty.o oscillatorstrength.o oscillatorstrengthg.o olinmin.o outcell.o outdcdheader.o \
          outarc.o outdefin.o outdener.o outderv.o outdlv.o outdrv.o outed.o outener.o outfdf.o outfile.o \
          outfrc.o outfit.o outfitcon.o outfreq.o outgafit.o outgaopt.o outgapar.o outneb.o outnebin.o \
          outsync.o outsyncin.o outspec_pdf.o onebody.o outlammps.o outlammpspots.o outatomicstress.o \
          outgen.o outkey.o outkpt.o outmarv.o outmarv2.o outmc.o outmd.o outmdin.o outmol.o \
          outobs.o outphon.o outpolar.o outpot.o outspec.o outthb.o outtime.o outbiograf.o outqbo.o \
          outvar.o outvarg.o outwarning.o outxr.o outxtl.o outxyz.o peigen.o phonon.o phonword.o poccon.o poccons.o \
          outcif.o outcssr.o outofp.o pireal0d.o pirealrecip.o pirealrecips.o outstructure.o outstresses.o \
          pdiag.o pdiagg.o pdiage.o pdiagl.o psumall.o polarisation.o potgrid.o phoncopy1.o phoncopy2.o potwordm.o \
          potcut.o potential.o potword21.o potword3.o potword4.o potword6.o pressure.o planepot2D.o \
          potword22.o peigeng.o projd0.o projd3a.o projthbk3ds.o projfork0.o planepotmc.o planepotmd.o \
          predict.o projthbk0.o projthbk3.o projthbk3s.o projthbk3d.o pr_integrator.o \
          projfork3.o projfork3s.o projfork3d.o projfork3ds.o property.o property0.o putpar.o pythag.o \
          qdupdate.o qeqbody.o qmatrix1D.o qmatrixelement.o qupdate.o radialforce.o radialforcetrial.o real1Dmc.o \
          random.o ratfn.o real0d.o real1D.o real1Dmd.o real1Dp.o real1Dq.o real11.o real11pot.o real11s.o real12a.o \
          real12apot.o real12as.o real2a.o real2a1.o real2a1f.o real2adef.o \
          real2amany.o real12a2.o real12as2.o real112.o real11s2.o realmc0.o \
          real2aef.o real2b.o real4.o realcmm.o realmc3.o realmd3.o realmd3s.o realmd3t.o realmd0.o reale.o realp.o \
          realmi3.o realmimc3.o realrecip3d3.o real0d3.o real1d3.o ramantrm.o reciptrmd3.o reciptrmdp.o \
          realsd.o realsd2.o recip2D.o recip2Dtrial.o recip3D.o recip3Dmd.o recip3Dtrial.o recip12a.o recip12apot.o recip12as.o \
          real2Dq.o recip2Dq.o reaxffmd.o reaxff_bo.o reaxff_funcs.o rebospline.o rfind.o rfindboth.o \
          recip4.o recip2Dp.o recip3Dp.o recipsd.o recipsd2.o remove.o reorder.o resetcfgdefaults.o \
          rfohess.o rfostep.o rhoinv.o rhtohex.o rlist.o rpair.o rpair2.o runsync.o reinitialise.o \
          reaxffpot.o rsearch1D.o rsearch2D.o rsearch3D.o rtlist.o runmc.o runmd.o runneb.o samemol.o \
          setcutoffmax.o \
          setbelashchenko.o setcutoffmaxbo.o setspatialbo.o setatomnodesbo.o setspatialcells.o setedip.o \
          sec0.o sec0f.o sec3.o sec3f.o selfterm.o set2a.o setbrenner1.o setbrenner3.o setc6.o setcfg.o setcmm.o \
          setcellderv.o seteam.o setratiom.o setreaxffQ.o setreaxffQiter.o setregiontrans.o setewald.o \
          setdef.o setdisp.o setele.o setfour.o setkpt.o setsix.o setuff.o \
          setatomnodes.o setbondorder.o setktrm4.o setktrmdp.o setlenn.o setlib.o setlist3.o setlist4.o setlist6.o \
          setmaxcell1D.o setmd.o setmol.o setmols.o setmoldef.o setmoldef12a.o settorcubic.o settaper.o secondpass.o \
          setpote.o setreaxFF.o setslice.o setspec.o setspring.o settricubic.o settricubic2.o sumderv2f.o sumderv1.o \
          setbsmptr.o setdr2mat.o setktrmd3.o setlistoop.o setoccptr.o setoccptrs.o setoccshptr.o setpolar.o setspatial.o \
          getdistances.o setdistance.o smbody.o setvbon.o symdervrot.o setoccshptrs.o \
          setthree.o settings.o setup.o setzmolslice.o sort.o sort1sym.o sort2a.o setstrainptr.o \
          setfeshtmat.o setquasiharm.o scaleevec.o setdestroyptr.o setmoveptr.o setrotateptr.o setkeywrd.o \
          setswapptr.o sitepot.o special.o speclibw.o specword.o spline.o stolc.o stouc.o surfword.o \
          sixbody.o sixlist.o sixmc.o sixmd.o sixmds.o sixnos.o sixp.o sixsd.o sixsd2.o sixstrterms.o \
          strain.o strfin.o strsym.o strword.o sumderv2.o sumderv2s.o surfaceenergy.o syncforce.o \
          super.o symbind.o symderv2.o symmet.o symoff.o symout.o sympot.o symprod.o tfunc.o thermalconductivity.o \
          taper.o threestrterms.o threenos.o three12.o threelist.o threep.o threemc.o threemd.o threemds.o threesd.o \
          threesd2.o threebody.o three0d3.o three3d3.o torsion.o transmat.o twobody.o twobodymd.o twoden.o \
          uncell.o uncentre.o unfreeze.o validtriad.o volume.o wcoord.o wolfself.o wolfselftrial.o kweight.o worsy.o wtof.o \
          x0tostrtrial.o x0strain.o x0tostr.o x0tostrcentroid.o xctox0.o $(CDABS) \
	  getline.o initcomms.o sumall.o stopnow.o mpfinish.o mpbarrier.o \
          initmemory.o nullpointer.o outofmemory.o \
          iterinv.o itpack.o \
          gulp.o gulpmain.o gulpsetup.o gulpfinish.o \
          changemaxatcosmo.o cosmoaiiderv.o cosmoaijderv.o cosmoaijzero.o cosmoamatdadd.o cosmoderv.o \
          dcosmowt.o dtotcosmowt.o setcosmo.o setcosmoainv.o setcosmoamat.o setcosmobmat.o \
          setdcosmoamat.o setdcosmobmat.o changemaxnearseg.o changemaxnppa.o changemaxnpts.o \
          changemaxnptsh.o changemaxnptstot.o changemaxnpwt.o changemaxsasparticles.o \
          changemaxsasparticlespart.o setsegweight.o setdsegweight.o setqonsas.o setsas.o \
          updatesas.o solvation.o switch.o cosmoaiizero.o changemaxnset.o createsphere.o \
          initqmatrixc.o qmatrixelementc.o changemaxallnearseg.o \
          qtpie.o ctfunct.o qtmin.o screenct.o reaxffctfunct.o \
	  trap_sigint.o csignal.o

CSDUM = CS_dummy.o

FOBJ = $(FOBJECTS) $(LOBJECTS)
#
#  For PLUMED use the following line:
#
#OBJ  = $(FOBJ) $(COBJECTS) $(POBJECTS)
#
#  else use this one: 
#
OBJ  = $(FOBJ) $(COBJECTS) 

#
# Dependencies
#
$(POBJECT): $(HOBJECT)

#
#  For PLUMED use the following line:
#
#gulp : *.f90 *.F90 Plumed/*.c ; /bin/sh getmachine gulp
#
#  else use this one: 
#
gulp : *.f90 *.F90 ; /bin/sh getmachine gulp
gulp_debug:          ; /bin/sh getmachine -debug gulp
gulp_: $(OBJ) $(CSDUM) ; $(RUNF90) $(LDFLAGS) $(BAGGER) $(OBJ) $(CSDUM) $(LIBS) $(FoX_libs) -o gulp

lib:    ../libgulp.a
../libgulp.a :  *.f90 *.F90   ; /bin/sh getmachine lib
lib_:   $(OBJ)
	$(ARCHIVE) libgulp.a $(OBJ)
	$(RANLIB) libgulp.a

clean:      
	(cd FoX-4.1.2; touch arch.make; make distclean)         
	/bin/sh getmachine clean
clean_debug: 
	(cd FoX-4.1.2; touch arch.make; make distclean)         
	/bin/sh getmachine -debug clean
libclean: 
	(cd FoX-4.1.2; touch arch.make; make distclean)
	/bin/sh getmachine libclean
clean_:          ; rm -f $(OBJ) $(CSDUM) *.mod ; rm -f ../gulp
libclean_:       ; rm -f $(OBJ) *.mod ; rm -f ../../libgulp.a
html:            ; gulp2html help.txt > help.html
tar  :           ; cd .. ; tar -cvf gulp4.0.tar README Src/*.f90 Src/*.F90 Src/*.c \
                   Src/Makefile Examples/README Examples/example*.gin Examples/*.dbout Libraries/eledata Src/getmachine \
                   Libraries/*.lib \
                   Utils/MC2arc/*.f Utils/MC2arc/*.F Utils/MC2arc/README \
                   Utils/MC2arc/Makefile Utils/MC2arc/getmachine Utils/ReaxFF/reaxff2lib.f90 \
                   Utils/MDarc2his/arc2his.f90 Utils/MDarc2his/cell.f90 Utils/MDarc2his/README \
                   Utils/MDbin2ascii/README Utils/MDbin2ascii/mdbin2ascii.f90 \
                   Docs/gulp.intro Docs/gulp2html Docs/help.txt Docs/gulp4.0_manual.pdf \
                   Src/FoX-4.1.2/LICENSE Src/FoX-4.1.2/configure Src/FoX-4.1.2/arch.make.in Src/FoX-4.1.2/Makefile Src/FoX-4.1.2/README.FoX.txt \
                   Src/FoX-4.1.2/FoX-config.in Src/FoX-4.1.2/config/config.* Src/FoX-4.1.2/config/install-sh Src/FoX-4.1.2/utils/*.f90 \
                   Src/FoX-4.1.2/utils/*.F90 Src/FoX-4.1.2/utils/makefile \
                   Src/FoX-4.1.2/fsys/*.f90 Src/FoX-4.1.2/fsys/*.F90 Src/FoX-4.1.2/fsys/makefile Src/FoX-4.1.2/common/*.F90 Src/FoX-4.1.2/common/makefile \
                   Src/FoX-4.1.2/wxml/*.f90 Src/FoX-4.1.2/wxml/*.F90 Src/FoX-4.1.2/wxml/makefile Src/FoX-4.1.2/wcml/*.f90 Src/FoX-4.1.2/wcml/*.F90 \
                   Src/FoX-4.1.2/wcml/makefile; \
                   gzip -9 gulp4.0.tar

## FoX config and making
FOX_PATH = ../FoX-4.1.2
FOX_CONFIG = $(FOX_PATH)/FoX-config
FOX_OBJ = $(FOX_PATH)/wcml/FoX_wcml.o

$(FOX_CONFIG):
ifeq (,$(findstring DNOFOX,$(DEFS)))
	(cd $(FOX_PATH); ./configure --enable-wcml --enable-wxml FC='$(RUNF90)' FCFLAGS='$(OPT) $(FFLAGS) $(BAGGER)' LDFLAGS='$(LDFLAGS) $(BAGGER) $(LIBS)')
else
	@echo 'WARNING - configuring FoX in dummy mode'
	(cd $(FOX_PATH); ./configure --enable-dummy FC=$(RUNF90) FCFLAGS='$(OPT) $(FFLAGS) $(BAGGER)' LDFLAGS='$(LDFLAGS) $(BAGGER) $(LIBS)')
endif

$(FOX_OBJ): $(FOX_CONFIG)
ifeq (,$(findstring DNOFOX,$(DEFS)))
	(cd $(FOX_PATH);make wxml_lib wcml_lib)
else
	@echo 'WARNING - building FoX dummy lib'
	(cd $(FOX_PATH);make)
endif

FoX_fcflags=`$(FOX_PATH)/FoX-config --fcflags --wcml --wxml`
FoX_libs=`$(FOX_PATH)/FoX-config --libs --wcml --wxml`
## End of FoX stuff

# .F90 files
banner.o                  : ../banner.F90                  ; $(F90) ../banner.F90
channels.o                : ../channels.F90                ; $(F90) ../channels.F90
chemshellenergy.o         : ../chemshellenergy.F90         ; $(F90) ../chemshellenergy.F90
cputime.o                 : ../cputime.F90                 ; $(F90) ../cputime.F90
getline.o                 : ../getline.F90                 ; $(F90) ../getline.F90
datatypes.o               : ../datatypes.F90               ; $(F90) ../datatypes.F90
gulp.o                    : ../gulp.F90                    ; $(F90) ../gulp.F90
gulpmain.o                : ../gulpmain.F90                ; $(F90) $(FoX_fcflags) ../gulpmain.F90
gulpsetup.o               : ../gulpsetup.F90               ; $(F90) $(FoX_fcflags) ../gulpsetup.F90
initcomms.o               : ../initcomms.F90               ; $(F90) ../initcomms.F90
initdervs.o               : ../initdervs.F90               ; $(F90) ../initdervs.F90
initial.o                 : ../initial.F90                 ; $(F90) ../initial.F90
local.o                   : ../local.F90                   ; $(F90) ../local.F90
mdfunct.o                 : ../mdfunct.F90                 ; $(F90) ../mdfunct.F90
minimise.o                : ../minimise.F90                ; $(F90) $(FoX_fcflags) ../minimise.F90
mpbarrier.o               : ../mpbarrier.F90               ; $(F90) ../mpbarrier.F90
mpfinish.o                : ../mpfinish.F90                ; $(F90) ../mpfinish.F90
runmd.o                   : ../runmd.F90                   ; $(F90) $(FoX_fcflags) ../runmd.F90
stopnow.o                 : ../stopnow.F90                 ; $(F90) ../stopnow.F90
sumall.o                  : ../sumall.F90                  ; $(F90) ../sumall.F90
CS_dummy.o                : ../CS_dummy.F90                ; $(F90) ../CS_dummy.F90
# .f90 files
modules.o                 : ../modules.f90                 ; $(F90) ../modules.f90
iomod.o                   : ../iomod.f90                   ; $(F90) ../iomod.f90
pr_random.o               : ../pr_random.f90               ; $(F90) ../pr_random.f90
m_pr.o                    : ../m_pr.f90                    ; $(F90) ../m_pr.f90
addframe2arc.o            : ../addframe2arc.f90            ; $(F90) ../addframe2arc.f90
addshell.o                : ../addshell.f90                ; $(F90) ../addshell.f90
angle.o                   : ../angle.f90                   ; $(F90) ../angle.f90
aocn2.o                   : ../aocn2.f90                   ; $(F90) ../aocn2.f90
background.o              : ../background.f90              ; $(F90) ../background.f90
bcgsolve.o                : ../bcgsolve.f90                ; $(F90) ../bcgsolve.f90
binary.o                  : ../binary.f90                  ; $(F90) ../binary.f90
blas.o                    : ../blas.f90                    ; $(F90) ../blas.f90
bond.o                    : ../bond.f90                    ; $(F90) ../bond.f90
bonded.o                  : ../bonded.f90                  ; $(F90) ../bonded.f90
bonded2donor.o            : ../bonded2donor.f90            ; $(F90) ../bonded2donor.f90
bonded3.o                 : ../bonded3.f90                 ; $(F90) ../bonded3.f90
bonded3c.o                : ../bonded3c.f90                ; $(F90) ../bonded3c.f90
bondint.o                 : ../bondint.f90                 ; $(F90) ../bondint.f90
bondorder.o               : ../bondorder.f90               ; $(F90) ../bondorder.f90
bondorder12.o             : ../bondorder12.f90             ; $(F90) ../bondorder12.f90
bondordermd.o             : ../bondordermd.f90             ; $(F90) ../bondordermd.f90
bondordersd2.o            : ../bondordersd2.f90            ; $(F90) ../bondordersd2.f90
bondpot.o                 : ../bondpot.f90                 ; $(F90) ../bondpot.f90
bondq.o                   : ../bondq.f90                   ; $(F90) ../bondq.f90
bondqtrial.o              : ../bondqtrial.f90              ; $(F90) ../bondqtrial.f90
borncharge.o              : ../borncharge.f90              ; $(F90) $(FoX_fcflags) ../borncharge.f90
boself.o                  : ../boself.f90                  ; $(F90) ../boself.f90
boselftrial.o             : ../boselftrial.f90             ; $(F90) ../boselftrial.f90
boword.o                  : ../boword.f90                  ; $(F90) ../boword.f90
brenner.o                 : ../brenner.f90                 ; $(F90) ../brenner.f90
brenner12.o               : ../brenner12.f90               ; $(F90) ../brenner12.f90
brennermd.o               : ../brennermd.f90               ; $(F90) ../brennermd.f90
brennersd2.o              : ../brennersd2.f90              ; $(F90) ../brennersd2.f90
buck4.o                   : ../buck4.f90                   ; $(F90) ../buck4.f90
calcpht.o                 : ../calcpht.f90                 ; $(F90) ../calcpht.f90
cdabs.o                   :                                ; $(F90) ../cdabs.f90
cell.o                    : ../cell.f90                    ; $(F90) ../cell.f90
cellcheck.o               : ../cellcheck.f90               ; $(F90) ../cellcheck.f90
celldrv.o                 : ../celldrv.f90                 ; $(F90) ../celldrv.f90
cellfhr.o                 : ../cellfhr.f90                 ; $(F90) ../cellfhr.f90
celltype.o                : ../celltype.f90                ; $(F90) ../celltype.f90
centre.o                  : ../centre.f90                  ; $(F90) ../centre.f90
cart2frac.o               : ../cart2frac.f90               ; $(F90) ../cart2frac.f90
changemaxat.o             : ../changemaxat.f90             ; $(F90) ../changemaxat.f90
changemaxatcosmo.o        : ../changemaxatcosmo.f90        ; $(F90) ../changemaxatcosmo.f90
changemaxatot.o           : ../changemaxatot.f90           ; $(F90) ../changemaxatot.f90
changemaxatompernode.o    : ../changemaxatompernode.f90    ; $(F90) ../changemaxatompernode.f90
changemaxatompernodebo.o  : ../changemaxatompernodebo.f90  ; $(F90) ../changemaxatompernodebo.f90
changemaxbond.o           : ../changemaxbond.f90           ; $(F90) ../changemaxbond.f90
changemaxbondq.o          : ../changemaxbondq.f90          ; $(F90) ../changemaxbondq.f90
changemaxbondvec.o        : ../changemaxbondvec.f90        ; $(F90) ../changemaxbondvec.f90
changemaxccspec.o         : ../changemaxccspec.f90         ; $(F90) ../changemaxccspec.f90
changemaxcellpernode.o    : ../changemaxcellpernode.f90    ; $(F90) ../changemaxcellpernode.f90
changemaxcellpernodebo.o  : ../changemaxcellpernodebo.f90  ; $(F90) ../changemaxcellpernodebo.f90
changemaxcfg.o            : ../changemaxcfg.f90            ; $(F90) ../changemaxcfg.f90
changemaxcon.o            : ../changemaxcon.f90            ; $(F90) ../changemaxcon.f90
changemaxconnect.o        : ../changemaxconnect.f90        ; $(F90) ../changemaxconnect.f90
changemaxcontot.o         : ../changemaxcontot.f90         ; $(F90) ../changemaxcontot.f90
changemaxd1.o             : ../changemaxd1.f90             ; $(F90) ../changemaxd1.f90
changemaxd2.o             : ../changemaxd2.f90             ; $(F90) ../changemaxd2.f90
changemaxd2q.o            : ../changemaxd2q.f90            ; $(F90) ../changemaxd2q.f90
changemaxdcon.o           : ../changemaxdcon.f90           ; $(F90) ../changemaxdcon.f90
changemaxdef.o            : ../changemaxdef.f90            ; $(F90) ../changemaxdef.f90
changemaxdis.o            : ../changemaxdis.f90            ; $(F90) ../changemaxdis.f90
changemaxeamden.o         : ../changemaxeamden.f90         ; $(F90) ../changemaxeamden.f90
changemaxeamfnspec.o      : ../changemaxeamfnspec.f90      ; $(F90O1) ../changemaxeamfnspec.f90
changemaxeamspec.o        : ../changemaxeamspec.f90        ; $(F90) ../changemaxeamspec.f90
changemaxedipspec.o       : ../changemaxedipspec.f90       ; $(F90) ../changemaxedipspec.f90
changemaxextrapol.o       : ../changemaxextrapol.f90       ; $(F90) ../changemaxextrapol.f90
changemaxfcon.o           : ../changemaxfcon.f90           ; $(F90) ../changemaxfcon.f90
changemaxfgrad.o          : ../changemaxfgrad.f90          ; $(F90) ../changemaxfgrad.f90
changemaxfstress.o        : ../changemaxfstress.f90        ; $(F90) ../changemaxfstress.f90
changemaxfit.o            : ../changemaxfit.f90            ; $(F90) ../changemaxfit.f90
changemaxfor.o            : ../changemaxfor.f90            ; $(F90) ../changemaxfor.f90
changemaxfreqat.o         : ../changemaxfreqat.f90         ; $(F90) ../changemaxfreqat.f90
changemaxgbcfg.o          : ../changemaxgbcfg.f90          ; $(F90) ../changemaxgbcfg.f90
changemaxgcmcmol.o        : ../changemaxgcmcmol.f90        ; $(F90) ../changemaxgcmcmol.f90
changemaxhold.o           : ../changemaxhold.f90           ; $(F90) ../changemaxhold.f90
changemaxiltor.o          : ../changemaxiltor.f90          ; $(F90) ../changemaxiltor.f90
changemaxkpt.o            : ../changemaxkpt.f90            ; $(F90) ../changemaxkpt.f90
changemaxfkpt.o           : ../changemaxfkpt.f90           ; $(F90) ../changemaxfkpt.f90
changemaxskpt.o           : ../changemaxskpt.f90           ; $(F90) ../changemaxskpt.f90
changemaxkvec.o           : ../changemaxkvec.f90           ; $(F90) ../changemaxkvec.f90
changemaxlib.o            : ../changemaxlib.f90            ; $(F90) ../changemaxlib.f90
changemaxlist3.o          : ../changemaxlist3.f90          ; $(F90) ../changemaxlist3.f90
changemaxlist4.o          : ../changemaxlist4.f90          ; $(F90) ../changemaxlist4.f90
changemaxlist6.o          : ../changemaxlist6.f90          ; $(F90) ../changemaxlist6.f90
changemaxmany.o           : ../changemaxmany.f90           ; $(F90) ../changemaxmany.f90
changemaxmcswapspec.o     : ../changemaxmcswapspec.f90     ; $(F90) ../changemaxmcswapspec.f90
changemaxmol.o            : ../changemaxmol.f90            ; $(F90) ../changemaxmol.f90
changemaxn3a.o            : ../changemaxn3a.f90            ; $(F90) ../changemaxn3a.f90
changemaxn3f.o            : ../changemaxn3f.f90            ; $(F90) ../changemaxn3f.f90
changemaxn3bondnono.o     : ../changemaxn3bondnono.f90     ; $(F90) ../changemaxn3bondnono.f90
changemaxallnearseg.o     : ../changemaxallnearseg.f90     ; $(F90) ../changemaxallnearseg.f90
changemaxnbopot.o         : ../changemaxnbopot.f90         ; $(F90) ../changemaxnbopot.f90
changemaxnboa.o           : ../changemaxnboa.f90           ; $(F90) ../changemaxnboa.f90
changemaxnboq.o           : ../changemaxnboq.f90           ; $(F90) ../changemaxnboq.f90
changemaxnboq0.o          : ../changemaxnboq0.f90          ; $(F90) ../changemaxnboq0.f90
changemaxnbor.o           : ../changemaxnbor.f90           ; $(F90) ../changemaxnbor.f90
changemaxndistancetotal.o : ../changemaxndistancetotal.f90 ; $(F90) ../changemaxndistancetotal.f90
changemaxneamfnnumeric.o  : ../changemaxneamfnnumeric.f90  ; $(F90) ../changemaxneamfnnumeric.f90
changemaxnearseg.o        : ../changemaxnearseg.f90        ; $(F90) ../changemaxnearseg.f90
changemaxnebreplicatot.o  : ../changemaxnebreplicatot.f90  ; $(F90) ../changemaxnebreplicatot.f90
changemaxnset.o           : ../changemaxnset.f90           ; $(F90) ../changemaxnset.f90
changemaxnspcellattot.o   : ../changemaxnspcellattot.f90   ; $(F90) ../changemaxnspcellattot.f90
changemaxnspcellattotbo.o : ../changemaxnspcellattotbo.f90 ; $(F90) ../changemaxnspcellattotbo.f90
changemaxnobo.o           : ../changemaxnobo.f90           ; $(F90) ../changemaxnobo.f90
changemaxnppa.o           : ../changemaxnppa.f90           ; $(F90) ../changemaxnppa.f90
changemaxnpwt.o           : ../changemaxnpwt.f90           ; $(F90) ../changemaxnpwt.f90
changemaxnpts.o           : ../changemaxnpts.f90           ; $(F90) ../changemaxnpts.f90
changemaxnptsh.o          : ../changemaxnptsh.f90          ; $(F90) ../changemaxnptsh.f90
changemaxnptstot.o        : ../changemaxnptstot.f90        ; $(F90) ../changemaxnptstot.f90
changemaxnqstepfit.o      : ../changemaxnqstepfit.f90      ; $(F90) ../changemaxnqstepfit.f90
changemaxnwstepfit.o      : ../changemaxnwstepfit.f90      ; $(F90) ../changemaxnwstepfit.f90
changemaxobs.o            : ../changemaxobs.f90            ; $(F90) ../changemaxobs.f90
changemaxobsmode.o        : ../changemaxobsmode.f90        ; $(F90) ../changemaxobsmode.f90
changemaxone.o            : ../changemaxone.f90            ; $(F90) ../changemaxone.f90
changemaxplanepot.o       : ../changemaxplanepot.f90       ; $(F90) ../changemaxplanepot.f90
changemaxpot.o            : ../changemaxpot.f90            ; $(F90) ../changemaxpot.f90
changemaxpotsites.o       : ../changemaxpotsites.f90       ; $(F90) ../changemaxpotsites.f90
changemaxppt.o            : ../changemaxppt.f90            ; $(F90) ../changemaxppt.f90
changemaxproj.o           : ../changemaxproj.f90           ; $(F90) ../changemaxproj.f90
changemaxproji.o          : ../changemaxproji.f90          ; $(F90) ../changemaxproji.f90
changemaxpts.o            : ../changemaxpts.f90            ; $(F90) ../changemaxpts.f90
changemaxqatoms.o         : ../changemaxqatoms.f90         ; $(F90) ../changemaxqatoms.f90
changemaxqatoms2.o        : ../changemaxqatoms2.f90        ; $(F90) ../changemaxqatoms2.f90
changemaxqvector.o        : ../changemaxqvector.f90        ; $(F90) ../changemaxqvector.f90
changemaxr1at.o           : ../changemaxr1at.f90           ; $(F90) ../changemaxr1at.f90
changemaxr2at.o           : ../changemaxr2at.f90           ; $(F90) ../changemaxr2at.f90
changemaxreaxfffixqspec.o : ../changemaxreaxfffixqspec.f90 ; $(F90) ../changemaxreaxfffixqspec.f90
changemaxreaxffspec.o     : ../changemaxreaxffspec.f90     ; $(F90) ../changemaxreaxffspec.f90
changemaxreaxffval3.o     : ../changemaxreaxffval3.f90     ; $(F90) ../changemaxreaxffval3.f90
changemaxregion.o         : ../changemaxregion.f90         ; $(F90) ../changemaxregion.f90
changemaxsa.o             : ../changemaxsa.f90             ; $(F90) ../changemaxsa.f90
changemaxsasparticles.o   : ../changemaxsasparticles.f90   ; $(F90) ../changemaxsasparticles.f90
changemaxsasparticlespart.o : ../changemaxsasparticlespart.f90 ; $(F90) ../changemaxsasparticlespart.f90
changemaxsix.o            : ../changemaxsix.f90            ; $(F90) ../changemaxsix.f90
changemaxspcell.o         : ../changemaxspcell.f90         ; $(F90) ../changemaxspcell.f90
changemaxspcellbo.o       : ../changemaxspcellbo.f90       ; $(F90) ../changemaxspcellbo.f90
changemaxspec.o           : ../changemaxspec.f90           ; $(F90) ../changemaxspec.f90
changemaxsymop.o          : ../changemaxsymop.f90          ; $(F90) ../changemaxsymop.f90
changemaxtdfield.o        : ../changemaxtdfield.f90        ; $(F90) ../changemaxtdfield.f90
changemaxthb.o            : ../changemaxthb.f90            ; $(F90) ../changemaxthb.f90
changemaxtitle.o          : ../changemaxtitle.f90          ; $(F90) ../changemaxtitle.f90
changemaxtrialatom.o      : ../changemaxtrialatom.f90      ; $(F90) ../changemaxtrialatom.f90
changemaxuffspec.o        : ../changemaxuffspec.f90        ; $(F90) ../changemaxuffspec.f90
changemaxvacint.o         : ../changemaxvacint.f90         ; $(F90) ../changemaxvacint.f90
changemaxvar.o            : ../changemaxvar.f90            ; $(F90) ../changemaxvar.f90
changemaxvectorpair.o     : ../changemaxvectorpair.f90     ; $(F90) ../changemaxvectorpair.f90
changemaxword.o           : ../changemaxword.f90           ; $(F90) ../changemaxword.f90
checkpot.o                : ../checkpot.f90                ; $(F90) ../checkpot.f90
checkdefshspec.o          : ../checkdefshspec.f90          ; $(F90) ../checkdefshspec.f90
chvo.o                    : ../chvo.f90                    ; $(F90) ../chvo.f90
closestimage.o            : ../closestimage.f90            ; $(F90) ../closestimage.f90
collect.o                 : ../collect.f90                 ; $(F90) ../collect.f90
compressd1.o              : ../compressd1.f90              ; $(F90) ../compressd1.f90
compressd2.o              : ../compressd2.f90              ; $(F90) ../compressd2.f90
compressd3.o              : ../compressd3.f90              ; $(F90) ../compressd3.f90
connectwrap.o             : ../connectwrap.f90             ; $(F90) ../connectwrap.f90
connectwraptrial.o        : ../connectwraptrial.f90        ; $(F90) ../connectwraptrial.f90
connectunwrap.o           : ../connectunwrap.f90           ; $(F90) ../connectunwrap.f90
copycfg.o                 : ../copycfg.f90                 ; $(F90) ../copycfg.f90
cosmoaiiderv.o            : ../cosmoaiiderv.f90            ; $(F90) ../cosmoaiiderv.f90
cosmoaijderv.o            : ../cosmoaijderv.f90            ; $(F90) ../cosmoaijderv.f90
cosmoaiizero.o            : ../cosmoaiizero.f90            ; $(F90) ../cosmoaiizero.f90
cosmoaijzero.o            : ../cosmoaijzero.f90            ; $(F90) ../cosmoaijzero.f90
cosmoamatdadd.o           : ../cosmoamatdadd.f90           ; $(F90) ../cosmoamatdadd.f90
cosmoderv.o               : ../cosmoderv.f90               ; $(F90) ../cosmoderv.f90
costfn.o                  : ../costfn.f90                  ; $(F90) ../costfn.f90
create.o                  : ../create.f90                  ; $(F90) ../create.f90
createsphere.o            : ../createsphere.f90            ; $(F90) ../createsphere.f90
cscheck.o                 : ../cscheck.f90                 ; $(F90) ../cscheck.f90
cutscheck.o               : ../cutscheck.f90               ; $(F90) ../cutscheck.f90
datetime.o                : ../datetime.f90                ; $(F90) ../datetime.f90
dcharge.o                 : ../dcharge.f90                 ; $(F90) ../dcharge.f90
dcharges.o                : ../dcharges.f90                ; $(F90) ../dcharges.f90
d1charge.o                : ../d1charge.f90                ; $(F90) ../d1charge.f90
d1charge3.o               : ../d1charge3.f90               ; $(F90) ../d1charge3.f90
d1charges.o               : ../d1charges.f90               ; $(F90) ../d1charges.f90
d2charge.o                : ../d2charge.f90                ; $(F90) ../d2charge.f90
d2charge3.o               : ../d2charge3.f90               ; $(F90) ../d2charge3.f90
d2charge3p.o              : ../d2charge3p.f90              ; $(F90) ../d2charge3p.f90
d2chargep.o               : ../d2chargep.f90               ; $(F90) ../d2chargep.f90
d2charge2D.o              : ../d2charge2D.f90              ; $(F90) ../d2charge2D.f90
d2chargep2D.o             : ../d2chargep2D.f90             ; $(F90) ../d2chargep2D.f90
dcosmowt.o                : ../dcosmowt.f90                ; $(F90) ../dcosmowt.f90
dtotcosmowt.o             : ../dtotcosmowt.f90             ; $(F90) ../dtotcosmowt.f90
defect.o                  : ../defect.f90                  ; $(F90) ../defect.f90
defener.o                 : ../defener.f90                 ; $(F90) ../defener.f90
defequ.o                  : ../defequ.f90                  ; $(F90) ../defequ.f90
defequ2a.o                : ../defequ2a.f90                ; $(F90) ../defequ2a.f90
deffreq.o                 : ../deffreq.f90                 ; $(F90) ../deffreq.f90
deffun.o                  : ../deffun.f90                  ; $(F90) ../deffun.f90
defoin.o                  : ../defoin.f90                  ; $(F90) ../defoin.f90
defopt.o                  : ../defopt.f90                  ; $(F90) ../defopt.f90
defout.o                  : ../defout.f90                  ; $(F90) ../defout.f90
defpot.o                  : ../defpot.f90                  ; $(F90) ../defpot.f90
defsec.o                  : ../defsec.f90                  ; $(F90) ../defsec.f90
defsym.o                  : ../defsym.f90                  ; $(F90) ../defsym.f90
deftmat.o                 : ../deftmat.f90                 ; $(F90) ../deftmat.f90
defword.o                 : ../defword.f90                 ; $(F90) ../defword.f90
density0.o                : ../density0.f90                ; $(F90) ../density0.f90
density112.o              : ../density112.f90              ; $(F90) ../density112.f90
density11s2.o             : ../density11s2.f90             ; $(F90) ../density11s2.f90
density12a2.o             : ../density12a2.f90             ; $(F90) ../density12a2.f90
density12as2.o            : ../density12as2.f90            ; $(F90) ../density12as2.f90
density2a.o               : ../density2a.f90               ; $(F90) ../density2a.f90
density3.o                : ../density3.f90                ; $(F90) ../density3.f90
density3s.o               : ../density3s.f90               ; $(F90) ../density3s.f90
densitysd.o               : ../densitysd.f90               ; $(F90) ../densitysd.f90
derfc.o                   : ../derfc.f90                   ; $(F90) ../derfc.f90
destroy.o                 : ../destroy.f90                 ; $(F90) ../destroy.f90
diagdyn.o                 : ../diagdyn.f90                 ; $(F90) ../diagdyn.f90
dipole3D.o                : ../dipole3D.f90                ; $(F90) ../dipole3D.f90
dipole3Dtrial.o           : ../dipole3Dtrial.f90           ; $(F90) ../dipole3Dtrial.f90
dipolesd.o                : ../dipolesd.f90                ; $(F90) ../dipolesd.f90
distance.o                : ../distance.f90                ; $(F90) ../distance.f90
dlamch.o                  : ../dlamch.f90                  ; $(F90O0) ../dlamch.f90
dpoccon.o                 : ../dpoccon.f90                 ; $(F90) ../dpoccon.f90
dspecial.o                : ../dspecial.f90                ; $(F90) ../dspecial.f90
dump34.o                  : ../dump34.f90                  ; $(F90O1) ../dump34.f90
dumpbo.o                  : ../dumpbo.f90                  ; $(F90O1) ../dumpbo.f90
dumpdur.o                 : ../dumpdur.f90                 ; $(F90O1) ../dumpdur.f90
dumpgen.o                 : ../dumpgen.f90                 ; $(F90O1) $(FoX_fcflags) ../dumpgen.f90
dumppot.o                 : ../dumppot.f90                 ; $(F90O1) ../dumppot.f90
dutils.o                  : ../dutils.f90                  ; $(F90) ../dutils.f90
dynamic.o                 : ../dynamic.f90                 ; $(F90) ../dynamic.f90
dynamicn.o                : ../dynamicn.f90                ; $(F90) ../dynamicn.f90
dynamicn_defect.o         : ../dynamicn_defect.f90         ; $(F90) ../dynamicn_defect.f90
eamfnderv.o               : ../eamfnderv.f90               ; $(F90O1) ../eamfnderv.f90
eamrho.o                  : ../eamrho.f90                  ; $(F90) ../eamrho.f90
eamscalescrho.o           : ../eamscalescrho.f90           ; $(F90) ../eamscalescrho.f90
edip2body.o               : ../edip2body.f90               ; $(F90) ../edip2body.f90
edip3body.o               : ../edip3body.f90               ; $(F90) ../edip3body.f90
edipcn.o                  : ../edipcn.f90                  ; $(F90) ../edipcn.f90
edipmd.o                  : ../edipmd.f90                  ; $(F90) ../edipmd.f90
eem.o                     : ../eem.f90                     ; $(F90) ../eem.f90
electricfield.o           : ../electricfield.f90           ; $(F90) ../electricfield.f90
electricfieldparts.o      : ../electricfieldparts.f90      ; $(F90) ../electricfieldparts.f90
electricfieldpotl.o       : ../electricfieldpotl.f90       ; $(F90) ../electricfieldpotl.f90
electricfieldtrial.o      : ../electricfieldtrial.f90      ; $(F90) ../electricfieldtrial.f90
einstein.o                : ../einstein.f90                ; $(F90) ../einstein.f90
einsteinsd2.o             : ../einsteinsd2.f90             ; $(F90) ../einsteinsd2.f90
einsteintrial.o           : ../einsteintrial.f90           ; $(F90) ../einsteintrial.f90
eispack.o                 : ../eispack.f90                 ; $(F90) ../eispack.f90
emfunc.o                  : ../emfunc.f90                  ; $(F90) ../emfunc.f90
endstring.o               : ../endstring.f90               ; $(F90) ../endstring.f90
energy.o                  : ../energy.f90                  ; $(F90) ../energy.f90
epot.o                    : ../epot.f90                    ; $(F90) ../epot.f90
epslon.o                  : ../epslon.f90                  ; $(F90) ../epslon.f90
equpos.o                  : ../equpos.f90                  ; $(F90) ../equpos.f90
exchng.o                  : ../exchng.f90                  ; $(F90) ../exchng.f90
fefunct.o                 : ../fefunct.f90                 ; $(F90) ../fefunct.f90
fefunctn.o                : ../fefunctn.f90                ; $(F90) ../fefunctn.f90
fenergy0.o                : ../fenergy0.f90                ; $(F90) ../fenergy0.f90
fenergy3.o                : ../fenergy3.f90                ; $(F90) ../fenergy3.f90
finddef.o                 : ../finddef.f90                 ; $(F90) ../finddef.f90
findrdcmax.o              : ../findrdcmax.f90              ; $(F90) ../findrdcmax.f90
finish.o                  : ../finish.f90                  ; $(F90) ../finish.f90
firstpass.o               : ../firstpass.f90               ; $(F90) ../firstpass.f90
fit.o                     : ../fit.f90                     ; $(F90) ../fit.f90
fitbfgs.o                 : ../fitbfgs.f90                 ; $(F90) ../fitbfgs.f90
fitcon.o                  : ../fitcon.f90                  ; $(F90) ../fitcon.f90
fitfun.o                  : ../fitfun.f90                  ; $(F90) ../fitfun.f90
fitfunct.o                : ../fitfunct.f90                ; $(F90) ../fitfunct.f90
fitgrad.o                 : ../fitgrad.f90                 ; $(F90) ../fitgrad.f90
fitmin.o                  : ../fitmin.f90                  ; $(F90) ../fitmin.f90
fitword.o                 : ../fitword.f90                 ; $(F90) ../fitword.f90
force.o                   : ../force.f90                   ; $(F90) ../force.f90
forcetrial.o              : ../forcetrial.f90              ; $(F90) ../forcetrial.f90
formula.o                 : ../formula.f90                 ; $(F90) ../formula.f90
fournos.o                 : ../fournos.f90                 ; $(F90) ../fournos.f90
four12.o                  : ../four12.f90                  ; $(F90) ../four12.f90
fourbody.o                : ../fourbody.f90                ; $(F90) ../fourbody.f90
fourlist.o                : ../fourlist.f90                ; $(F90) ../fourlist.f90
fouroop.o                 : ../fouroop.f90                 ; $(F90) ../fouroop.f90
fouroopp.o                : ../fouroopp.f90                ; $(F90) ../fouroopp.f90
fouroopmc.o               : ../fouroopmc.f90               ; $(F90) ../fouroopmc.f90
fouroopmd.o               : ../fouroopmd.f90               ; $(F90) ../fouroopmd.f90
fouroopmds.o              : ../fouroopmds.f90              ; $(F90) ../fouroopmds.f90
fouroopsd.o               : ../fouroopsd.f90               ; $(F90) ../fouroopsd.f90
fouroop12.o               : ../fouroop12.f90               ; $(F90) ../fouroop12.f90
fouroopsd2.o              : ../fouroopsd2.f90              ; $(F90) ../fouroopsd2.f90
fourp.o                   : ../fourp.f90                   ; $(F90O1) ../fourp.f90
fourmc.o                  : ../fourmc.f90                  ; $(F90) ../fourmc.f90
fourmd.o                  : ../fourmd.f90                  ; $(F90) ../fourmd.f90
fourmds.o                 : ../fourmds.f90                 ; $(F90) ../fourmds.f90
foursd.o                  : ../foursd.f90                  ; $(F90) ../foursd.f90
foursd2.o                 : ../foursd2.f90                 ; $(F90) ../foursd2.f90
fourstrterms.o            : ../fourstrterms.f90            ; $(F90) ../fourstrterms.f90
four0d3.o                 : ../four0d3.f90                 ; $(F90) ../four0d3.f90
four3d3.o                 : ../four3d3.f90                 ; $(F90) ../four3d3.f90
ftow.o                    : ../ftow.f90                    ; $(F90) ../ftow.f90
funct.o                   : ../funct.f90                   ; $(F90) ../funct.f90
functn.o                  : ../functn.f90                  ; $(F90) ../functn.f90
functnf.o                 : ../functnf.f90                 ; $(F90) ../functnf.f90
functneb.o                : ../functneb.f90                ; $(F90) ../functneb.f90
functsync.o               : ../functsync.f90               ; $(F90) ../functsync.f90
gabcfg.o                  : ../gabcfg.f90                  ; $(F90) ../gabcfg.f90
gacreate.o                : ../gacreate.f90                ; $(F90) ../gacreate.f90
gacross.o                 : ../gacross.f90                 ; $(F90) ../gacross.f90
gaexpd.o                  : ../gaexpd.f90                  ; $(F90) ../gaexpd.f90
gafit.o                   : ../gafit.f90                   ; $(F90) ../gafit.f90
gamaxmin.o                : ../gamaxmin.f90                ; $(F90) ../gamaxmin.f90
gamma.o                   : ../gamma.f90                   ; $(F90) ../gamma.f90
gammasm.o                 : ../gammasm.f90                 ; $(F90) ../gammasm.f90
gamuta.o                  : ../gamuta.f90                  ; $(F90) ../gamuta.f90
gaopt.o                   : ../gaopt.f90                   ; $(F90) ../gaopt.f90
garepro.o                 : ../garepro.f90                 ; $(F90) ../garepro.f90
gasort.o                  : ../gasort.f90                  ; $(F90) ../gasort.f90
gasteiger.o               : ../gasteiger.f90               ; $(F90) ../gasteiger.f90
gasubs.o                  : ../gasubs.f90                  ; $(F90) ../gasubs.f90
gauss.o                   : ../gauss.f90                   ; $(F90) ../gauss.f90
gaword.o                  : ../gaword.f90                  ; $(F90) ../gaword.f90
genpot.o                  : ../genpot.f90                  ; $(F90) ../genpot.f90
genword.o                 : ../genword.f90                 ; $(F90) $(FoX_fcflags) ../genword.f90
getbocharge.o             : ../getbocharge.f90             ; $(F90) ../getbocharge.f90
getbochargetrial.o        : ../getbochargetrial.f90        ; $(F90) ../getbochargetrial.f90
getbonds.o                : ../getbonds.f90                ; $(F90) ../getbonds.f90
getcoordno.o              : ../getcoordno.f90              ; $(F90) ../getcoordno.f90
getderv1.o                : ../getderv1.f90                ; $(F90) ../getderv1.f90
getdipole.o               : ../getdipole.f90               ; $(F90) ../getdipole.f90
getdistances.o            : ../getdistances.f90            ; $(F90) ../getdistances.f90
getkeywrd.o               : ../getkeywrd.f90               ; $(F90) ../getkeywrd.f90
getneighbour.o            : ../getneighbour.f90            ; $(F90) ../getneighbour.f90
getnmr.o                  : ../getnmr.f90                  ; $(F90) ../getnmr.f90
getphi.o                  : ../getphi.f90                  ; $(F90) ../getphi.f90
getpotsymbol.o            : ../getpotsymbol.f90            ; $(F90) ../getpotsymbol.f90
gettraceinertia.o         : ../gettraceinertia.f90         ; $(F90) ../gettraceinertia.f90
getvibmode.o              : ../getvibmode.f90              ; $(F90) ../getvibmode.f90
gtheta.o                  : ../gtheta.f90                  ; $(F90) ../gtheta.f90
gthetabo.o                : ../gthetabo.f90                ; $(F90) ../gthetabo.f90
gulpfinish.o              : ../gulpfinish.f90              ; $(F90) $(FoX_fcflags) ../gulpfinish.f90
help.o                    : ../help.f90                    ; $(F90) ../help.f90
hfunc.o                   : ../hfunc.f90                   ; $(F90) ../hfunc.f90
indsft.o                  : ../indsft.f90                  ; $(F90) ../indsft.f90
ind1toijk.o               : ../ind1toijk.f90               ; $(F90) ../ind1toijk.f90
indtoijk.o                : ../indtoijk.f90                ; $(F90) ../indtoijk.f90
initarc.o                 : ../initarc.f90                 ; $(F90) ../initarc.f90
init1bodydefaults.o       : ../init1bodydefaults.f90       ; $(F90) ../init1bodydefaults.f90
init2bodydefaults.o       : ../init2bodydefaults.f90       ; $(F90) ../init2bodydefaults.f90
initmaxcfgdefaults.o      : ../initmaxcfgdefaults.f90      ; $(F90) ../initmaxcfgdefaults.f90
initmaxatdefaults.o       : ../initmaxatdefaults.f90       ; $(F90) ../initmaxatdefaults.f90
initmaxatotdefaults.o     : ../initmaxatotdefaults.f90     ; $(F90) ../initmaxatotdefaults.f90
initmaxbonddefaults.o     : ../initmaxbonddefaults.f90     ; $(F90) ../initmaxbonddefaults.f90
initmaxbondqdefaults.o    : ../initmaxbondqdefaults.f90    ; $(F90) ../initmaxbondqdefaults.f90
initmaxccspecdefaults.o   : ../initmaxccspecdefaults.f90   ; $(F90) ../initmaxccspecdefaults.f90
initmaxconnectdefaults.o  : ../initmaxconnectdefaults.f90  ; $(F90) ../initmaxconnectdefaults.f90
initmaxdefdefaults.o      : ../initmaxdefdefaults.f90      ; $(F90) ../initmaxdefdefaults.f90
initmaxeamdendefaults.o   : ../initmaxeamdendefaults.f90   ; $(F90) ../initmaxeamdendefaults.f90
initmaxeamfnspecdefaults.o: ../initmaxeamfnspecdefaults.f90; $(F90) ../initmaxeamfnspecdefaults.f90
initmaxeamspecdefaults.o  : ../initmaxeamspecdefaults.f90  ; $(F90) ../initmaxeamspecdefaults.f90
initmaxedipspecdefaults.o : ../initmaxedipspecdefaults.f90 ; $(F90) ../initmaxedipspecdefaults.f90
initmaxfgraddefaults.o    : ../initmaxfgraddefaults.f90    ; $(F90) ../initmaxfgraddefaults.f90
initmaxfitdefaults.o      : ../initmaxfitdefaults.f90      ; $(F90) ../initmaxfitdefaults.f90
initmaxfordefaults.o      : ../initmaxfordefaults.f90      ; $(F90) ../initmaxfordefaults.f90
initmaxfstressdefaults.o  : ../initmaxfstressdefaults.f90  ; $(F90) ../initmaxfstressdefaults.f90
initmaxgcmcmoldefaults.o  : ../initmaxgcmcmoldefaults.f90  ; $(F90) ../initmaxgcmcmoldefaults.f90
initmaxlibdefaults.o      : ../initmaxlibdefaults.f90      ; $(F90) ../initmaxlibdefaults.f90
initmaxmcswapspec.o       : ../initmaxmcswapspec.f90       ; $(F90) ../initmaxmcswapspec.f90
initmaxmoldefaults.o      : ../initmaxmoldefaults.f90      ; $(F90) ../initmaxmoldefaults.f90
initmaxnboadefaults.o     : ../initmaxnboadefaults.f90     ; $(F90) ../initmaxnboadefaults.f90
initmaxnbopotdefaults.o   : ../initmaxnbopotdefaults.f90   ; $(F90) ../initmaxnbopotdefaults.f90
initmaxnboq0defaults.o    : ../initmaxnboq0defaults.f90    ; $(F90) ../initmaxnboq0defaults.f90
initmaxnboqdefaults.o     : ../initmaxnboqdefaults.f90     ; $(F90) ../initmaxnboqdefaults.f90
initmaxnbordefaults.o     : ../initmaxnbordefaults.f90     ; $(F90) ../initmaxnbordefaults.f90
initmaxnebreplicatotdefaults.o : ../initmaxnebreplicatotdefaults.f90 ; $(F90) ../initmaxnebreplicatotdefaults.f90
initmaxnppadefaults.o     : ../initmaxnppadefaults.f90     ; $(F90) ../initmaxnppadefaults.f90
initmaxnptsdefaults.o     : ../initmaxnptsdefaults.f90     ; $(F90) ../initmaxnptsdefaults.f90
initmaxobsdefaults.o      : ../initmaxobsdefaults.f90      ; $(F90) ../initmaxobsdefaults.f90
initmaxplanepotdefaults.o : ../initmaxplanepotdefaults.f90 ; $(F90) ../initmaxplanepotdefaults.f90
initmaxr1atdefaults.o     : ../initmaxr1atdefaults.f90     ; $(F90) ../initmaxr1atdefaults.f90
initmaxreaxffspecdefaults.o : ../initmaxreaxffspecdefaults.f90 ; $(F90) ../initmaxreaxffspecdefaults.f90
initmaxreaxffval3defaults.o : ../initmaxreaxffval3defaults.f90 ; $(F90) ../initmaxreaxffval3defaults.f90
initmaxregiondefaults.o   : ../initmaxregiondefaults.f90   ; $(F90) ../initmaxregiondefaults.f90
initmaxsixdefaults.o      : ../initmaxsixdefaults.f90      ; $(F90) ../initmaxsixdefaults.f90
initmaxspcellbodefaults.o : ../initmaxspcellbodefaults.f90 ; $(F90) ../initmaxspcellbodefaults.f90
initmaxspcelldefaults.o   : ../initmaxspcelldefaults.f90   ; $(F90) ../initmaxspcelldefaults.f90
initmaxspecdefaults.o     : ../initmaxspecdefaults.f90     ; $(F90) ../initmaxspecdefaults.f90
initmaxthbdefaults.o      : ../initmaxthbdefaults.f90      ; $(F90) ../initmaxthbdefaults.f90
initmaxtitledefaults.o    : ../initmaxtitledefaults.f90    ; $(F90) ../initmaxtitledefaults.f90
init_ins.o                : ../init_ins.f90                ; $(F90) ../init_ins.f90
initktrm.o                : ../initktrm.f90                ; $(F90) ../initktrm.f90
initmemory.o              : ../initmemory.f90              ; $(F90) ../initmemory.f90
initqmatrix.o             : ../initqmatrix.f90             ; $(F90) ../initqmatrix.f90
initqmatrixc.o            : ../initqmatrixc.f90            ; $(F90) ../initqmatrixc.f90
insertatom.o              : ../insertatom.f90              ; $(F90) ../insertatom.f90
intochar.o                : ../intochar.f90                ; $(F90) ../intochar.f90
inword.o                  : ../inword.f90                  ; $(F90O1) ../inword.f90
istoan.o                  : ../istoan.f90                  ; $(F90) ../istoan.f90
itcom.o                   : ../itcom.f90                   ; $(F90) ../itcom.f90
iterinv.o                 : ../iterinv.f90                 ; $(F90) ../iterinv.f90
itow.o                    : ../itow.f90                    ; $(F90) ../itow.f90
itpack.o                  : ../itpack.f90                  ; $(F90) ../itpack.f90
kindex.o                  : ../kindex.f90                  ; $(F90) ../kindex.f90
kvector.o                 : ../kvector.f90                 ; $(F90) ../kvector.f90
kvectorf.o                : ../kvectorf.f90                ; $(F90) ../kvectorf.f90
kweight.o                 : ../kweight.f90                 ; $(F90) ../kweight.f90
label.o                   : ../label.f90                   ; $(F90) ../label.f90
labellc.o                 : ../labellc.f90                 ; $(F90) ../labellc.f90
lambda.o                  : ../lambda.f90                  ; $(F90O1) ../lambda.f90
lapack.o                  : ../lapack.f90                  ; $(F90O2) ../lapack.f90
latpot.o                  : ../latpot.f90                  ; $(F90) $(FoX_fcflags) ../latpot.f90
lmbfgs.o                  : ../lmbfgs.f90                  ; $(F90) ../lmbfgs.f90
linepro.o                 : ../linepro.f90                 ; $(F90) ../linepro.f90
linpack.o                 : ../linpack.f90                 ; $(F90) ../linpack.f90
lintoijk.o                : ../lintoijk.f90                ; $(F90) ../lintoijk.f90
lmatch.o                  : ../lmatch.f90                  ; $(F90) ../lmatch.f90
lmatchany.o               : ../lmatchany.f90               ; $(F90) ../lmatchany.f90
lmatchpair.o              : ../lmatchpair.f90              ; $(F90) ../lmatchpair.f90
lower.o                   : ../lower.f90                   ; $(F90) ../lower.f90
ltont.o                   : ../ltont.f90                   ; $(F90) ../ltont.f90
m_pdf.o                   : ../m_pdf.f90                   ; $(F90) ../m_pdf.f90
m_pdfneutron.o            : ../m_pdfneutron.f90            ; $(F90) ../m_pdfneutron.f90
m_pdfvariables.o          : ../m_pdfvariables.f90          ; $(F90) ../m_pdfvariables.f90
m_simplex.o               : ../m_simplex.f90               ; $(F90) ../m_simplex.f90
many.o                    : ../many.f90                    ; $(F90) ../many.f90
manyp.o                   : ../manyp.f90                   ; $(F90) ../manyp.f90
manysd.o                  : ../manysd.f90                  ; $(F90) ../manysd.f90
many0d.o                  : ../many0d.f90                  ; $(F90) ../many0d.f90
many12.o                  : ../many12.f90                  ; $(F90) ../many12.f90
manymd0.o                 : ../manymd0.f90                 ; $(F90) ../manymd0.f90
manymd3.o                 : ../manymd3.f90                 ; $(F90) ../manymd3.f90
manymd3s.o                : ../manymd3s.f90                ; $(F90) ../manymd3s.f90
manymi3.o                 : ../manymi3.f90                 ; $(F90) ../manymi3.f90
manysd2.o                 : ../manysd2.f90                 ; $(F90) ../manysd2.f90
matinv.o                  : ../matinv.f90                  ; $(F90) ../matinv.f90
mccreate.o                : ../mccreate.f90                ; $(F90) ../mccreate.f90
mcenergy.o                : ../mcenergy.f90                ; $(F90) ../mcenergy.f90
mctrialfunction.o         : ../mctrialfunction.f90         ; $(F90) ../mctrialfunction.f90
mcdestroy.o               : ../mcdestroy.f90               ; $(F90) ../mcdestroy.f90
mcmolinsert.o             : ../mcmolinsert.f90             ; $(F90) ../mcmolinsert.f90
mcmove.o                  : ../mcmove.f90                  ; $(F90) ../mcmove.f90
mcrotate.o                : ../mcrotate.f90                ; $(F90) ../mcrotate.f90
mcstrain.o                : ../mcstrain.f90                ; $(F90) ../mcstrain.f90
mcstorecfg.o              : ../mcstorecfg.f90              ; $(F90) ../mcstorecfg.f90
mcswap.o                  : ../mcswap.f90                  ; $(F90) ../mcswap.f90
mcword.o                  : ../mcword.f90                  ; $(F90) ../mcword.f90
mdcellind.o               : ../mdcellind.f90               ; $(F90) ../mdcellind.f90
mdcorrect.o               : ../mdcorrect.f90               ; $(F90) ../mdcorrect.f90
mddistconpost.o           : ../mddistconpost.f90           ; $(F90) ../mddistconpost.f90
mdgcorrect.o              : ../mdgcorrect.f90              ; $(F90) ../mdgcorrect.f90
mdvvcorrect.o             : ../mdvvcorrect.f90             ; $(F90) ../mdvvcorrect.f90
mdlfvcorrect.o            : ../mdlfvcorrect.f90            ; $(F90) ../mdlfvcorrect.f90
mdke.o                    : ../mdke.f90                    ; $(F90) ../mdke.f90
mdkeatomicstress.o        : ../mdkeatomicstress.f90        ; $(F90) ../mdkeatomicstress.f90
mdkestrain.o              : ../mdkestrain.f90              ; $(F90) ../mdkestrain.f90
mdpredict.o               : ../mdpredict.f90               ; $(F90) ../mdpredict.f90
mdprop.o                  : ../mdprop.f90                  ; $(F90) $(FoX_fcflags) ../mdprop.f90
mdreset.o                 : ../mdreset.f90                 ; $(F90) ../mdreset.f90
mdrestart.o               : ../mdrestart.f90               ; $(F90) ../mdrestart.f90
mdscale.o                 : ../mdscale.f90                 ; $(F90) ../mdscale.f90
mdshscale.o               : ../mdshscale.f90               ; $(F90) ../mdshscale.f90
mdstop.o                  : ../mdstop.f90                  ; $(F90) ../mdstop.f90
mdtemperature.o           : ../mdtemperature.f90           ; $(F90) ../mdtemperature.f90
mdvelcor.o                : ../mdvelcor.f90                ; $(F90) ../mdvelcor.f90
mdword.o                  : ../mdword.f90                  ; $(F90) ../mdword.f90
mdwrite.o                 : ../mdwrite.f90                 ; $(F90) ../mdwrite.f90
meamfnderv.o              : ../meamfnderv.f90              ; $(F90) ../meamfnderv.f90
meamrho.o                 : ../meamrho.f90                 ; $(F90) ../meamrho.f90
meamscreen.o              : ../meamscreen.f90              ; $(F90) ../meamscreen.f90
meamsymdensity.o          : ../meamsymdensity.f90          ; $(F90) ../meamsymdensity.f90
meamtotalrho.o            : ../meamtotalrho.f90            ; $(F90) ../meamtotalrho.f90
meamtotalrhoderv.o        : ../meamtotalrhoderv.f90        ; $(F90) ../meamtotalrhoderv.f90
meamtotalrhocrossderv.o   : ../meamtotalrhocrossderv.f90   ; $(F90) ../meamtotalrhocrossderv.f90
meamtotalrhoscreenderv.o  : ../meamtotalrhoscreenderv.f90  ; $(F90) ../meamtotalrhoscreenderv.f90
methodok.o                : ../methodok.f90                ; $(F90) ../methodok.f90
mindtoijk.o               : ../mindtoijk.f90               ; $(F90) ../mindtoijk.f90
molind.o                  : ../molind.f90                  ; $(F90) ../molind.f90
molindfix.o               : ../molindfix.f90               ; $(F90) ../molindfix.f90
molindtrial.o             : ../molindtrial.f90             ; $(F90) ../molindtrial.f90
molindfixtrial.o          : ../molindfixtrial.f90          ; $(F90) ../molindfixtrial.f90
moltype.o                 : ../moltype.f90                 ; $(F90) ../moltype.f90
move2a1.o                 : ../move2a1.f90                 ; $(F90) ../move2a1.f90
mxmb.o                    : ../mxmb.f90                    ; $(F90) ../mxmb.f90
nagamma.o                 : ../nagamma.f90                 ; $(F90) ../nagamma.f90
nearest.o                 : ../nearest.f90                 ; $(F90) ../nearest.f90
nebforce.o                : ../nebforce.f90                ; $(F90) ../nebforce.f90
nebword.o                 : ../nebword.f90                 ; $(F90) ../nebword.f90
nrhess.o                  : ../nrhess.f90                  ; $(F90) ../nrhess.f90
nrstep.o                  : ../nrstep.f90                  ; $(F90) ../nrstep.f90
nullpointer.o             : ../nullpointer.f90             ; $(F90) ../nullpointer.f90
nudge.o                   : ../nudge.f90                   ; $(F90) ../nudge.f90
numhess.o                 : ../numhess.f90                 ; $(F90) ../numhess.f90
okspec.o                  : ../okspec.f90                  ; $(F90) ../okspec.f90
olinmin.o                 : ../olinmin.f90                 ; $(F90) ../olinmin.f90
omegaproperty.o           : ../omegaproperty.f90           ; $(F90) ../omegaproperty.f90
onebody.o                 : ../onebody.f90                 ; $(F90) ../onebody.f90
optim.o                   : ../optim.f90                   ; $(F90) $(FoX_fcflags) ../optim.f90
optin.o                   : ../optin.f90                   ; $(F90) ../optin.f90
options.o                 : ../options.f90                 ; $(F90) $(FoX_fcflags) ../options.f90
optout.o                  : ../optout.f90                  ; $(F90) ../optout.f90
optshe.o                  : ../optshe.f90                  ; $(F90) ../optshe.f90
oscillatorstrength.o      : ../oscillatorstrength.f90      ; $(F90) ../oscillatorstrength.f90
oscillatorstrengthg.o     : ../oscillatorstrengthg.f90     ; $(F90) ../oscillatorstrengthg.f90
outadvice.o               : ../outadvice.f90               ; $(F90) ../outadvice.f90
outarc.o                  : ../outarc.f90                  ; $(F90) ../outarc.f90
outatomicstress.o         : ../outatomicstress.f90         ; $(F90) ../outatomicstress.f90
outbiograf.o              : ../outbiograf.f90              ; $(F90) ../outbiograf.f90
outcell.o                 : ../outcell.f90                 ; $(F90) ../outcell.f90
outcif.o                  : ../outcif.f90                  ; $(F90) ../outcif.f90
outcssr.o                 : ../outcssr.f90                 ; $(F90) ../outcssr.f90
outdcdheader.o            : ../outdcdheader.f90            ; $(F90) ../outdcdheader.f90
outdefin.o                : ../outdefin.f90                ; $(F90) ../outdefin.f90
outdener.o                : ../outdener.f90                ; $(F90) ../outdener.f90
outderv.o                 : ../outderv.f90                 ; $(F90) ../outderv.f90
outdlv.o                  : ../outdlv.f90                  ; $(F90) ../outdlv.f90
outdrv.o                  : ../outdrv.f90                  ; $(F90) ../outdrv.f90
outed.o                   : ../outed.f90                   ; $(F90) ../outed.f90
outener.o                 : ../outener.f90                 ; $(F90) $(FoX_fcflags) ../outener.f90
outerror.o                : ../outerror.f90                ; $(F90) ../outerror.f90
outfdf.o                  : ../outfdf.f90                  ; $(F90) ../outfdf.f90
outfile.o                 : ../outfile.f90                 ; $(F90) $(FoX_fcflags) ../outfile.f90
outfit.o                  : ../outfit.f90                  ; $(F90) ../outfit.f90
outfitcon.o               : ../outfitcon.f90               ; $(F90) ../outfitcon.f90
outfrc.o                  : ../outfrc.f90                  ; $(F90) ../outfrc.f90
outfreq.o                 : ../outfreq.f90                 ; $(F90) ../outfreq.f90
outgafit.o                : ../outgafit.f90                ; $(F90) ../outgafit.f90
outgaopt.o                : ../outgaopt.f90                ; $(F90) ../outgaopt.f90
outgapar.o                : ../outgapar.f90                ; $(F90) ../outgapar.f90
outgen.o                  : ../outgen.f90                  ; $(F90) ../outgen.f90
outkey.o                  : ../outkey.f90                  ; $(F90) ../outkey.f90
outkpt.o                  : ../outkpt.f90                  ; $(F90) ../outkpt.f90
outlammps.o               : ../outlammps.f90               ; $(F90) ../outlammps.f90
outlammpspots.o           : ../outlammpspots.f90           ; $(F90) ../outlammpspots.f90
outmarv.o                 : ../outmarv.f90                 ; $(F90) ../outmarv.f90
outmarv2.o                : ../outmarv2.f90                ; $(F90) ../outmarv2.f90
outmc.o                   : ../outmc.f90                   ; $(F90) ../outmc.f90
outmd.o                   : ../outmd.f90                   ; $(F90) ../outmd.f90
outmdin.o                 : ../outmdin.f90                 ; $(F90) ../outmdin.f90
outmol.o                  : ../outmol.f90                  ; $(F90) ../outmol.f90
outneb.o                  : ../outneb.f90                  ; $(F90) ../outneb.f90
outnebin.o                : ../outnebin.f90                ; $(F90) ../outnebin.f90
outobs.o                  : ../outobs.f90                  ; $(F90) ../outobs.f90
outofmemory.o             : ../outofmemory.f90             ; $(F90) ../outofmemory.f90
outofp.o                  : ../outofp.f90                  ; $(F90) ../outofp.f90
outphon.o                 : ../outphon.f90                 ; $(F90) ../outphon.f90
outpolar.o                : ../outpolar.f90                ; $(F90) ../outpolar.f90
outpot.o                  : ../outpot.f90                  ; $(F90) $(FoX_fcflags) ../outpot.f90
outqbo.o                  : ../outqbo.f90                  ; $(F90) ../outqbo.f90
outspec.o                 : ../outspec.f90                 ; $(F90) ../outspec.f90
outspec_pdf.o             : ../outspec_pdf.f90             ; $(F90) ../outspec_pdf.f90
outstresses.o             : ../outstresses.f90             ; $(F90) ../outstresses.f90
outstructure.o            : ../outstructure.f90            ; $(F90) ../outstructure.f90
outsync.o                 : ../outsync.f90                 ; $(F90) ../outsync.f90
outsyncin.o               : ../outsyncin.f90               ; $(F90) ../outsyncin.f90
outthb.o                  : ../outthb.f90                  ; $(F90) ../outthb.f90
outtime.o                 : ../outtime.f90                 ; $(F90) ../outtime.f90
outvar.o                  : ../outvar.f90                  ; $(F90) ../outvar.f90
outvarg.o                 : ../outvarg.f90                 ; $(F90) ../outvarg.f90
outwarning.o              : ../outwarning.f90              ; $(F90) ../outwarning.f90
outxr.o                   : ../outxr.f90                   ; $(F90) ../outxr.f90
outxtl.o                  : ../outxtl.f90                  ; $(F90) ../outxtl.f90
outxyz.o                  : ../outxyz.f90                  ; $(F90) ../outxyz.f90
pdiag.o                   : ../pdiag.f90                   ; $(F90) ../pdiag.f90
pdiage.o                  : ../pdiage.f90                  ; $(F90) ../pdiage.f90
pdiagg.o                  : ../pdiagg.f90                  ; $(F90) ../pdiagg.f90
pdiagl.o                  : ../pdiagl.f90                  ; $(F90) ../pdiagl.f90
peigen.o                  : ../peigen.f90                  ; $(F90) ../peigen.f90
peigeng.o                 : ../peigeng.f90                 ; $(F90) ../peigeng.f90
phoncopy1.o               : ../phoncopy1.f90               ; $(F90) ../phoncopy1.f90
phoncopy2.o               : ../phoncopy2.f90               ; $(F90) ../phoncopy2.f90
phonon.o                  : ../phonon.f90                  ; $(F90) $(FoX_fcflags) ../phonon.f90
phonword.o                : ../phonword.f90                ; $(F90) ../phonword.f90
pireal0d.o                : ../pireal0d.f90                ; $(F90) ../pireal0d.f90
pirealrecip.o             : ../pirealrecip.f90             ; $(F90) ../pirealrecip.f90
pirealrecips.o            : ../pirealrecips.f90            ; $(F90) ../pirealrecips.f90
planepot2D.o              : ../planepot2D.f90              ; $(F90) ../planepot2D.f90
planepotmc.o              : ../planepotmc.f90              ; $(F90) ../planepotmc.f90
planepotmd.o              : ../planepotmd.f90              ; $(F90) ../planepotmd.f90
poccon.o                  : ../poccon.f90                  ; $(F90) ../poccon.f90
poccons.o                 : ../poccons.f90                 ; $(F90) ../poccons.f90
polarisation.o            : ../polarisation.f90            ; $(F90) ../polarisation.f90
potcut.o                  : ../potcut.f90                  ; $(F90O1) ../potcut.f90
potential.o               : ../potential.f90               ; $(F90) ../potential.f90
potgrid.o                 : ../potgrid.f90                 ; $(F90) ../potgrid.f90
potword21.o               : ../potword21.f90               ; $(F90O1) ../potword21.f90
potword22.o               : ../potword22.f90               ; $(F90O1) ../potword22.f90
potword3.o                : ../potword3.f90                ; $(F90O1) ../potword3.f90
potword4.o                : ../potword4.f90                ; $(F90O1) ../potword4.f90
potword6.o                : ../potword6.f90                ; $(F90O1) ../potword6.f90
potwordm.o                : ../potwordm.f90                ; $(F90O1) ../potwordm.f90
pr_integrator.o           : ../pr_integrator.f90           ; $(F90) ../pr_integrator.f90
predict.o                 : ../predict.f90                 ; $(F90) ../predict.f90
pressure.o                : ../pressure.f90                ; $(F90) ../pressure.f90
projd0.o                  : ../projd0.f90                  ; $(F90) ../projd0.f90
projd3a.o                 : ../projd3a.f90                 ; $(F90) ../projd3a.f90
projfork0.o               : ../projfork0.f90               ; $(F90) ../projfork0.f90
projfork3.o               : ../projfork3.f90               ; $(F90) ../projfork3.f90
projfork3d.o              : ../projfork3d.f90              ; $(F90) ../projfork3d.f90
projfork3ds.o             : ../projfork3ds.f90             ; $(F90) ../projfork3ds.f90
projfork3s.o              : ../projfork3s.f90              ; $(F90) ../projfork3s.f90
projthbk0.o               : ../projthbk0.f90               ; $(F90) ../projthbk0.f90
projthbk3.o               : ../projthbk3.f90               ; $(F90) ../projthbk3.f90
projthbk3d.o              : ../projthbk3d.f90              ; $(F90) ../projthbk3d.f90
projthbk3ds.o             : ../projthbk3ds.f90             ; $(F90) ../projthbk3ds.f90
projthbk3s.o              : ../projthbk3s.f90              ; $(F90) ../projthbk3s.f90
property.o                : ../property.f90                ; $(F90) $(FoX_fcflags) ../property.f90
property0.o               : ../property0.f90               ; $(F90) $(FoX_fcflags) ../property0.f90
psumall.o                 : ../psumall.f90                 ; $(F90) ../psumall.f90
putpar.o                  : ../putpar.f90                  ; $(F90) ../putpar.f90
pythag.o                  : ../pythag.f90                  ; $(F90) ../pythag.f90
qdupdate.o                : ../qdupdate.f90                ; $(F90) ../qdupdate.f90
qeqbody.o                 : ../qeqbody.f90                 ; $(F90) ../qeqbody.f90
qmatrix1D.o               : ../qmatrix1D.f90               ; $(F90) ../qmatrix1D.f90
qmatrixelement.o          : ../qmatrixelement.f90          ; $(F90) ../qmatrixelement.f90
qmatrixelementc.o         : ../qmatrixelementc.f90         ; $(F90) ../qmatrixelementc.f90
qupdate.o                 : ../qupdate.f90                 ; $(F90) ../qupdate.f90
radialforce.o             : ../radialforce.f90             ; $(F90) ../radialforce.f90
radialforcetrial.o        : ../radialforcetrial.f90        ; $(F90) ../radialforcetrial.f90
ramantrm.o                : ../ramantrm.f90                ; $(F90) ../ramantrm.f90
random.o                  : ../random.f90                  ; $(F90) ../random.f90
ratfn.o                   : ../ratfn.f90                   ; $(F90) ../ratfn.f90
real0d.o                  : ../real0d.f90                  ; $(F90) ../real0d.f90
real0d3.o                 : ../real0d3.f90                 ; $(F90) ../real0d3.f90
real1d3.o                 : ../real1d3.f90                 ; $(F90) ../real1d3.f90
real1D.o                  : ../real1D.f90                  ; $(F90) ../real1D.f90
real1Dmc.o                : ../real1Dmc.f90                ; $(F90) ../real1Dmc.f90
real1Dmd.o                : ../real1Dmd.f90                ; $(F90) ../real1Dmd.f90
real1Dp.o                 : ../real1Dp.f90                 ; $(F90) ../real1Dp.f90
real1Dq.o                 : ../real1Dq.f90                 ; $(F90) ../real1Dq.f90
real11.o                  : ../real11.f90                  ; $(F90) ../real11.f90
real112.o                 : ../real112.f90                 ; $(F90) ../real112.f90
real11pot.o               : ../real11pot.f90               ; $(F90) ../real11pot.f90
real11s.o                 : ../real11s.f90                 ; $(F90) ../real11s.f90
real11s2.o                : ../real11s2.f90                ; $(F90) ../real11s2.f90
real12a.o                 : ../real12a.f90                 ; $(F90) ../real12a.f90
real12a2.o                : ../real12a2.f90                ; $(F90) ../real12a2.f90
real12apot.o              : ../real12apot.f90              ; $(F90) ../real12apot.f90
real12as.o                : ../real12as.f90                ; $(F90) ../real12as.f90
real12as2.o               : ../real12as2.f90               ; $(F90) ../real12as2.f90
real2a.o                  : ../real2a.f90                  ; $(F90O1) ../real2a.f90
real2a1.o                 : ../real2a1.f90                 ; $(F90) ../real2a1.f90
real2a1f.o                : ../real2a1f.f90                ; $(F90) ../real2a1f.f90
real2adef.o               : ../real2adef.f90               ; $(F90) ../real2adef.f90
real2aef.o                : ../real2aef.f90                ; $(F90) ../real2aef.f90
real2amany.o              : ../real2amany.f90              ; $(F90) ../real2amany.f90
real2b.o                  : ../real2b.f90                  ; $(F90) ../real2b.f90
real2Dq.o                 : ../real2Dq.f90                 ; $(F90) ../real2Dq.f90
real4.o                   : ../real4.f90                   ; $(F90) ../real4.f90
realcmm.o                 : ../realcmm.f90                 ; $(F90) ../realcmm.f90
reale.o                   : ../reale.f90                   ; $(F90) ../reale.f90
reallocate.o              : ../reallocate.f90              ; $(F90) ../reallocate.f90
realmc0.o                 : ../realmc0.f90                 ; $(F90) ../realmc0.f90
realmc3.o                 : ../realmc3.f90                 ; $(F90) ../realmc3.f90
realmd0.o                 : ../realmd0.f90                 ; $(F90) ../realmd0.f90
realmd3.o                 : ../realmd3.f90                 ; $(F90) ../realmd3.f90
realmd3s.o                : ../realmd3s.f90                ; $(F90) ../realmd3s.f90
realmd3t.o                : ../realmd3t.f90                ; $(F90) ../realmd3t.f90
realmi3.o                 : ../realmi3.f90                 ; $(F90) ../realmi3.f90
realmimc3.o               : ../realmimc3.f90               ; $(F90) ../realmimc3.f90
realp.o                   : ../realp.f90                   ; $(F90) ../realp.f90
realrecip3d3.o            : ../realrecip3d3.f90            ; $(F90) ../realrecip3d3.f90
realsd.o                  : ../realsd.f90                  ; $(F90) ../realsd.f90
realsd2.o                 : ../realsd2.f90                 ; $(F90) ../realsd2.f90
reaxffmd.o                : ../reaxffmd.f90                ; $(F90) ../reaxffmd.f90
reaxffpot.o               : ../reaxffpot.f90               ; $(F90) ../reaxffpot.f90
reaxff_bo.o               : ../reaxff_bo.f90               ; $(F90) ../reaxff_bo.f90
reaxff_funcs.o            : ../reaxff_funcs.f90            ; $(F90) ../reaxff_funcs.f90
rebospline.o              : ../rebospline.f90              ; $(F90) ../rebospline.f90
recip2D.o                 : ../recip2D.f90                 ; $(F90) ../recip2D.f90
recip2Dtrial.o            : ../recip2Dtrial.f90            ; $(F90) ../recip2Dtrial.f90
recip3D.o                 : ../recip3D.f90                 ; $(F90) ../recip3D.f90
recip3Dmd.o               : ../recip3Dmd.f90               ; $(F90) ../recip3Dmd.f90
recip3Dtrial.o            : ../recip3Dtrial.f90            ; $(F90) ../recip3Dtrial.f90
recip12a.o                : ../recip12a.f90                ; $(F90) ../recip12a.f90
recip12apot.o             : ../recip12apot.f90             ; $(F90) ../recip12apot.f90
recip12as.o               : ../recip12as.f90               ; $(F90) ../recip12as.f90
recip4.o                  : ../recip4.f90                  ; $(F90) ../recip4.f90
recip2Dp.o                : ../recip2Dp.f90                ; $(F90) ../recip2Dp.f90
recip2Dq.o                : ../recip2Dq.f90                ; $(F90) ../recip2Dq.f90
recip3Dp.o                : ../recip3Dp.f90                ; $(F90) ../recip3Dp.f90
recipsd.o                 : ../recipsd.f90                 ; $(F90) ../recipsd.f90
recipsd2.o                : ../recipsd2.f90                ; $(F90) ../recipsd2.f90
reciptrmd3.o              : ../reciptrmd3.f90              ; $(F90) ../reciptrmd3.f90
reciptrmdp.o              : ../reciptrmdp.f90              ; $(F90) ../reciptrmdp.f90
reinitialise.o            : ../reinitialise.f90            ; $(F90) ../reinitialise.f90
remove.o                  : ../remove.f90                  ; $(F90) ../remove.f90
reorder.o                 : ../reorder.f90                 ; $(F90) ../reorder.f90
resetcfgdefaults.o        : ../resetcfgdefaults.f90        ; $(F90) ../resetcfgdefaults.f90
rfind.o                   : ../rfind.f90                   ; $(F90) ../rfind.f90
rfindboth.o               : ../rfindboth.f90               ; $(F90) ../rfindboth.f90
rfohess.o                 : ../rfohess.f90                 ; $(F90) ../rfohess.f90
rfostep.o                 : ../rfostep.f90                 ; $(F90) ../rfostep.f90
rhoinv.o                  : ../rhoinv.f90                  ; $(F90) ../rhoinv.f90
rhtohex.o                 : ../rhtohex.f90                 ; $(F90) ../rhtohex.f90
rlist.o                   : ../rlist.f90                   ; $(F90) ../rlist.f90
rpair.o                   : ../rpair.f90                   ; $(F90) ../rpair.f90
rpair2.o                  : ../rpair2.f90                  ; $(F90) ../rpair2.f90
rsearch1D.o               : ../rsearch1D.f90               ; $(F90) ../rsearch1D.f90
rsearch2D.o               : ../rsearch2D.f90               ; $(F90) ../rsearch2D.f90
rsearch3D.o               : ../rsearch3D.f90               ; $(F90) ../rsearch3D.f90
rtlist.o                  : ../rtlist.f90                  ; $(F90) ../rtlist.f90
runmc.o                   : ../runmc.f90                   ; $(F90) ../runmc.f90
runneb.o                  : ../runneb.f90                  ; $(F90) ../runneb.f90
runsync.o                 : ../runsync.f90                 ; $(F90) ../runsync.f90
samemol.o                 : ../samemol.f90                 ; $(F90) ../samemol.f90
scaleevec.o               : ../scaleevec.f90               ; $(F90) ../scaleevec.f90
sec0.o                    : ../sec0.f90                    ; $(F90) ../sec0.f90
sec0f.o                   : ../sec0f.f90                   ; $(F90) ../sec0f.f90
sec3.o                    : ../sec3.f90                    ; $(F90) ../sec3.f90
sec3f.o                   : ../sec3f.f90                   ; $(F90) ../sec3f.f90
secondpass.o              : ../secondpass.f90              ; $(F90) ../secondpass.f90
selfterm.o                : ../selfterm.f90                ; $(F90) ../selfterm.f90
set2a.o                   : ../set2a.f90                   ; $(F90) ../set2a.f90
setatomnodes.o            : ../setatomnodes.f90            ; $(F90) ../setatomnodes.f90
setatomnodesbo.o          : ../setatomnodesbo.f90          ; $(F90) ../setatomnodesbo.f90
setbelashchenko.o         : ../setbelashchenko.f90         ; $(F90) ../setbelashchenko.f90
setbondorder.o            : ../setbondorder.f90            ; $(F90) ../setbondorder.f90
setbrenner1.o             : ../setbrenner1.f90             ; $(F90) ../setbrenner1.f90
setbrenner3.o             : ../setbrenner3.f90             ; $(F90) ../setbrenner3.f90
setbsmptr.o               : ../setbsmptr.f90               ; $(F90) ../setbsmptr.f90
setc6.o                   : ../setc6.f90                   ; $(F90) ../setc6.f90
setcellderv.o             : ../setcellderv.f90             ; $(F90) ../setcellderv.f90
setcfg.o                  : ../setcfg.f90                  ; $(F90) ../setcfg.f90
setcmm.o                  : ../setcmm.f90                  ; $(F90) ../setcmm.f90
setcosmo.o                : ../setcosmo.f90                ; $(F90) ../setcosmo.f90
setcosmoainv.o            : ../setcosmoainv.f90            ; $(F90) ../setcosmoainv.f90
setcosmoamat.o            : ../setcosmoamat.f90            ; $(F90) ../setcosmoamat.f90
setcosmobmat.o            : ../setcosmobmat.f90            ; $(F90) ../setcosmobmat.f90
setdcosmoamat.o           : ../setdcosmoamat.f90           ; $(F90) ../setdcosmoamat.f90
setdcosmobmat.o           : ../setdcosmobmat.f90           ; $(F90) ../setdcosmobmat.f90
setcutoffmax.o            : ../setcutoffmax.f90            ; $(F90) ../setcutoffmax.f90
setcutoffmaxbo.o          : ../setcutoffmaxbo.f90          ; $(F90) ../setcutoffmaxbo.f90
setdef.o                  : ../setdef.f90                  ; $(F90) ../setdef.f90
setdestroyptr.o           : ../setdestroyptr.f90           ; $(F90) ../setdestroyptr.f90
setdisp.o                 : ../setdisp.f90                 ; $(F90) ../setdisp.f90
setdistance.o             : ../setdistance.f90             ; $(F90) ../setdistance.f90
setdr2mat.o               : ../setdr2mat.f90               ; $(F90O1) ../setdr2mat.f90
seteam.o                  : ../seteam.f90                  ; $(F90) ../seteam.f90
setedip.o                 : ../setedip.f90                 ; $(F90) ../setedip.f90
setele.o                  : ../setele.f90                  ; $(F90) ../setele.f90
setewald.o                : ../setewald.f90                ; $(F90) ../setewald.f90
setfeshtmat.o             : ../setfeshtmat.f90             ; $(F90) ../setfeshtmat.f90
setfour.o                 : ../setfour.f90                 ; $(F90) ../setfour.f90
setkeywrd.o               : ../setkeywrd.f90               ; $(F90) ../setkeywrd.f90
setkpt.o                  : ../setkpt.f90                  ; $(F90) ../setkpt.f90
setktrmd3.o               : ../setktrmd3.f90               ; $(F90) ../setktrmd3.f90
setktrmdp.o               : ../setktrmdp.f90               ; $(F90) ../setktrmdp.f90
setktrm4.o                : ../setktrm4.f90                ; $(F90) ../setktrm4.f90
setlenn.o                 : ../setlenn.f90                 ; $(F90) ../setlenn.f90
setlib.o                  : ../setlib.f90                  ; $(F90) ../setlib.f90
setlist3.o                : ../setlist3.f90                ; $(F90) ../setlist3.f90
setlist4.o                : ../setlist4.f90                ; $(F90) ../setlist4.f90
setlist6.o                : ../setlist6.f90                ; $(F90) ../setlist6.f90
setlistoop.o              : ../setlistoop.f90              ; $(F90) ../setlistoop.f90
setmaxcell1D.o            : ../setmaxcell1D.f90            ; $(F90) ../setmaxcell1D.f90
setmd.o                   : ../setmd.f90                   ; $(F90) ../setmd.f90
setmol.o                  : ../setmol.f90                  ; $(F90) ../setmol.f90
setmols.o                 : ../setmols.f90                 ; $(F90) ../setmols.f90
setmoldef.o               : ../setmoldef.f90               ; $(F90) ../setmoldef.f90
setmoldef12a.o            : ../setmoldef12a.f90            ; $(F90) ../setmoldef12a.f90
setmoveptr.o              : ../setmoveptr.f90              ; $(F90) ../setmoveptr.f90
setoccptr.o               : ../setoccptr.f90               ; $(F90) ../setoccptr.f90
setoccptrs.o              : ../setoccptrs.f90              ; $(F90) ../setoccptrs.f90
setoccshptr.o             : ../setoccshptr.f90             ; $(F90) ../setoccshptr.f90
setoccshptrs.o            : ../setoccshptrs.f90            ; $(F90) ../setoccshptrs.f90
setpolar.o                : ../setpolar.f90                ; $(F90) ../setpolar.f90
setpote.o                 : ../setpote.f90                 ; $(F90) ../setpote.f90
setquasiharm.o            : ../setquasiharm.f90            ; $(F90) ../setquasiharm.f90
setratiom.o               : ../setratiom.f90               ; $(F90) ../setratiom.f90
setreaxFF.o               : ../setreaxFF.f90               ; $(F90) ../setreaxFF.f90
setreaxffQ.o              : ../setreaxffQ.f90              ; $(F90) ../setreaxffQ.f90
setreaxffQiter.o          : ../setreaxffQiter.f90          ; $(F90) ../setreaxffQiter.f90
setregiontrans.o          : ../setregiontrans.f90          ; $(F90) ../setregiontrans.f90
setrotateptr.o            : ../setrotateptr.f90            ; $(F90) ../setrotateptr.f90
setsas.o                  : ../setsas.f90                  ; $(F90) ../setsas.f90
setsegweight.o            : ../setsegweight.f90            ; $(F90) ../setsegweight.f90
setdsegweight.o           : ../setdsegweight.f90           ; $(F90) ../setdsegweight.f90
setqonsas.o               : ../setqonsas.f90               ; $(F90) ../setqonsas.f90
setstrainptr.o            : ../setstrainptr.f90            ; $(F90) ../setstrainptr.f90
setsix.o                  : ../setsix.f90                  ; $(F90) ../setsix.f90
setslice.o                : ../setslice.f90                ; $(F90) ../setslice.f90
setspatial.o              : ../setspatial.f90              ; $(F90) ../setspatial.f90
setspatialbo.o            : ../setspatialbo.f90            ; $(F90) ../setspatialbo.f90
setspatialcells.o         : ../setspatialcells.f90         ; $(F90) ../setspatialcells.f90
setspec.o                 : ../setspec.f90                 ; $(F90) ../setspec.f90
setspring.o               : ../setspring.f90               ; $(F90) ../setspring.f90
setswapptr.o              : ../setswapptr.f90              ; $(F90) ../setswapptr.f90
settaper.o                : ../settaper.f90                ; $(F90) ../settaper.f90
setthree.o                : ../setthree.f90                ; $(F90) ../setthree.f90
settings.o                : ../settings.f90                ; $(F90) ../settings.f90
settorcubic.o             : ../settorcubic.f90             ; $(F90O1) ../settorcubic.f90
settricubic.o             : ../settricubic.f90             ; $(F90O1) ../settricubic.f90
settricubic2.o            : ../settricubic2.f90            ; $(F90O1) ../settricubic2.f90
setuff.o                  : ../setuff.f90                  ; $(F90) ../setuff.f90
setup.o                   : ../setup.f90                   ; $(F90) ../setup.f90
setvbon.o                 : ../setvbon.f90                 ; $(F90) ../setvbon.f90
setzmolslice.o            : ../setzmolslice.f90            ; $(F90) ../setzmolslice.f90
sitepot.o                 : ../sitepot.f90                 ; $(F90) ../sitepot.f90
sixbody.o                 : ../sixbody.f90                 ; $(F90) ../sixbody.f90
sixlist.o                 : ../sixlist.f90                 ; $(F90) ../sixlist.f90
sixmc.o                   : ../sixmc.f90                   ; $(F90) ../sixmc.f90
sixmd.o                   : ../sixmd.f90                   ; $(F90) ../sixmd.f90
sixmds.o                  : ../sixmds.f90                  ; $(F90) ../sixmds.f90
sixnos.o                  : ../sixnos.f90                  ; $(F90) ../sixnos.f90
sixp.o                    : ../sixp.f90                    ; $(F90) ../sixp.f90
sixsd.o                   : ../sixsd.f90                   ; $(F90) ../sixsd.f90
sixsd2.o                  : ../sixsd2.f90                  ; $(F90) ../sixsd2.f90
sixstrterms.o             : ../sixstrterms.f90             ; $(F90) ../sixstrterms.f90
smbody.o                  : ../smbody.f90                  ; $(F90) ../smbody.f90
solvation.o               : ../solvation.f90               ; $(F90) ../solvation.f90
sort.o                    : ../sort.f90                    ; $(F90) ../sort.f90
sort1sym.o                : ../sort1sym.f90                ; $(F90) ../sort1sym.f90
sort2a.o                  : ../sort2a.f90                  ; $(F90) ../sort2a.f90
special.o                 : ../special.f90                 ; $(F90) ../special.f90
speclibw.o                : ../speclibw.f90                ; $(F90) ../speclibw.f90
specword.o                : ../specword.f90                ; $(F90) ../specword.f90
spline.o                  : ../spline.f90                  ; $(F90) ../spline.f90
stolc.o                   : ../stolc.f90                   ; $(F90) ../stolc.f90
stouc.o                   : ../stouc.f90                   ; $(F90) ../stouc.f90
strain.o                  : ../strain.f90                  ; $(F90) ../strain.f90
strfin.o                  : ../strfin.f90                  ; $(F90) ../strfin.f90
strsym.o                  : ../strsym.f90                  ; $(F90) ../strsym.f90
strword.o                 : ../strword.f90                 ; $(F90) ../strword.f90
sumderv1.o                : ../sumderv1.f90                ; $(F90) ../sumderv1.f90
sumderv2.o                : ../sumderv2.f90                ; $(F90) ../sumderv2.f90
sumderv2f.o               : ../sumderv2f.f90               ; $(F90) ../sumderv2f.f90
sumderv2s.o               : ../sumderv2s.f90               ; $(F90) ../sumderv2s.f90
super.o                   : ../super.f90                   ; $(F90) ../super.f90
surfaceenergy.o           : ../surfaceenergy.f90           ; $(F90) ../surfaceenergy.f90
surfword.o                : ../surfword.f90                ; $(F90) ../surfword.f90
symbind.o                 : ../symbind.f90                 ; $(F90) ../symbind.f90
symderv2.o                : ../symderv2.f90                ; $(F90) ../symderv2.f90
symdervrot.o              : ../symdervrot.f90              ; $(F90) ../symdervrot.f90
symmet.o                  : ../symmet.f90                  ; $(F90) ../symmet.f90
symoff.o                  : ../symoff.f90                  ; $(F90) ../symoff.f90
symout.o                  : ../symout.f90                  ; $(F90) ../symout.f90
sympot.o                  : ../sympot.f90                  ; $(F90) ../sympot.f90
symprod.o                 : ../symprod.f90                 ; $(F90) ../symprod.f90
syncforce.o               : ../syncforce.f90               ; $(F90) ../syncforce.f90
switch.o                  : ../switch.f90                  ; $(F90) ../switch.f90
taper.o                   : ../taper.f90                   ; $(F90) ../taper.f90
tfunc.o                   : ../tfunc.f90                   ; $(F90) ../tfunc.f90
thermalconductivity.o     : ../thermalconductivity.f90     ; $(F90) ../thermalconductivity.f90
threenos.o                : ../threenos.f90                ; $(F90) ../threenos.f90
three12.o                 : ../three12.f90                 ; $(F90) ../three12.f90
threebody.o               : ../threebody.f90               ; $(F90O1) ../threebody.f90
threelist.o               : ../threelist.f90               ; $(F90) ../threelist.f90
threep.o                  : ../threep.f90                  ; $(F90) ../threep.f90
threemc.o                 : ../threemc.f90                 ; $(F90) ../threemc.f90
threemd.o                 : ../threemd.f90                 ; $(F90) ../threemd.f90
threemds.o                : ../threemds.f90                ; $(F90) ../threemds.f90
threesd.o                 : ../threesd.f90                 ; $(F90) ../threesd.f90
threesd2.o                : ../threesd2.f90                ; $(F90) ../threesd2.f90
threestrterms.o           : ../threestrterms.f90           ; $(F90) ../threestrterms.f90
three0d3.o                : ../three0d3.f90                ; $(F90) ../three0d3.f90
three3d3.o                : ../three3d3.f90                ; $(F90) ../three3d3.f90
torsion.o                 : ../torsion.f90                 ; $(F90) ../torsion.f90
transmat.o                : ../transmat.f90                ; $(F90) ../transmat.f90
trap_sigint.o             : ../trap_sigint.f90             ; $(F90) ../trap_sigint.f90
twobody.o                 : ../twobody.f90                 ; $(F90) ../twobody.f90
twobodymd.o               : ../twobodymd.f90               ; $(F90) ../twobodymd.f90
twoden.o                  : ../twoden.f90                  ; $(F90) ../twoden.f90
uncell.o                  : ../uncell.f90                  ; $(F90) ../uncell.f90
uncentre.o                : ../uncentre.f90                ; $(F90) ../uncentre.f90
unfreeze.o                : ../unfreeze.f90                ; $(F90) ../unfreeze.f90
updatesas.o               : ../updatesas.f90               ; $(F90) ../updatesas.f90
validtriad.o              : ../validtriad.f90              ; $(F90) ../validtriad.f90
volume.o                  : ../volume.f90                  ; $(F90) ../volume.f90
wcoord.o                  : ../wcoord.f90                  ; $(F90) ../wcoord.f90
wolfself.o                : ../wolfself.f90                ; $(F90) ../wolfself.f90
wolfselftrial.o           : ../wolfselftrial.f90           ; $(F90) ../wolfselftrial.f90
worsy.o                   : ../worsy.f90                   ; $(F90) ../worsy.f90
wtof.o                    : ../wtof.f90                    ; $(F90) ../wtof.f90
x0strain.o                : ../x0strain.f90                ; $(F90) ../x0strain.f90
x0tostr.o                 : ../x0tostr.f90                 ; $(F90) ../x0tostr.f90
x0tostrcentroid.o         : ../x0tostrcentroid.f90         ; $(F90) ../x0tostrcentroid.f90
x0tostrtrial.o            : ../x0tostrtrial.f90            ; $(F90) ../x0tostrtrial.f90
xctox0.o                  : ../xctox0.f90                  ; $(F90) ../xctox0.f90
#
qtpie.o                   : ../qtpie.f90                   ; $(F90) ../qtpie.f90
qtmin.o                   : ../qtmin.f90                   ; $(F90) ../qtmin.f90
ctfunct.o                 : ../ctfunct.f90                 ; $(F90) ../ctfunct.f90
reaxffctfunct.o           : ../reaxffctfunct.f90           ; $(F90) ../reaxffctfunct.f90
m_conjgr.o                : ../m_conjgr.f90                ; $(F90) ../m_conjgr.f90
screenct.o                : ../screenct.f90                ; $(F90) ../screenct.f90
#
gulpcml.o                 : ../gulpcml.F90 $(FOX_OBJ)                  ; $(F90) $(FoX_fcflags) ../gulpcml.F90
gulpcml_md.o              : gulpcml.o ../gulpcml_md.F90 $(FOX_OBJ)     ; $(F90) $(FoX_fcflags) ../gulpcml_md.F90
gulpcml_phonon.o          : gulpcml.o ../gulpcml_phonon.F90 $(FOX_OBJ) ; $(F90) $(FoX_fcflags) ../gulpcml_phonon.F90
gulpcml_props.o           : gulpcml.o ../gulpcml_props.F90 $(FOX_OBJ)  ; $(F90) $(FoX_fcflags) ../gulpcml_props.F90
#
#  C routines
#
csignal.o                 : ../csignal.c       	           ; $(CC) ../csignal.c
#
#  Plumed code
#
biasexchange.o            : ../Plumed/biasexchange.c       ; $(CC) ../Plumed/biasexchange.c
hills.o                   : ../Plumed/hills.c              ; $(CC) ../Plumed/hills.c
metadyn.o                 : ../Plumed/metadyn.c            ; $(CC) ../Plumed/metadyn.c
ptmetad.o                 : ../Plumed/ptmetad.c            ; $(CC) ../Plumed/ptmetad.c
read_restraint.o          : ../Plumed/read_restraint.c     ; $(CC) ../Plumed/read_restraint.c
restraint.o               : ../Plumed/restraint.c          ; $(CC) ../Plumed/restraint.c
restraint_alfabeta.o      : ../Plumed/restraint_alfabeta.c ; $(CC) ../Plumed/restraint_alfabeta.c
restraint_alpharmsd.o     : ../Plumed/restraint_alpharmsd.c; $(CC) ../Plumed/restraint_alpharmsd.c
restraint_angle.o         : ../Plumed/restraint_angle.c    ; $(CC) ../Plumed/restraint_angle.c
restraint_antibetarmsd.o  : ../Plumed/restraint_antibetarmsd.c; $(CC) ../Plumed/restraint_antibetarmsd.c
restraint_coord.o         : ../Plumed/restraint_coord.c    ; $(CC) ../Plumed/restraint_coord.c
restraint_dihcor.o        : ../Plumed/restraint_dihcor.c   ; $(CC) ../Plumed/restraint_dihcor.c
restraint_dipole.o        : ../Plumed/restraint_dipole.c   ; $(CC) ../Plumed/restraint_dipole.c
restraint_dist.o          : ../Plumed/restraint_dist.c     ; $(CC) ../Plumed/restraint_dist.c
restraint_elstpot.o       : ../Plumed/restraint_elstpot.c  ; $(CC) ../Plumed/restraint_elstpot.c
restraint_energy.o        : ../Plumed/restraint_energy.c   ; $(CC) ../Plumed/restraint_energy.c
restraint_hbonds.o        : ../Plumed/restraint_hbonds.c   ; $(CC) ../Plumed/restraint_hbonds.c
restraint_helix.o         : ../Plumed/restraint_helix.c    ; $(CC) ../Plumed/restraint_helix.c
restraint_mindist.o       : ../Plumed/restraint_mindist.c  ; $(CC) ../Plumed/restraint_mindist.c
restraint_parabetarmsd.o  : ../Plumed/restraint_parabetarmsd.c; $(CC) ../Plumed/restraint_parabetarmsd.c
restraint_puckering.o     : ../Plumed/restraint_puckering.c; $(CC) ../Plumed/restraint_puckering.c
restraint_position.o      : ../Plumed/restraint_position.c ; $(CC) ../Plumed/restraint_position.c
restraint_rgyr.o          : ../Plumed/restraint_rgyr.c     ; $(CC) ../Plumed/restraint_rgyr.c
restraint_rmsdtor.o       : ../Plumed/restraint_rmsdtor.c  ; $(CC) ../Plumed/restraint_rmsdtor.c
restraint_spath.o         : ../Plumed/restraint_spath.c    ; $(CC) ../Plumed/restraint_spath.c
restraint_torsion.o       : ../Plumed/restraint_torsion.c  ; $(CC) ../Plumed/restraint_torsion.c
restraint_waterbridge.o   : ../Plumed/restraint_waterbridge.c; $(CC) ../Plumed/restraint_waterbridge.c
restraint_zpath.o         : ../Plumed/restraint_zpath.c    ; $(CC) ../Plumed/restraint_zpath.c
testderivatives.o         : ../Plumed/testderivatives.c    ; $(CC) ../Plumed/testderivatives.c
images.o                  : ../Plumed/images.f             ; $(F90) ../Plumed/images.f
#
#  Automatically generated dependences from mkmf
#
CS_dummy.o: ../CS_dummy.F90 datatypes.o
addframe2arc.o: ../addframe2arc.f90 modules.o
addshell.o: ../addshell.f90 modules.o iomod.o
angle.o: ../angle.f90 modules.o iomod.o
anneal.o: ../anneal.f90 modules.o iomod.o
aocn2.o: ../aocn2.f90 modules.o iomod.o
background.o: ../background.f90 modules.o
banner.o: ../banner.F90 iomod.o modules.o
bcgsolve.o: ../bcgsolve.f90 datatypes.o iomod.o modules.o
binary.o: ../binary.f90 modules.o datatypes.o
blas.o: ../blas.f90 datatypes.o
bond.o: ../bond.f90 modules.o iomod.o
bonded.o: ../bonded.f90 modules.o
bonded2donor.o: ../bonded2donor.f90 modules.o
bonded3.o: ../bonded3.f90 modules.o
bonded3c.o: ../bonded3c.f90 modules.o
bondint.o: ../bondint.f90 modules.o
bondorder.o: ../bondorder.f90 datatypes.o modules.o iomod.o
bondorder12.o: ../bondorder12.f90 datatypes.o modules.o iomod.o
bondordermd.o: ../bondordermd.f90 datatypes.o modules.o iomod.o
bondordersd2.o: ../bondordersd2.f90 datatypes.o modules.o iomod.o
bondpot.o: ../bondpot.f90 modules.o
bondq.o: ../bondq.f90 modules.o iomod.o
bondqtrial.o: ../bondqtrial.f90 modules.o iomod.o
borncharge.o: ../borncharge.f90 modules.o gulpcml.o iomod.o gulpcml_props.o
boself.o: ../boself.f90 datatypes.o modules.o
boselftrial.o: ../boselftrial.f90 datatypes.o modules.o
boword.o: ../boword.f90 modules.o iomod.o
brenner.o: ../brenner.f90 datatypes.o modules.o iomod.o
brenner12.o: ../brenner12.f90 datatypes.o modules.o iomod.o
brennermd.o: ../brennermd.f90 datatypes.o modules.o iomod.o
brennersd2.o: ../brennersd2.f90 datatypes.o modules.o iomod.o
buck4.o: ../buck4.f90 modules.o iomod.o
calcpht.o: ../calcpht.f90 modules.o datatypes.o
cart2frac.o: ../cart2frac.f90 datatypes.o modules.o
cdabs.o: ../cdabs.f90 datatypes.o
cell.o: ../cell.f90 modules.o
cellcheck.o: ../cellcheck.f90 datatypes.o
celldrv.o: ../celldrv.f90 modules.o
cellfhr.o: ../cellfhr.f90 datatypes.o
celltype.o: ../celltype.f90 datatypes.o modules.o
centre.o: ../centre.f90 modules.o
changemaxallnearseg.o: ../changemaxallnearseg.f90 modules.o reallocate.o
changemaxat.o: ../changemaxat.f90 modules.o reallocate.o
changemaxatcosmo.o: ../changemaxatcosmo.f90 modules.o reallocate.o
changemaxatompernode.o: ../changemaxatompernode.f90 modules.o reallocate.o
changemaxatompernodebo.o: ../changemaxatompernodebo.f90 modules.o reallocate.o
changemaxatot.o: ../changemaxatot.f90 modules.o reallocate.o
changemaxbond.o: ../changemaxbond.f90 modules.o reallocate.o
changemaxbondq.o: ../changemaxbondq.f90 modules.o reallocate.o
changemaxbondvec.o: ../changemaxbondvec.f90 modules.o reallocate.o
changemaxccspec.o: ../changemaxccspec.f90 modules.o reallocate.o
changemaxcellpernode.o: ../changemaxcellpernode.f90 modules.o reallocate.o
changemaxcellpernodebo.o: ../changemaxcellpernodebo.f90 modules.o reallocate.o
changemaxcfg.o: ../changemaxcfg.f90 modules.o m_pr.o m_pdfneutron.o reallocate.o
changemaxcon.o: ../changemaxcon.f90 modules.o reallocate.o
changemaxconnect.o: ../changemaxconnect.f90 modules.o reallocate.o
changemaxcontot.o: ../changemaxcontot.f90 modules.o reallocate.o
changemaxd1.o: ../changemaxd1.f90 modules.o reallocate.o
changemaxd2.o: ../changemaxd2.f90 modules.o reallocate.o
changemaxd2q.o: ../changemaxd2q.f90 modules.o reallocate.o
changemaxdcon.o: ../changemaxdcon.f90 modules.o reallocate.o
changemaxdef.o: ../changemaxdef.f90 modules.o reallocate.o
changemaxdis.o: ../changemaxdis.f90 modules.o reallocate.o
changemaxeamden.o: ../changemaxeamden.f90 modules.o reallocate.o
changemaxeamfnspec.o: ../changemaxeamfnspec.f90 modules.o reallocate.o
changemaxeamspec.o: ../changemaxeamspec.f90 modules.o reallocate.o
changemaxedipspec.o: ../changemaxedipspec.f90 modules.o reallocate.o
changemaxextrapol.o: ../changemaxextrapol.f90 modules.o reallocate.o
changemaxfcon.o: ../changemaxfcon.f90 modules.o reallocate.o
changemaxfgrad.o: ../changemaxfgrad.f90 modules.o reallocate.o
changemaxfit.o: ../changemaxfit.f90 modules.o reallocate.o
changemaxfkpt.o: ../changemaxfkpt.f90 modules.o reallocate.o
changemaxfor.o: ../changemaxfor.f90 modules.o reallocate.o
changemaxfreqat.o: ../changemaxfreqat.f90 datatypes.o modules.o reallocate.o
changemaxfstress.o: ../changemaxfstress.f90 modules.o reallocate.o
changemaxgbcfg.o: ../changemaxgbcfg.f90 modules.o reallocate.o
changemaxgcmcmol.o: ../changemaxgcmcmol.f90 modules.o reallocate.o
changemaxhold.o: ../changemaxhold.f90 reallocate.o modules.o
changemaxiltor.o: ../changemaxiltor.f90 modules.o reallocate.o
changemaxkpt.o: ../changemaxkpt.f90 modules.o reallocate.o
changemaxkvec.o: ../changemaxkvec.f90 modules.o reallocate.o
changemaxlib.o: ../changemaxlib.f90 modules.o reallocate.o
changemaxlist3.o: ../changemaxlist3.f90 reallocate.o modules.o
changemaxlist4.o: ../changemaxlist4.f90 modules.o reallocate.o
changemaxlist6.o: ../changemaxlist6.f90 modules.o reallocate.o
changemaxmany.o: ../changemaxmany.f90 modules.o reallocate.o
changemaxmcswapspec.o: ../changemaxmcswapspec.f90 modules.o reallocate.o
changemaxmol.o: ../changemaxmol.f90 modules.o reallocate.o
changemaxn3a.o: ../changemaxn3a.f90 reallocate.o modules.o
changemaxn3bondnono.o: ../changemaxn3bondnono.f90 reallocate.o modules.o
changemaxn3f.o: ../changemaxn3f.f90 reallocate.o modules.o
changemaxnboa.o: ../changemaxnboa.f90 modules.o reallocate.o
changemaxnbopot.o: ../changemaxnbopot.f90 modules.o reallocate.o
changemaxnboq.o: ../changemaxnboq.f90 modules.o reallocate.o
changemaxnboq0.o: ../changemaxnboq0.f90 modules.o reallocate.o
changemaxnbor.o: ../changemaxnbor.f90 modules.o reallocate.o
changemaxndistancetotal.o: ../changemaxndistancetotal.f90 modules.o reallocate.o
changemaxneamfnnumeric.o: ../changemaxneamfnnumeric.f90 modules.o reallocate.o
changemaxnearseg.o: ../changemaxnearseg.f90 modules.o reallocate.o
changemaxnebreplicatot.o: ../changemaxnebreplicatot.f90 modules.o reallocate.o
changemaxnobo.o: ../changemaxnobo.f90 modules.o reallocate.o
changemaxnppa.o: ../changemaxnppa.f90 modules.o reallocate.o
changemaxnpts.o: ../changemaxnpts.f90 modules.o reallocate.o
changemaxnptsh.o: ../changemaxnptsh.f90 modules.o reallocate.o
changemaxnptstot.o: ../changemaxnptstot.f90 modules.o reallocate.o
changemaxnpwt.o: ../changemaxnpwt.f90 modules.o reallocate.o
changemaxnqstepfit.o: ../changemaxnqstepfit.f90 reallocate.o modules.o
changemaxnset.o: ../changemaxnset.f90 modules.o reallocate.o
changemaxnspcellattot.o: ../changemaxnspcellattot.f90 reallocate.o modules.o
changemaxnspcellattotbo.o: ../changemaxnspcellattotbo.f90 reallocate.o modules.o
changemaxnwstepfit.o: ../changemaxnwstepfit.f90 reallocate.o modules.o
changemaxobs.o: ../changemaxobs.f90 modules.o reallocate.o
changemaxobsmode.o: ../changemaxobsmode.f90 modules.o reallocate.o
changemaxone.o: ../changemaxone.f90 reallocate.o modules.o
changemaxplanepot.o: ../changemaxplanepot.f90 modules.o reallocate.o
changemaxpot.o: ../changemaxpot.f90 modules.o reallocate.o
changemaxpotsites.o: ../changemaxpotsites.f90 modules.o reallocate.o
changemaxppt.o: ../changemaxppt.f90 modules.o reallocate.o
changemaxproj.o: ../changemaxproj.f90 modules.o reallocate.o
changemaxproji.o: ../changemaxproji.f90 modules.o reallocate.o
changemaxpts.o: ../changemaxpts.f90 reallocate.o modules.o
changemaxqatoms.o: ../changemaxqatoms.f90 modules.o reallocate.o
changemaxqatoms2.o: ../changemaxqatoms2.f90 modules.o reallocate.o
changemaxqvector.o: ../changemaxqvector.f90 reallocate.o modules.o
changemaxr1at.o: ../changemaxr1at.f90 modules.o reallocate.o
changemaxr2at.o: ../changemaxr2at.f90 modules.o reallocate.o
changemaxreaxfffixqspec.o: ../changemaxreaxfffixqspec.f90 modules.o reallocate.o
changemaxreaxffspec.o: ../changemaxreaxffspec.f90 modules.o reallocate.o
changemaxreaxffval3.o: ../changemaxreaxffval3.f90 modules.o reallocate.o
changemaxregion.o: ../changemaxregion.f90 modules.o reallocate.o
changemaxsa.o: ../changemaxsa.f90 modules.o reallocate.o
changemaxsasparticles.o: ../changemaxsasparticles.f90 modules.o reallocate.o
changemaxsasparticlespart.o: ../changemaxsasparticlespart.f90 modules.o reallocate.o
changemaxsix.o: ../changemaxsix.f90 reallocate.o modules.o
changemaxskpt.o: ../changemaxskpt.f90 modules.o reallocate.o
changemaxspcell.o: ../changemaxspcell.f90 modules.o reallocate.o
changemaxspcellbo.o: ../changemaxspcellbo.f90 modules.o reallocate.o
changemaxspec.o: ../changemaxspec.f90 modules.o m_pr.o reallocate.o
changemaxsymop.o: ../changemaxsymop.f90 modules.o reallocate.o
changemaxtdfield.o: ../changemaxtdfield.f90 modules.o reallocate.o
changemaxthb.o: ../changemaxthb.f90 modules.o reallocate.o
changemaxtitle.o: ../changemaxtitle.f90 modules.o reallocate.o
changemaxtrialatom.o: ../changemaxtrialatom.f90 modules.o reallocate.o
changemaxuffspec.o: ../changemaxuffspec.f90 modules.o reallocate.o
changemaxvacint.o: ../changemaxvacint.f90 modules.o reallocate.o
changemaxvar.o: ../changemaxvar.f90 modules.o reallocate.o
changemaxvectorpair.o: ../changemaxvectorpair.f90 modules.o reallocate.o
changemaxword.o: ../changemaxword.f90 modules.o reallocate.o
channels.o: ../channels.F90 iomod.o modules.o
checkdefshspec.o: ../checkdefshspec.f90 modules.o
checkpot.o: ../checkpot.f90 modules.o iomod.o
chemshellenergy.o: ../chemshellenergy.F90 modules.o iomod.o
chvo.o: ../chvo.f90 datatypes.o
closestimage.o: ../closestimage.f90 datatypes.o modules.o
collect.o: ../collect.f90 datatypes.o
compressd1.o: ../compressd1.f90 datatypes.o
compressd2.o: ../compressd2.f90 datatypes.o
compressd3.o: ../compressd3.f90 datatypes.o
connectunwrap.o: ../connectunwrap.f90 modules.o
connectwrap.o: ../connectwrap.f90 modules.o
connectwraptrial.o: ../connectwraptrial.f90 modules.o
copycfg.o: ../copycfg.f90 modules.o m_pr.o reallocate.o
cosmoaiiderv.o: ../cosmoaiiderv.f90 datatypes.o modules.o
cosmoaiizero.o: ../cosmoaiizero.f90 datatypes.o modules.o
cosmoaijderv.o: ../cosmoaijderv.f90 datatypes.o modules.o
cosmoaijzero.o: ../cosmoaijzero.f90 datatypes.o modules.o
cosmoamatdadd.o: ../cosmoamatdadd.f90 modules.o
cosmoderv.o: ../cosmoderv.f90 modules.o iomod.o reallocate.o
costfn.o: ../costfn.f90 modules.o datatypes.o iomod.o
cputime.o: ../cputime.F90 datatypes.o
create.o: ../create.f90 modules.o
createsphere.o: ../createsphere.f90 datatypes.o modules.o
cscheck.o: ../cscheck.f90 modules.o iomod.o
csignal.o: ../csignal.c
ctfunct.o: ../ctfunct.f90 iomod.o modules.o
cutscheck.o: ../cutscheck.f90 modules.o iomod.o
d1charge.o: ../d1charge.f90 modules.o
d1charge3.o: ../d1charge3.f90 modules.o
d1charges.o: ../d1charges.f90 modules.o
d2charge.o: ../d2charge.f90 modules.o
d2charge2D.o: ../d2charge2D.f90 modules.o
d2charge3.o: ../d2charge3.f90 modules.o
d2charge3p.o: ../d2charge3p.f90 modules.o
d2chargep.o: ../d2chargep.f90 modules.o
d2chargep2D.o: ../d2chargep2D.f90 modules.o
datatypes.o: ../datatypes.F90
datetime.o: ../datetime.f90 iomod.o modules.o
dcharge.o: ../dcharge.f90 modules.o iomod.o
dcharges.o: ../dcharges.f90 modules.o iomod.o
dcosmowt.o: ../dcosmowt.f90 modules.o
defect.o: ../defect.f90 modules.o iomod.o
defener.o: ../defener.f90 modules.o
defequ.o: ../defequ.f90 modules.o
defequ2a.o: ../defequ2a.f90 modules.o
deffreq.o: ../deffreq.f90 modules.o iomod.o
deffun.o: ../deffun.f90 modules.o
defoin.o: ../defoin.f90 modules.o iomod.o
defopt.o: ../defopt.f90 modules.o iomod.o reallocate.o
defout.o: ../defout.f90 modules.o iomod.o
defpot.o: ../defpot.f90 modules.o iomod.o
defsec.o: ../defsec.f90 modules.o iomod.o
defsym.o: ../defsym.f90 modules.o iomod.o
deftmat.o: ../deftmat.f90 modules.o iomod.o
defword.o: ../defword.f90 datatypes.o modules.o iomod.o reallocate.o
density0.o: ../density0.f90 modules.o
density112.o: ../density112.f90 modules.o
density11s2.o: ../density11s2.f90 modules.o
density12a2.o: ../density12a2.f90 modules.o
density12as2.o: ../density12as2.f90 modules.o
density2a.o: ../density2a.f90 modules.o
density3.o: ../density3.f90 modules.o datatypes.o
density3s.o: ../density3s.f90 modules.o datatypes.o
densitysd.o: ../densitysd.f90 modules.o datatypes.o
derfc.o: ../derfc.f90 datatypes.o
destroy.o: ../destroy.f90 modules.o
diagdyn.o: ../diagdyn.f90 modules.o
dipole3D.o: ../dipole3D.f90 modules.o
dipole3Dtrial.o: ../dipole3Dtrial.f90 modules.o
dipolesd.o: ../dipolesd.f90 modules.o
distance.o: ../distance.f90 modules.o iomod.o
dlamch.o: ../dlamch.f90
dpoccon.o: ../dpoccon.f90 modules.o
dspecial.o: ../dspecial.f90 modules.o iomod.o
dtotcosmowt.o: ../dtotcosmowt.f90 modules.o
dump34.o: ../dump34.f90 modules.o
dumpbo.o: ../dumpbo.f90 modules.o
dumpdur.o: ../dumpdur.f90 modules.o m_pr.o
dumpgen.o: ../dumpgen.f90 modules.o gulpcml.o
dumppot.o: ../dumppot.f90 modules.o
dutils.o: ../dutils.f90 modules.o
dynamic.o: ../dynamic.f90 modules.o
dynamicn.o: ../dynamicn.f90 modules.o iomod.o
dynamicn_defect.o: ../dynamicn_defect.f90 modules.o
eamfnderv.o: ../eamfnderv.f90 datatypes.o modules.o
eamrho.o: ../eamrho.f90 modules.o
eamscalescrho.o: ../eamscalescrho.f90 modules.o
edip2body.o: ../edip2body.f90 modules.o
edip3body.o: ../edip3body.f90 modules.o datatypes.o
edipcn.o: ../edipcn.f90 modules.o datatypes.o
edipmd.o: ../edipmd.f90 datatypes.o modules.o iomod.o
eem.o: ../eem.f90 modules.o iomod.o
einstein.o: ../einstein.f90 modules.o
einsteinsd2.o: ../einsteinsd2.f90 modules.o
einsteintrial.o: ../einsteintrial.f90 modules.o
eispack.o: ../eispack.f90 datatypes.o
electricfield.o: ../electricfield.f90 modules.o
electricfieldparts.o: ../electricfieldparts.f90 modules.o
electricfieldpotl.o: ../electricfieldpotl.f90 modules.o
electricfieldtrial.o: ../electricfieldtrial.f90 modules.o
emfunc.o: ../emfunc.f90 datatypes.o
endstring.o: ../endstring.f90 datatypes.o
energy.o: ../energy.f90 modules.o iomod.o
epot.o: ../epot.f90 modules.o datatypes.o
epslon.o: ../epslon.f90
equpos.o: ../equpos.f90 modules.o iomod.o
exchng.o: ../exchng.f90 datatypes.o
fefunct.o: ../fefunct.f90 modules.o
fefunctn.o: ../fefunctn.f90 modules.o
fenergy0.o: ../fenergy0.f90 modules.o iomod.o
fenergy3.o: ../fenergy3.f90 modules.o iomod.o
finddef.o: ../finddef.f90 modules.o
findrdcmax.o: ../findrdcmax.f90 modules.o
finish.o: ../finish.f90 modules.o iomod.o reallocate.o
firstpass.o: ../firstpass.f90 modules.o
fit.o: ../fit.f90 modules.o iomod.o m_simplex.o
fitbfgs.o: ../fitbfgs.f90 modules.o iomod.o
fitcon.o: ../fitcon.f90 modules.o
fitfun.o: ../fitfun.f90 modules.o
fitfunct.o: ../fitfunct.f90 modules.o iomod.o
fitgrad.o: ../fitgrad.f90 modules.o
fitmin.o: ../fitmin.f90 modules.o iomod.o
fitword.o: ../fitword.f90 modules.o iomod.o
force.o: ../force.f90 modules.o
forcetrial.o: ../forcetrial.f90 modules.o
formula.o: ../formula.f90 datatypes.o modules.o
four0d3.o: ../four0d3.f90 modules.o
four12.o: ../four12.f90 modules.o
four3d3.o: ../four3d3.f90 modules.o
fourbody.o: ../fourbody.f90 modules.o iomod.o datatypes.o
fourlist.o: ../fourlist.f90 modules.o iomod.o
fourmc.o: ../fourmc.f90 modules.o iomod.o
fourmd.o: ../fourmd.f90 modules.o datatypes.o iomod.o
fourmds.o: ../fourmds.f90 modules.o iomod.o
fournos.o: ../fournos.f90 modules.o iomod.o
fouroop.o: ../fouroop.f90 modules.o iomod.o
fouroop12.o: ../fouroop12.f90 modules.o
fouroopmc.o: ../fouroopmc.f90 modules.o iomod.o
fouroopmd.o: ../fouroopmd.f90 modules.o iomod.o
fouroopmds.o: ../fouroopmds.f90 modules.o iomod.o
fouroopp.o: ../fouroopp.f90 modules.o
fouroopsd.o: ../fouroopsd.f90 modules.o datatypes.o iomod.o
fouroopsd2.o: ../fouroopsd2.f90 modules.o iomod.o
fourp.o: ../fourp.f90 modules.o
foursd.o: ../foursd.f90 modules.o datatypes.o iomod.o
foursd2.o: ../foursd2.f90 modules.o iomod.o
fourstrterms.o: ../fourstrterms.f90 datatypes.o
ftow.o: ../ftow.f90 datatypes.o modules.o
funct.o: ../funct.f90 modules.o
functn.o: ../functn.f90 modules.o iomod.o
functneb.o: ../functneb.f90 modules.o
functnf.o: ../functnf.f90 modules.o
functsync.o: ../functsync.f90 modules.o
gabcfg.o: ../gabcfg.f90 modules.o iomod.o
gacreate.o: ../gacreate.f90 modules.o iomod.o
gacross.o: ../gacross.f90 datatypes.o modules.o
gaexpd.o: ../gaexpd.f90 modules.o iomod.o
gafit.o: ../gafit.f90 modules.o iomod.o datatypes.o
gamaxmin.o: ../gamaxmin.f90 modules.o
gamma.o: ../gamma.f90 datatypes.o modules.o
gammasm.o: ../gammasm.f90 datatypes.o
gamuta.o: ../gamuta.f90 datatypes.o modules.o
gaopt.o: ../gaopt.f90 modules.o iomod.o
garepro.o: ../garepro.f90 modules.o iomod.o
gasort.o: ../gasort.f90 datatypes.o iomod.o modules.o
gasteiger.o: ../gasteiger.f90 modules.o iomod.o
gasubs.o: ../gasubs.f90 modules.o iomod.o datatypes.o
gauss.o: ../gauss.f90 datatypes.o modules.o iomod.o
gaword.o: ../gaword.f90 modules.o iomod.o
genpot.o: ../genpot.f90 modules.o
genword.o: ../genword.f90 modules.o gulpcml.o iomod.o m_pdfneutron.o
getbocharge.o: ../getbocharge.f90 datatypes.o modules.o iomod.o
getbochargetrial.o: ../getbochargetrial.f90 datatypes.o modules.o iomod.o
getbonds.o: ../getbonds.f90 modules.o
getcoordno.o: ../getcoordno.f90 modules.o iomod.o
getderv1.o: ../getderv1.f90 modules.o
getdipole.o: ../getdipole.f90 modules.o
getdistances.o: ../getdistances.f90 modules.o
getkeywrd.o: ../getkeywrd.f90 modules.o iomod.o
getline.o: ../getline.F90 modules.o iomod.o
getneighbour.o: ../getneighbour.f90 datatypes.o modules.o
getnmr.o: ../getnmr.f90 datatypes.o modules.o
getphi.o: ../getphi.f90 datatypes.o
getpotsymbol.o: ../getpotsymbol.f90 datatypes.o modules.o
gettraceinertia.o: ../gettraceinertia.f90 modules.o iomod.o
getvibmode.o: ../getvibmode.f90 modules.o
gtheta.o: ../gtheta.f90 modules.o iomod.o
gthetabo.o: ../gthetabo.f90 modules.o iomod.o
gulp.o: ../gulp.F90 datatypes.o
gulpcml.o: ../gulpcml.F90 datatypes.o modules.o m_pdfneutron.o iomod.o m_pdfvariables.o
gulpcml_md.o: ../gulpcml_md.F90 gulpcml.o datatypes.o modules.o
gulpcml_phonon.o: ../gulpcml_phonon.F90 gulpcml.o datatypes.o modules.o iomod.o m_pdfneutron.o m_pdfvariables.o
gulpcml_props.o: ../gulpcml_props.F90 datatypes.o gulpcml.o modules.o
gulpfinish.o: ../gulpfinish.f90 modules.o gulpcml.o
gulpmain.o: ../gulpmain.F90 modules.o iomod.o license.o
gulpsetup.o: ../gulpsetup.F90 modules.o gulpcml.o gulpcml_phonon.o iomod.o m_pdfneutron.o license.o
help.o: ../help.f90 modules.o iomod.o
hfunc.o: ../hfunc.f90 datatypes.o
ind1toijk.o: ../ind1toijk.f90 datatypes.o
indsft.o: ../indsft.f90 datatypes.o
indtoijk.o: ../indtoijk.f90 datatypes.o
init1bodydefaults.o: ../init1bodydefaults.f90 modules.o
init2bodydefaults.o: ../init2bodydefaults.f90 modules.o
init_ins.o: ../init_ins.f90 modules.o
initarc.o: ../initarc.f90 datatypes.o modules.o
initcomms.o: ../initcomms.F90 iomod.o modules.o
initdervs.o: ../initdervs.F90 modules.o iomod.o
initial.o: ../initial.F90 modules.o m_pdfneutron.o m_pr.o
initktrm.o: ../initktrm.f90 modules.o
initmaxatdefaults.o: ../initmaxatdefaults.f90 modules.o
initmaxatotdefaults.o: ../initmaxatotdefaults.f90 modules.o
initmaxbonddefaults.o: ../initmaxbonddefaults.f90 modules.o
initmaxbondqdefaults.o: ../initmaxbondqdefaults.f90 modules.o
initmaxccspecdefaults.o: ../initmaxccspecdefaults.f90 modules.o
initmaxcfgdefaults.o: ../initmaxcfgdefaults.f90 modules.o m_pr.o reallocate.o
initmaxconnectdefaults.o: ../initmaxconnectdefaults.f90 modules.o
initmaxdefdefaults.o: ../initmaxdefdefaults.f90 modules.o
initmaxeamdendefaults.o: ../initmaxeamdendefaults.f90 modules.o
initmaxeamfnspecdefaults.o: ../initmaxeamfnspecdefaults.f90 modules.o
initmaxeamspecdefaults.o: ../initmaxeamspecdefaults.f90 modules.o
initmaxedipspecdefaults.o: ../initmaxedipspecdefaults.f90 modules.o
initmaxfgraddefaults.o: ../initmaxfgraddefaults.f90 modules.o
initmaxfitdefaults.o: ../initmaxfitdefaults.f90 modules.o
initmaxfordefaults.o: ../initmaxfordefaults.f90 modules.o
initmaxfstressdefaults.o: ../initmaxfstressdefaults.f90 modules.o
initmaxgcmcmoldefaults.o: ../initmaxgcmcmoldefaults.f90 modules.o
initmaxlibdefaults.o: ../initmaxlibdefaults.f90 modules.o
initmaxmcswapspec.o: ../initmaxmcswapspec.f90 modules.o
initmaxmoldefaults.o: ../initmaxmoldefaults.f90 modules.o
initmaxnboadefaults.o: ../initmaxnboadefaults.f90 modules.o
initmaxnbopotdefaults.o: ../initmaxnbopotdefaults.f90 modules.o
initmaxnboq0defaults.o: ../initmaxnboq0defaults.f90 modules.o
initmaxnboqdefaults.o: ../initmaxnboqdefaults.f90 modules.o
initmaxnbordefaults.o: ../initmaxnbordefaults.f90 modules.o
initmaxnebreplicatotdefaults.o: ../initmaxnebreplicatotdefaults.f90 modules.o
initmaxnppadefaults.o: ../initmaxnppadefaults.f90 datatypes.o modules.o
initmaxnptsdefaults.o: ../initmaxnptsdefaults.f90 modules.o
initmaxobsdefaults.o: ../initmaxobsdefaults.f90 modules.o
initmaxplanepotdefaults.o: ../initmaxplanepotdefaults.f90 modules.o
initmaxr1atdefaults.o: ../initmaxr1atdefaults.f90 modules.o
initmaxreaxffspecdefaults.o: ../initmaxreaxffspecdefaults.f90 modules.o
initmaxreaxffval3defaults.o: ../initmaxreaxffval3defaults.f90 modules.o
initmaxregiondefaults.o: ../initmaxregiondefaults.f90 modules.o
initmaxsixdefaults.o: ../initmaxsixdefaults.f90 modules.o
initmaxspcellbodefaults.o: ../initmaxspcellbodefaults.f90 modules.o
initmaxspcelldefaults.o: ../initmaxspcelldefaults.f90 modules.o
initmaxspecdefaults.o: ../initmaxspecdefaults.f90 modules.o
initmaxthbdefaults.o: ../initmaxthbdefaults.f90 modules.o
initmaxtitledefaults.o: ../initmaxtitledefaults.f90 modules.o
initmemory.o: ../initmemory.f90 modules.o
initqmatrix.o: ../initqmatrix.f90 modules.o
initqmatrixc.o: ../initqmatrixc.f90 datatypes.o modules.o
insertatom.o: ../insertatom.f90 modules.o
intochar.o: ../intochar.f90 datatypes.o
inword.o: ../inword.f90 modules.o iomod.o m_pdfneutron.o
iomod.o: ../iomod.f90 datatypes.o
istoan.o: ../istoan.f90 modules.o iomod.o
itcom.o: ../itcom.f90 datatypes.o
iterinv.o: ../iterinv.f90 datatypes.o
itow.o: ../itow.f90 datatypes.o
itpack.o: ../itpack.f90 datatypes.o itcom.o
kindex.o: ../kindex.f90 modules.o iomod.o
kvector.o: ../kvector.f90 modules.o iomod.o
kvectorf.o: ../kvectorf.f90 modules.o iomod.o
kweight.o: ../kweight.f90 datatypes.o
label.o: ../label.f90 modules.o
labellc.o: ../labellc.f90 modules.o
lambda.o: ../lambda.f90 modules.o iomod.o
lapack.o: ../lapack.f90 datatypes.o
latpot.o: ../latpot.f90 modules.o gulpcml.o iomod.o
license.o: ../license.F90 modules.o iomod.o
linepro.o: ../linepro.f90 modules.o iomod.o
linpack.o: ../linpack.f90 datatypes.o
lintoijk.o: ../lintoijk.f90 datatypes.o
lmatch.o: ../lmatch.f90 datatypes.o modules.o
lmatchany.o: ../lmatchany.f90 datatypes.o modules.o
lmatchpair.o: ../lmatchpair.f90 datatypes.o modules.o
lmbfgs.o: ../lmbfgs.f90 datatypes.o iomod.o modules.o
local.o: ../local.F90 modules.o
lower.o: ../lower.f90 modules.o iomod.o
ltont.o: ../ltont.f90 datatypes.o
m_conjgr.o: ../m_conjgr.f90 datatypes.o
m_pdf.o: ../m_pdf.f90 datatypes.o m_pdfneutron.o modules.o m_pdfvariables.o
m_pdfneutron.o: ../m_pdfneutron.f90 datatypes.o iomod.o modules.o reallocate.o
m_pdfvariables.o: ../m_pdfvariables.f90 datatypes.o
m_pr.o: ../m_pr.f90 datatypes.o
m_simplex.o: ../m_simplex.f90 datatypes.o modules.o iomod.o
many.o: ../many.f90 modules.o iomod.o
many0d.o: ../many0d.f90 modules.o iomod.o
many12.o: ../many12.f90 modules.o iomod.o
manymd0.o: ../manymd0.f90 modules.o iomod.o
manymd3.o: ../manymd3.f90 modules.o iomod.o
manymd3s.o: ../manymd3s.f90 modules.o iomod.o
manymi3.o: ../manymi3.f90 modules.o iomod.o
manyp.o: ../manyp.f90 modules.o
manysd.o: ../manysd.f90 modules.o datatypes.o iomod.o
manysd2.o: ../manysd2.f90 modules.o iomod.o
matinv.o: ../matinv.f90 modules.o datatypes.o
mccreate.o: ../mccreate.f90 modules.o
mcdestroy.o: ../mcdestroy.f90 modules.o
mcenergy.o: ../mcenergy.f90 modules.o iomod.o
mcmolinsert.o: ../mcmolinsert.f90 modules.o
mcmove.o: ../mcmove.f90 modules.o reallocate.o
mcrotate.o: ../mcrotate.f90 modules.o reallocate.o
mcstorecfg.o: ../mcstorecfg.f90 modules.o
mcstrain.o: ../mcstrain.f90 modules.o
mcswap.o: ../mcswap.f90 modules.o reallocate.o
mctrialfunction.o: ../mctrialfunction.f90 datatypes.o modules.o
mcword.o: ../mcword.f90 modules.o
mdcellind.o: ../mdcellind.f90 modules.o
mdcorrect.o: ../mdcorrect.f90 modules.o m_pr.o
mddistconpost.o: ../mddistconpost.f90 modules.o
mdfunct.o: ../mdfunct.F90 modules.o m_pr.o
mdgcorrect.o: ../mdgcorrect.f90 modules.o
mdke.o: ../mdke.f90 modules.o
mdkeatomicstress.o: ../mdkeatomicstress.f90 modules.o
mdkestrain.o: ../mdkestrain.f90 modules.o
mdlfvcorrect.o: ../mdlfvcorrect.f90 modules.o
mdpredict.o: ../mdpredict.f90 modules.o m_pr.o
mdprop.o: ../mdprop.f90 modules.o gulpcml.o gulpcml_md.o iomod.o m_pr.o
mdreset.o: ../mdreset.f90 modules.o
mdrestart.o: ../mdrestart.f90 modules.o iomod.o m_pr.o pr_random.o
mdscale.o: ../mdscale.f90 modules.o iomod.o
mdshscale.o: ../mdshscale.f90 modules.o
mdstop.o: ../mdstop.f90 iomod.o modules.o
mdtemperature.o: ../mdtemperature.f90 modules.o m_pr.o pr_random.o
mdvelcor.o: ../mdvelcor.f90 modules.o
mdvvcorrect.o: ../mdvvcorrect.f90 modules.o
mdword.o: ../mdword.f90 modules.o iomod.o m_pr.o
mdwrite.o: ../mdwrite.f90 modules.o
meamfnderv.o: ../meamfnderv.f90 datatypes.o modules.o
meamrho.o: ../meamrho.f90 modules.o
meamscreen.o: ../meamscreen.f90 modules.o
meamsymdensity.o: ../meamsymdensity.f90 modules.o
meamtotalrho.o: ../meamtotalrho.f90 modules.o
meamtotalrhocrossderv.o: ../meamtotalrhocrossderv.f90 modules.o
meamtotalrhoderv.o: ../meamtotalrhoderv.f90 modules.o
meamtotalrhoscreenderv.o: ../meamtotalrhoscreenderv.f90 modules.o
methodok.o: ../methodok.f90 modules.o iomod.o
mindtoijk.o: ../mindtoijk.f90 datatypes.o
minimise.o: ../minimise.F90 modules.o gulpcml.o iomod.o license.o
modules.o: ../modules.f90 datatypes.o
molind.o: ../molind.f90 modules.o iomod.o reallocate.o
molindfix.o: ../molindfix.f90 modules.o reallocate.o
molindfixtrial.o: ../molindfixtrial.f90 modules.o reallocate.o
molindtrial.o: ../molindtrial.f90 modules.o iomod.o reallocate.o
moltype.o: ../moltype.f90 modules.o
move2a1.o: ../move2a1.f90 modules.o
mpbarrier.o: ../mpbarrier.F90 modules.o
mpfinish.o: ../mpfinish.F90
mxmb.o: ../mxmb.f90 datatypes.o
nagamma.o: ../nagamma.f90 modules.o
nearest.o: ../nearest.f90 datatypes.o
nebforce.o: ../nebforce.f90 modules.o
nebword.o: ../nebword.f90 modules.o reallocate.o
nrhess.o: ../nrhess.f90 modules.o iomod.o
nrstep.o: ../nrstep.f90 datatypes.o
nudge.o: ../nudge.f90 datatypes.o modules.o
nullpointer.o: ../nullpointer.f90 modules.o m_pr.o reallocate.o
numhess.o: ../numhess.f90 iomod.o modules.o
okspec.o: ../okspec.f90 modules.o
olinmin.o: ../olinmin.f90 modules.o iomod.o
omegaproperty.o: ../omegaproperty.f90 modules.o iomod.o
onebody.o: ../onebody.f90 modules.o
optim.o: ../optim.f90 modules.o gulpcml.o iomod.o reallocate.o
optin.o: ../optin.f90 modules.o iomod.o
options.o: ../options.f90 modules.o gulpcml.o iomod.o
optout.o: ../optout.f90 modules.o iomod.o
optshe.o: ../optshe.f90 modules.o iomod.o
oscillatorstrength.o: ../oscillatorstrength.f90 modules.o iomod.o
oscillatorstrengthg.o: ../oscillatorstrengthg.f90 modules.o iomod.o
outadvice.o: ../outadvice.f90 iomod.o modules.o
outarc.o: ../outarc.f90 modules.o iomod.o
outatomicstress.o: ../outatomicstress.f90 modules.o iomod.o
outbiograf.o: ../outbiograf.f90 modules.o iomod.o
outcell.o: ../outcell.f90 modules.o iomod.o
outcif.o: ../outcif.f90 modules.o
outcosmo.o: ../outcosmo.f90 modules.o iomod.o
outcssr.o: ../outcssr.f90 modules.o iomod.o
outdcdheader.o: ../outdcdheader.f90 modules.o
outdefin.o: ../outdefin.f90 modules.o iomod.o
outdener.o: ../outdener.f90 modules.o iomod.o
outderv.o: ../outderv.f90 modules.o iomod.o
outdlv.o: ../outdlv.f90 modules.o iomod.o
outdrv.o: ../outdrv.f90 modules.o
outed.o: ../outed.f90 modules.o iomod.o
outener.o: ../outener.f90 modules.o gulpcml.o iomod.o
outerror.o: ../outerror.f90 iomod.o modules.o
outfdf.o: ../outfdf.f90 modules.o
outfile.o: ../outfile.f90 modules.o iomod.o gulpcml.o
outfit.o: ../outfit.f90 modules.o iomod.o
outfitcon.o: ../outfitcon.f90 modules.o iomod.o
outfrc.o: ../outfrc.f90 modules.o
outfreq.o: ../outfreq.f90 modules.o iomod.o
outgafit.o: ../outgafit.f90 modules.o iomod.o
outgaopt.o: ../outgaopt.f90 modules.o iomod.o
outgapar.o: ../outgapar.f90 modules.o iomod.o
outgen.o: ../outgen.f90 modules.o iomod.o
outkey.o: ../outkey.f90 modules.o iomod.o m_pdfneutron.o
outkpt.o: ../outkpt.f90 modules.o iomod.o
outlammps.o: ../outlammps.f90 modules.o
outlammpspots.o: ../outlammpspots.f90 modules.o
outmarv.o: ../outmarv.f90 modules.o
outmarv2.o: ../outmarv2.f90 modules.o
outmc.o: ../outmc.f90 modules.o
outmd.o: ../outmd.f90 modules.o
outmdin.o: ../outmdin.f90 modules.o iomod.o m_pr.o
outmol.o: ../outmol.f90 modules.o iomod.o
outneb.o: ../outneb.f90 modules.o iomod.o
outnebin.o: ../outnebin.f90 modules.o iomod.o
outobs.o: ../outobs.f90 modules.o iomod.o
outofmemory.o: ../outofmemory.f90 iomod.o modules.o
outofp.o: ../outofp.f90 modules.o iomod.o
outphon.o: ../outphon.f90 modules.o iomod.o
outpolar.o: ../outpolar.f90 iomod.o modules.o
outpot.o: ../outpot.f90 modules.o iomod.o
outqbo.o: ../outqbo.f90 modules.o
outspec.o: ../outspec.f90 modules.o iomod.o
outspec_pdf.o: ../outspec_pdf.f90 modules.o iomod.o m_pdfneutron.o
outstresses.o: ../outstresses.f90 modules.o iomod.o
outstructure.o: ../outstructure.f90 modules.o iomod.o
outsync.o: ../outsync.f90 modules.o iomod.o
outsyncin.o: ../outsyncin.f90 modules.o iomod.o
outthb.o: ../outthb.f90 modules.o
outtime.o: ../outtime.f90 iomod.o modules.o
outvar.o: ../outvar.f90 modules.o iomod.o
outvarg.o: ../outvarg.f90 modules.o iomod.o
outwarning.o: ../outwarning.f90 iomod.o modules.o
outxr.o: ../outxr.f90 modules.o iomod.o
outxtl.o: ../outxtl.f90 modules.o iomod.o
outxyz.o: ../outxyz.f90 modules.o iomod.o
pdiag.o: ../pdiag.f90 modules.o m_pdfneutron.o
pdiage.o: ../pdiage.f90 modules.o m_pdfneutron.o
pdiagg.o: ../pdiagg.f90 modules.o
pdiagl.o: ../pdiagl.f90 modules.o m_pdfneutron.o
peigen.o: ../peigen.f90 modules.o iomod.o m_pdfneutron.o
peigeng.o: ../peigeng.f90 modules.o iomod.o m_pdfneutron.o
phoncopy1.o: ../phoncopy1.f90 datatypes.o
phoncopy2.o: ../phoncopy2.f90 datatypes.o
phonon.o: ../phonon.f90 modules.o gulpcml.o gulpcml_phonon.o iomod.o m_pdf.o m_pdfneutron.o
phonword.o: ../phonword.f90 modules.o iomod.o m_pdfneutron.o
pireal0d.o: ../pireal0d.f90 modules.o
pirealrecip.o: ../pirealrecip.f90 modules.o datatypes.o
pirealrecips.o: ../pirealrecips.f90 modules.o datatypes.o
planepot2D.o: ../planepot2D.f90 modules.o
planepotmc.o: ../planepotmc.f90 modules.o
planepotmd.o: ../planepotmd.f90 modules.o
poccon.o: ../poccon.f90 modules.o
poccons.o: ../poccons.f90 modules.o
polarisation.o: ../polarisation.f90 modules.o
potcut.o: ../potcut.f90 modules.o iomod.o
potential.o: ../potential.f90 modules.o
potgrid.o: ../potgrid.f90 modules.o iomod.o
potword21.o: ../potword21.f90 modules.o iomod.o
potword22.o: ../potword22.f90 modules.o iomod.o
potword3.o: ../potword3.f90 modules.o iomod.o
potword4.o: ../potword4.f90 modules.o iomod.o
potword6.o: ../potword6.f90 modules.o iomod.o
potwordm.o: ../potwordm.f90 modules.o
pr_integrator.o: ../pr_integrator.f90 modules.o m_pr.o pr_random.o datatypes.o
pr_random.o: ../pr_random.f90 datatypes.o modules.o
predict.o: ../predict.f90 modules.o iomod.o
pressure.o: ../pressure.f90 modules.o
projd0.o: ../projd0.f90 datatypes.o
projd3a.o: ../projd3a.f90 modules.o datatypes.o
projfork0.o: ../projfork0.f90 modules.o
projfork3.o: ../projfork3.f90 modules.o
projfork3d.o: ../projfork3d.f90 modules.o
projfork3ds.o: ../projfork3ds.f90 modules.o
projfork3s.o: ../projfork3s.f90 modules.o datatypes.o
projthbk0.o: ../projthbk0.f90 modules.o
projthbk3.o: ../projthbk3.f90 modules.o
projthbk3d.o: ../projthbk3d.f90 modules.o
projthbk3ds.o: ../projthbk3ds.f90 modules.o
projthbk3s.o: ../projthbk3s.f90 modules.o datatypes.o
property.o: ../property.f90 modules.o gulpcml.o gulpcml_props.o iomod.o
property0.o: ../property0.f90 modules.o gulpcml.o gulpcml_props.o iomod.o
psumall.o: ../psumall.f90 modules.o
putpar.o: ../putpar.f90 modules.o
pythag.o: ../pythag.f90
qdupdate.o: ../qdupdate.f90 modules.o
qeqbody.o: ../qeqbody.f90 modules.o
qmatrix1D.o: ../qmatrix1D.f90 modules.o
qmatrixelement.o: ../qmatrixelement.f90 modules.o
qmatrixelementc.o: ../qmatrixelementc.f90 datatypes.o modules.o
qtmin.o: ../qtmin.f90 modules.o iomod.o m_conjgr.o
qtpie.o: ../qtpie.f90 modules.o iomod.o
qupdate.o: ../qupdate.f90 modules.o
radialforce.o: ../radialforce.f90 modules.o
radialforcetrial.o: ../radialforcetrial.f90 modules.o
ramantrm.o: ../ramantrm.f90 modules.o
random.o: ../random.f90 datatypes.o modules.o
ratfn.o: ../ratfn.f90 datatypes.o
real0d.o: ../real0d.f90 modules.o iomod.o
real0d3.o: ../real0d3.f90 modules.o
real11.o: ../real11.f90 modules.o
real112.o: ../real112.f90 modules.o
real11pot.o: ../real11pot.f90 modules.o
real11s.o: ../real11s.f90 modules.o
real11s2.o: ../real11s2.f90 modules.o
real12a.o: ../real12a.f90 modules.o
real12a2.o: ../real12a2.f90 modules.o
real12apot.o: ../real12apot.f90 modules.o
real12as.o: ../real12as.f90 modules.o
real12as2.o: ../real12as2.f90 modules.o
real1D.o: ../real1D.f90 modules.o iomod.o
real1Dmc.o: ../real1Dmc.f90 modules.o iomod.o
real1Dmd.o: ../real1Dmd.f90 modules.o iomod.o
real1Dp.o: ../real1Dp.f90 modules.o
real1Dq.o: ../real1Dq.f90 modules.o
real1d3.o: ../real1d3.f90 modules.o
real2Dq.o: ../real2Dq.f90 modules.o
real2a.o: ../real2a.f90 modules.o iomod.o
real2a1.o: ../real2a1.f90 modules.o
real2a1f.o: ../real2a1f.f90 modules.o
real2adef.o: ../real2adef.f90 modules.o
real2aef.o: ../real2aef.f90 modules.o
real2amany.o: ../real2amany.f90 modules.o
real2b.o: ../real2b.f90 modules.o
real4.o: ../real4.f90 modules.o datatypes.o
realcmm.o: ../realcmm.f90 modules.o iomod.o
reale.o: ../reale.f90 modules.o datatypes.o iomod.o
reallocate.o: ../reallocate.f90 datatypes.o iomod.o
realmc0.o: ../realmc0.f90 modules.o
realmc3.o: ../realmc3.f90 modules.o datatypes.o
realmd0.o: ../realmd0.f90 modules.o iomod.o
realmd3.o: ../realmd3.f90 modules.o datatypes.o iomod.o
realmd3s.o: ../realmd3s.f90 modules.o datatypes.o iomod.o
realmd3t.o: ../realmd3t.f90 modules.o datatypes.o iomod.o
realmi3.o: ../realmi3.f90 modules.o iomod.o
realmimc3.o: ../realmimc3.f90 modules.o
realp.o: ../realp.f90 modules.o datatypes.o
realrecip3d3.o: ../realrecip3d3.f90 modules.o datatypes.o iomod.o
realsd.o: ../realsd.f90 modules.o datatypes.o iomod.o
realsd2.o: ../realsd2.f90 modules.o datatypes.o iomod.o
reaxff_bo.o: ../reaxff_bo.f90 datatypes.o modules.o iomod.o
reaxff_funcs.o: ../reaxff_funcs.f90 modules.o datatypes.o
reaxffctfunct.o: ../reaxffctfunct.f90 iomod.o modules.o
reaxffmd.o: ../reaxffmd.f90 datatypes.o modules.o iomod.o
reaxffpot.o: ../reaxffpot.f90 datatypes.o modules.o
rebospline.o: ../rebospline.f90 datatypes.o modules.o
recip12a.o: ../recip12a.f90 modules.o
recip12apot.o: ../recip12apot.f90 modules.o
recip12as.o: ../recip12as.f90 modules.o
recip2D.o: ../recip2D.f90 modules.o
recip2Dp.o: ../recip2Dp.f90 modules.o
recip2Dq.o: ../recip2Dq.f90 modules.o
recip2Dtrial.o: ../recip2Dtrial.f90 modules.o
recip3D.o: ../recip3D.f90 modules.o
recip3Dmd.o: ../recip3Dmd.f90 modules.o
recip3Dp.o: ../recip3Dp.f90 modules.o
recip3Dtrial.o: ../recip3Dtrial.f90 modules.o
recip4.o: ../recip4.f90 modules.o
recipsd.o: ../recipsd.f90 modules.o
recipsd2.o: ../recipsd2.f90 modules.o
reciptrmd3.o: ../reciptrmd3.f90 modules.o
reciptrmdp.o: ../reciptrmdp.f90 modules.o
reinitialise.o: ../reinitialise.f90 datatypes.o modules.o
remove.o: ../remove.f90 modules.o
reorder.o: ../reorder.f90 modules.o
resetcfgdefaults.o: ../resetcfgdefaults.f90 modules.o reallocate.o
rfind.o: ../rfind.f90 modules.o
rfindboth.o: ../rfindboth.f90 modules.o
rfohess.o: ../rfohess.f90 modules.o
rfostep.o: ../rfostep.f90 modules.o iomod.o
rhoinv.o: ../rhoinv.f90 modules.o
rhtohex.o: ../rhtohex.f90 modules.o
rlist.o: ../rlist.f90 modules.o
rpair.o: ../rpair.f90 modules.o
rpair2.o: ../rpair2.f90 modules.o
rsearch1D.o: ../rsearch1D.f90 modules.o
rsearch2D.o: ../rsearch2D.f90 modules.o
rsearch3D.o: ../rsearch3D.f90 modules.o
rtlist.o: ../rtlist.f90 modules.o
runmc.o: ../runmc.f90 modules.o iomod.o
runmd.o: ../runmd.F90 modules.o gulpcml.o iomod.o m_pr.o license.o
runneb.o: ../runneb.f90 modules.o iomod.o
runsync.o: ../runsync.f90 modules.o iomod.o
samemol.o: ../samemol.f90 modules.o
scaleevec.o: ../scaleevec.f90 datatypes.o
screenct.o: ../screenct.f90 modules.o
sec0.o: ../sec0.f90 modules.o iomod.o
sec0f.o: ../sec0f.f90 modules.o iomod.o
sec3.o: ../sec3.f90 modules.o iomod.o
sec3f.o: ../sec3f.f90 modules.o iomod.o
secondpass.o: ../secondpass.f90 modules.o iomod.o
selfterm.o: ../selfterm.f90 modules.o
set2a.o: ../set2a.f90 modules.o datatypes.o
setatomnodes.o: ../setatomnodes.f90 modules.o iomod.o
setatomnodesbo.o: ../setatomnodesbo.f90 datatypes.o modules.o iomod.o
setbelashchenko.o: ../setbelashchenko.f90 modules.o
setbondorder.o: ../setbondorder.f90 modules.o
setbrenner1.o: ../setbrenner1.f90 datatypes.o modules.o
setbrenner3.o: ../setbrenner3.f90 datatypes.o modules.o
setbsmptr.o: ../setbsmptr.f90 modules.o
setc6.o: ../setc6.f90 modules.o
setcellderv.o: ../setcellderv.f90 modules.o
setcfg.o: ../setcfg.f90 modules.o iomod.o
setcmm.o: ../setcmm.f90 modules.o iomod.o
setcosmo.o: ../setcosmo.f90 modules.o
setcosmoainv.o: ../setcosmoainv.f90 modules.o
setcosmoamat.o: ../setcosmoamat.f90 modules.o iomod.o reallocate.o
setcosmobmat.o: ../setcosmobmat.f90 modules.o iomod.o
setcutoffmax.o: ../setcutoffmax.f90 modules.o
setcutoffmaxbo.o: ../setcutoffmaxbo.f90 modules.o
setdcosmoamat.o: ../setdcosmoamat.f90 modules.o iomod.o reallocate.o
setdcosmobmat.o: ../setdcosmobmat.f90 modules.o reallocate.o
setdef.o: ../setdef.f90 modules.o reallocate.o
setdestroyptr.o: ../setdestroyptr.f90 modules.o reallocate.o
setdisp.o: ../setdisp.f90 modules.o
setdistance.o: ../setdistance.f90 modules.o
setdr2mat.o: ../setdr2mat.f90 modules.o iomod.o
setdsegweight.o: ../setdsegweight.f90 modules.o
seteam.o: ../seteam.f90 modules.o
setedip.o: ../setedip.f90 datatypes.o modules.o iomod.o
setele.o: ../setele.f90 modules.o m_pdfneutron.o
setewald.o: ../setewald.f90 modules.o iomod.o
setfeshtmat.o: ../setfeshtmat.f90 datatypes.o
setfour.o: ../setfour.f90 modules.o
setkeywrd.o: ../setkeywrd.f90 modules.o m_pdfneutron.o
setkpt.o: ../setkpt.f90 modules.o iomod.o m_pdfneutron.o
setktrm4.o: ../setktrm4.f90 modules.o
setktrmd3.o: ../setktrmd3.f90 modules.o
setktrmdp.o: ../setktrmdp.f90 modules.o
setlenn.o: ../setlenn.f90 modules.o
setlib.o: ../setlib.f90 modules.o
setlist3.o: ../setlist3.f90 modules.o
setlist4.o: ../setlist4.f90 modules.o
setlist6.o: ../setlist6.f90 modules.o
setlistoop.o: ../setlistoop.f90 modules.o
setmaxcell1D.o: ../setmaxcell1D.f90 modules.o
setmd.o: ../setmd.f90 modules.o iomod.o m_pr.o pr_random.o
setmol.o: ../setmol.f90 modules.o reallocate.o
setmoldef.o: ../setmoldef.f90 modules.o reallocate.o
setmoldef12a.o: ../setmoldef12a.f90 modules.o
setmols.o: ../setmols.f90 modules.o reallocate.o
setmoveptr.o: ../setmoveptr.f90 modules.o reallocate.o
setoccptr.o: ../setoccptr.f90 modules.o
setoccptrs.o: ../setoccptrs.f90 modules.o
setoccshptr.o: ../setoccshptr.f90 modules.o
setoccshptrs.o: ../setoccshptrs.f90 modules.o
setpointgroup.o: ../setpointgroup.f90 datatypes.o
setpolar.o: ../setpolar.f90 modules.o
setpote.o: ../setpote.f90 modules.o
setqonsas.o: ../setqonsas.f90 modules.o iomod.o
setquasiharm.o: ../setquasiharm.f90 modules.o
setratiom.o: ../setratiom.f90 modules.o
setreaxFF.o: ../setreaxFF.f90 datatypes.o modules.o iomod.o
setreaxffQ.o: ../setreaxffQ.f90 modules.o iomod.o
setreaxffQiter.o: ../setreaxffQiter.f90 modules.o iomod.o reallocate.o
setregiontrans.o: ../setregiontrans.f90 modules.o
setrotateptr.o: ../setrotateptr.f90 modules.o reallocate.o
setsas.o: ../setsas.f90 modules.o iomod.o
setsegweight.o: ../setsegweight.f90 modules.o
setsix.o: ../setsix.f90 modules.o
setslice.o: ../setslice.f90 modules.o
setspatial.o: ../setspatial.f90 modules.o iomod.o
setspatialbo.o: ../setspatialbo.f90 modules.o iomod.o
setspatialcells.o: ../setspatialcells.f90 datatypes.o
setspec.o: ../setspec.f90 modules.o
setspring.o: ../setspring.f90 modules.o
setstrainptr.o: ../setstrainptr.f90 modules.o reallocate.o
setswapptr.o: ../setswapptr.f90 modules.o reallocate.o
settaper.o: ../settaper.f90 modules.o
setthree.o: ../setthree.f90 modules.o
settings.o: ../settings.f90 datatypes.o modules.o
settorcubic.o: ../settorcubic.f90 datatypes.o modules.o
settricubic.o: ../settricubic.f90 datatypes.o modules.o
settricubic2.o: ../settricubic2.f90 datatypes.o modules.o
setuff.o: ../setuff.f90 modules.o
setup.o: ../setup.f90 modules.o datatypes.o iomod.o reallocate.o
setvbon.o: ../setvbon.f90 modules.o iomod.o
setzmolslice.o: ../setzmolslice.f90 modules.o
sitepot.o: ../sitepot.f90 modules.o iomod.o
sixbody.o: ../sixbody.f90 modules.o iomod.o
sixlist.o: ../sixlist.f90 modules.o
sixmc.o: ../sixmc.f90 modules.o
sixmd.o: ../sixmd.f90 modules.o
sixmds.o: ../sixmds.f90 modules.o
sixnos.o: ../sixnos.f90 modules.o
sixp.o: ../sixp.f90 modules.o
sixsd.o: ../sixsd.f90 modules.o
sixsd2.o: ../sixsd2.f90 modules.o
sixstrterms.o: ../sixstrterms.f90 datatypes.o
smbody.o: ../smbody.f90 modules.o
solvation.o: ../solvation.f90 modules.o
sort.o: ../sort.f90 modules.o
sort1sym.o: ../sort1sym.f90 modules.o
sort2a.o: ../sort2a.f90 modules.o
special.o: ../special.f90 modules.o iomod.o
speclibw.o: ../speclibw.f90 modules.o
specword.o: ../specword.f90 modules.o
spline.o: ../spline.f90 modules.o datatypes.o iomod.o
stolc.o: ../stolc.f90 datatypes.o
stopnow.o: ../stopnow.F90 iomod.o modules.o
stouc.o: ../stouc.f90 datatypes.o
strain.o: ../strain.f90 datatypes.o
strfin.o: ../strfin.f90 modules.o iomod.o
strsym.o: ../strsym.f90 modules.o
strword.o: ../strword.f90 modules.o iomod.o reallocate.o
sumall.o: ../sumall.F90 modules.o
sumderv1.o: ../sumderv1.f90 modules.o
sumderv2.o: ../sumderv2.f90 modules.o
sumderv2f.o: ../sumderv2f.f90 modules.o
sumderv2s.o: ../sumderv2s.f90 modules.o
super.o: ../super.f90 modules.o
surfaceenergy.o: ../surfaceenergy.f90 modules.o
surfword.o: ../surfword.f90 modules.o reallocate.o
switch.o: ../switch.f90 modules.o
symbind.o: ../symbind.f90 datatypes.o
symderv2.o: ../symderv2.f90 modules.o
symdervrot.o: ../symdervrot.f90 modules.o
symmet.o: ../symmet.f90 modules.o iomod.o
symoff.o: ../symoff.f90 modules.o
symout.o: ../symout.f90 modules.o iomod.o
sympot.o: ../sympot.f90 modules.o
symprod.o: ../symprod.f90 iomod.o modules.o
syncforce.o: ../syncforce.f90 modules.o
taper.o: ../taper.f90 datatypes.o modules.o
tfunc.o: ../tfunc.f90 datatypes.o
thermalconductivity.o: ../thermalconductivity.f90 modules.o iomod.o
three0d3.o: ../three0d3.f90 modules.o
three12.o: ../three12.f90 modules.o
three3d3.o: ../three3d3.f90 modules.o
threebody.o: ../threebody.f90 modules.o
threelist.o: ../threelist.f90 modules.o iomod.o
threemc.o: ../threemc.f90 modules.o iomod.o
threemd.o: ../threemd.f90 modules.o iomod.o
threemds.o: ../threemds.f90 modules.o iomod.o
threenos.o: ../threenos.f90 modules.o iomod.o
threep.o: ../threep.f90 modules.o
threesd.o: ../threesd.f90 modules.o datatypes.o iomod.o
threesd2.o: ../threesd2.f90 modules.o iomod.o
threestrterms.o: ../threestrterms.f90 datatypes.o
torsion.o: ../torsion.f90 modules.o iomod.o
transmat.o: ../transmat.f90 modules.o iomod.o
trap_sigint.o: ../trap_sigint.f90 modules.o iomod.o
twobody.o: ../twobody.f90 modules.o
twobodymd.o: ../twobodymd.f90 modules.o
twoden.o: ../twoden.f90 modules.o
uncell.o: ../uncell.f90 modules.o
uncentre.o: ../uncentre.f90 modules.o datatypes.o
unfreeze.o: ../unfreeze.f90 modules.o
updatesas.o: ../updatesas.f90 modules.o iomod.o
validtriad.o: ../validtriad.f90 modules.o
volume.o: ../volume.f90 datatypes.o
wcoord.o: ../wcoord.f90 datatypes.o modules.o
wolfself.o: ../wolfself.f90 modules.o
wolfselftrial.o: ../wolfselftrial.f90 modules.o
worsy.o: ../worsy.f90 modules.o
wtof.o: ../wtof.f90 datatypes.o
x0strain.o: ../x0strain.f90 modules.o
x0tostr.o: ../x0tostr.f90 modules.o
x0tostrcentroid.o: ../x0tostrcentroid.f90 modules.o
x0tostrtrial.o: ../x0tostrtrial.f90 modules.o
xctox0.o: ../xctox0.f90 modules.o
