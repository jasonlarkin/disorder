  subroutine setkeyword
!
!  Sets general flags based on keywords
!
!   2/04 Created from getkeyword
!   7/05 lSandM added
!   7/05 Setting of linten flag improved to remove ambiguity
!   9/06 libdump keyword added
!  11/06 NEB modifications added
!   1/07 Gasteiger charges added
!   3/07 lPrintEAM keyword added
!   3/07 lPrintTwo keyword added
!   3/07 More robust checking of keywords added by ensuring
!        string comes at the start of the word
!   3/07 lpreserveQ added
!   4/07 Conjugate gradients set if running in parallel
!   5/07 Mean KE option added
!   5/07 qbond keyword added
!   7/07 lmeta added
!  10/08 COSMO/COSMIC keywords merged in 
!  10/08 Error in logic for lmodco setting corrected
!  10/08 Error in logic for other keywords also corrected
!  11/08 lPrintFour keyword added
!  11/08 lPrintThree keyword added
!   2/09 lconj only set to be true for > one processor if not LM-BFGS
!   6/09 Module name changed from three to m_three
!   6/09 PDF keywords added
!   7/09 Symmetry turned off for derivatives in reaxFF case
!  12/09 pregionforce keyword added
!   4/10 qtpie keyword added
!   6/10 Hopping keyword added
!   8/10 lconvert, lphase, lcutbelow keywords removed
!   8/10 Keyword settings adjusted for PDF case
!   8/10 lfix1atom added
!  10/10 Symmetry turned off for EDIP potentials
!  12/10 Hiding of shells added
!   1/11 Force minimisation option added
!   2/11 Keyword added to turn off ReaxFF charges
!   3/11 lstressout added
!   9/11 Metadynamics internal code replaced with Plumed
!   9/11 Madelung correction added
!  11/11 eregion keyword added
!   5/12 Atomic stress keyword added
!   6/12 Thermal conductivity added
!   7/12 Oldinten keyword added
!   9/12 Pacha added
!   9/12 Eckart transformation added 
!  10/12 Option added to allow two atoms co-exist on a site with an
!        occupancy greater than one.
!  12/12 site_energy keyword added
!
!  Conditions of use:
!
!  GULP is available free of charge to academic institutions
!  and non-commerical establishments only. Copies should be
!  obtained from the author only and should not be distributed
!  in any form by the user to a third party without the express
!  permission of the author. This notice applies to all parts
!  of the program, except any library routines which are
!  distributed with the code for completeness. All rights for
!  such routines remain with the original distributor.
!
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, December 2012
!
  use bondorderdata,  only : nboQ
  use control
  use cosmo,          only : lcosmic, lsegsmooth
  use distances,      only : lStoreVectors
  use eam,            only : lPrintEAM
  use element
  use fitting
  use four,           only : lPrintFour
  use library,        only : llibsymdump
  use m_pdfneutron,   only : lmakeeigarray, lcoreinfo, lpdf
  use m_pdfneutron,   only : lnowidth, lpartial, lfreqcut, lkeepcut, lnoksym
  use m_pdfneutron,   only : lpdfout
  use m_three,        only : lPrintThree
  use mdlogic
  use molecule
  use neb,            only : lnebclimbingimage, lnebdoublynudged
  use optimisation
  use parallel,       only : nprocs
  use symmetry
  use synchro,        only : lfixtangent
  use two,            only : lPrintTwo
  use wolfcosmo,      only : lPureCoulomb0D
  implicit none
!
!  Set flags from keywords
!
  lallowgt1 = (index(keyword,' allo').ne.0.or.index(keyword,'allo').eq.1)
  langle = (index(keyword,' angl').ne.0.or.index(keyword,'angl').eq.1)
  lanneal = (index(keyword,' anne').ne.0.or.index(keyword,'anne').eq.1)
  latomicstress = (index(keyword,' atom').ne.0.or.index(keyword,'atom').eq.1)
  laver = (index(keyword,' aver').ne.0.or.index(keyword,'aver').eq.1)
  lbond = (index(keyword,' bond').ne.0.or.index(keyword,'bond').eq.1)
  lbroad = (index(keyword,' broa').ne.0.or.index(keyword,'broa').eq.1)
  lbulknoopt = (index(keyword,' bulk').ne.0.or.index(keyword,'bulk').eq.1)
  lc6 = ((index(keyword,' c6').ne.0.or.index(keyword,'c6').eq.1).and.index(keyword,'noel').eq.0)
  lcello = (index(keyword,' cell').ne.0.or.index(keyword,'cell').eq.1)
  lcomp = (index(keyword,' comp').ne.0.or.index(keyword,'comp').eq.1)
  lconj = (index(keyword,' conj').ne.0.or.index(keyword,'conj').eq.1)
  lconp = (index(keyword,' conp').ne.0.or.index(keyword,'conp').eq.1)
  lconv = (index(keyword,' conv').ne.0.or.index(keyword,'conv').eq.1)
  lcosmic = (index(keyword,'cosmi').ne.0)
  lcosmo = (index(keyword,'cosmo').ne.0)
  ldcharge = (index(keyword,' dcha').ne.0.or.index(keyword,'dcha').eq.1)
  ldebug = (index(keyword,' debu').ne.0.or.index(keyword,'debu').eq.1)
  ldefect = (index(keyword,' defe').ne.0.or.index(keyword,'defe').eq.1)
  ldfp = ((index(keyword,' dfp').ne.0.or.index(keyword,'dfp').eq.1).and.index(keyword,'bfgs').eq.0)
  ldipole = (index(keyword,' dipo').ne.0.or.index(keyword,'dipo').eq.1)
  lStoreVectors = (index(keyword,' stor').ne.0.or.index(keyword,'stor').eq.1)
  ldist = (index(keyword,' dist').ne.0.or.index(keyword,'dist').eq.1)
  lDoElectrostatics = (index(keyword,' noel').eq.0.and.index(keyword,'noel').ne.1)
  ldsym = (index(keyword,' nodsym').eq.0.and.index(keyword,'nodsym').ne.1)
  leckart = (index(keyword,' eck').ne.0.or.index(keyword,'eck').eq.1)
  leem = (index(keyword,' eem').ne.0.or.index(keyword,'eem').eq.1)
  lefg = (index(keyword,' efg').ne.0.or.index(keyword,'efg').eq.1)
  leigen = (index(keyword,' eige').ne.0.or.index(keyword,'eige').eq.1)
  leregion = (index(keyword,' ereg').ne.0.or.index(keyword,'ereg').eq.1)
  lfbfgs = (index(keyword,' fbfg').ne.0.or.index(keyword,'fbfg').eq.1)
  lfit = (index(keyword,' fit').ne.0.or.index(keyword,'fit').eq.1)
  lfix1atom = (index(keyword,' unfi').eq.0.and.index(keyword,'unfi').ne.1)
  lforcemin = (index(keyword,' forc').ne.0.or.index(keyword,'forc').eq.1)
  lfree = (index(keyword,' free').ne.0.or.index(keyword,'free').eq.1)
  lfreq = (index(keyword,' freq').ne.0.or.index(keyword,'freq').eq.1)
  lfreqout = (index(keyword,' nofr').eq.0.and.index(keyword,'nofr').ne.1)
  lfixtangent = (index(keyword,' tfix').ne.0.and.index(keyword,'tfix').eq.1)
  lga = (index(keyword,' gene').ne.0.or.index(keyword,'gene').eq.1)
  lgasteiger = (index(keyword,' gast').ne.0.or.index(keyword,'gast').eq.1)
  lgrad = (index(keyword,' grad').ne.0.or.index(keyword,'grad').eq.1)
  lhex = (index(keyword,' hex').ne.0.or.index(keyword,'hex').eq.1)
  lhideshells = (index(keyword,' hide').ne.0.or.index(keyword,'hide').eq.1)
  linten = (index(keyword,'inte').eq.1.or.index(keyword,' inte').ne.0)
  lkfull = (index(keyword,' kful').ne.0.or.index(keyword,'kful').eq.1)
  llbfgs = (index(keyword,' lbfg').ne.0.or.index(keyword,'lbfg').eq.1)
  llibsymdump = (index(keyword,' libd').ne.0.or.index(keyword,'libd').eq.1)
  lmadelung = (index(keyword,'made').eq.1.or.index(keyword,' made').ne.0)
  lmarvreg2 = (index(keyword,' marv').ne.0.or.index(keyword,'marv').eq.1)
  lmc = (index(keyword,'mont').eq.1.or.index(keyword,' mont').ne.0)
  lmd = (index(keyword,'md').eq.1.or.index(keyword,' md').ne.0)
  lmeanke = (index(keyword,'mean').eq.1.or.index(keyword,' mean').ne.0)
  lminimage = (index(keyword,' mini').ne.0.or.index(keyword,'mini').eq.1)
  lmodco = (index(keyword,' nomod').eq.0.and.index(keyword,'nomod').ne.1)
  lmol = (index(keyword,' mol').ne.0.or.index(keyword,'mol').eq.1)
  lmolq = (index(keyword,' molq').ne.0.or.index(keyword,'molq').eq.1)
  lmolmec = (index(keyword,' molm').ne.0.or.index(keyword,'molm').eq.1)
  lmolfix = (index(keyword,' fix').ne.0.or.index(keyword,'fix').eq.1)
  lneb = (index(keyword,' neb').ne.0.or.index(keyword,'neb').eq.1)
  lnebclimbingimage = (index(keyword,' cineb').ne.0.or.index(keyword,'cineb').eq.1)
  lnebdoublynudged = (index(keyword,' nodn').eq.0.and.index(keyword,'nodn').ne.1)
  lnoautobond = (index(keyword,' noau').ne.0.or.index(keyword,'noau').eq.1)
  lnoenergy = (index(keyword,' noen').ne.0.or.index(keyword,'noen').eq.1)
  lnoflags = (index(keyword,' nofl').ne.0.or.index(keyword,'nofl').eq.1)
  lnoreal = (index(keyword,' noreal').ne.0.or.index(keyword,'noreal').eq.1)
  lnorecip = (index(keyword,' noreci').ne.0.or.index(keyword,'noreci').eq.1)
  lnumdiag = (index(keyword,' numd').ne.0.or.index(keyword,'numd').eq.1)
  loldinten = (index(keyword,'oldi').eq.1.or.index(keyword,' oldi').ne.0)
  lopt = (index(keyword,' opti').ne.0.or.index(keyword,'opti').eq.1)
  loptcellpar = (index(keyword,' ocel').ne.0.or.index(keyword,'ocel').eq.1)
  lpacha = (index(keyword,' pac').ne.0.or.index(keyword,'pac').eq.1)
  lphon = (index(keyword,' phon').ne.0.or.index(keyword,'phon').eq.1)
  lposidef = (index(keyword,' posi').ne.0.or.index(keyword,'posi').eq.1)
  lpot = (index(keyword,' pot').ne.0.or.index(keyword,'pot').eq.1)
  lpredict = (index(keyword,' pred').ne.0.or.index(keyword,'pred').eq.1)
  lpreserveQ = (index(keyword,' pres').ne.0.or.index(keyword,'pres').eq.1)
  lPrintEAM = (index(keyword,' prt_eam').ne.0.or.index(keyword,'ptr_eam').eq.1)
  lPrintFour = (index(keyword,' prt_fo').ne.0.or.index(keyword,'ptr_fo').eq.1)
  lPrintThree = (index(keyword,' prt_th').ne.0.or.index(keyword,'ptr_th').eq.1)
  lPrintTwo = (index(keyword,' prt_two').ne.0.or.index(keyword,'ptr_two').eq.1)
  lprop = (index(keyword,' prop').ne.0.or.index(keyword,'prop').eq.1)
  lPureCoulomb0D = (index(keyword,'pure').ne.0)
  lqbond = (index(keyword,' qbon').ne.0.or.index(keyword,'qbon').eq.1)
  lqeq = (index(keyword,' qeq').ne.0.or.index(keyword,'qeq').eq.1)
  lqtpie = (index(keyword,' qtp').ne.0.or.index(keyword,'qtp').eq.1)
  lquicksearch = (index(keyword,' quic').ne.0.or.index(keyword,'quic').eq.1)
  lreaxFFQ = (index(keyword,' norx').eq.0.and.index(keyword,'norx').ne.1)
  lrelax = ((index(keyword,' rela').ne.0.or.index(keyword,'rela').eq.1).and.lfit)
  lregionforce = (index(keyword,' preg').ne.0.or.index(keyword,'preg').eq.1)
  lrest = (index(keyword,' rest').ne.0.or.index(keyword,'rest').eq.1)
  lrfo = (index(keyword,' rfo').ne.0.or.index(keyword,'rfo').eq.1.or.index(keyword,'tran').ne.0)
  lSandM = (index(keyword,' sm').ne.0.or.index(keyword,'sm ').eq.1)
  lSandMnozz = (index(keyword,' smzz').eq.0.and.index(keyword,'smzz').ne.1)
  lsave = (index(keyword,' save').ne.0.or.index(keyword,'save').eq.1)
  lsegsmooth = (index(keyword,' nosmo').eq.0.and.index(keyword,'nosmo').ne.1)
  lshello = (index(keyword,' shel').ne.0.or.index(keyword,'shel').eq.1)
  lsiteenergy = (index(keyword,' site').ne.0.or.index(keyword,'site').eq.1)
  lspatial = (index(keyword,' spat').ne.0.or.index(keyword,'spat').eq.1)
  lstaticfirst = (index(keyword,' stat').ne.0.or.index(keyword,'stat').eq.1)
  lstressout = (index(keyword,' stre').ne.0.or.index(keyword,'stre').eq.1)
  lsym = (index(keyword,' nosy').eq.0.and.index(keyword,'nosy').ne.1)
  lsymdok = (index(keyword,' nosd').eq.0.and.index(keyword,'nosd').ne.1)
  lsymoff = (index(keyword,' symoff').ne.0.or.index(keyword,'symoff').eq.1)
  lthermal = (index(keyword,' ther').ne.0.or.index(keyword,'ther').eq.1)
  ltors = (index(keyword,' tors').ne.0.or.index(keyword,'tors').eq.1)
  ltran = (index(keyword,' tran').ne.0.or.index(keyword,'tran').eq.1)
  lunit = (index(keyword,' unit').ne.0.or.index(keyword,'unit').eq.1)
  lzsisa = (index(keyword,' zsis').ne.0.or.index(keyword,'zsis').eq.1)
!
!  Neutron Keywords (ers29)
!
  lmakeeigarray = (index(keyword,' make').ne.0.or.index(keyword,'make').eq.1)
  lcoreinfo = (index(keyword,' core').ne.0.or.index(keyword,'core').eq.1)
  lnoksym = (index(keyword,' noks').ne.0.or.index(keyword,'noks').eq.1)
  lpdf = (index(keyword,' pdf').ne.0.or.index(keyword,'pdf').eq.1)
  lfreqcut = (index(keyword,' pdfc').ne.0.or.index(keyword,'pdfc').eq.1)
  lkeepcut = (index(keyword,' pdfk').ne.0.or.index(keyword,'pdfk').eq.1)
  lnowidth = (index(keyword,' nowi').ne.0.or.index(keyword,'nowi').eq.1)
  lpartial = (index(keyword,' nopa').eq.0.or.index(keyword,'nopa').eq.1)
!*******************************
!  Handle keyword dependances  *
!*******************************
!
!  lflags depends on other keywords
!
  if (lnoflags) then
    lflags = .false.
  else
    if (lopt.or.lgrad.or.lfit.or.lrfo.or.lmc.or.lmd.or.lneb) then
      if ((.not.lconp).and.(.not.lconv).and.(.not.lcello)) lflags = .true.
    endif
  endif
!
!  PDF dependances: (ers29)
!
  if (lkeepcut) lfreqcut = .true.
  if (lfreqcut.or.lkeepcut) lpdf = .true.
  if (lpdf) then
    lmakeeigarray = .true.
    lsym = .false.
    if (.not.(index(keyword,' full').ne.0.or.index(keyword,'full').eq.1)) then
      write(keyword,*) trim(adjustl(keyword)), " full"
    endif
  endif
  if (lcoreinfo) lphon = .true.
  if (lmakeeigarray) then
    lphon = .true.
    leigen = .true.
    lnoksym = .true.
    lpdfout = .true.
  endif
!
!  If CI-NEB is requested then turn on NEB too
!
  if (lnebclimbingimage) lneb = .true.
!
!  If defect calc, then we need properties
!
  if (ldefect.and..not.lrest) lprop = .true.
!
!  If prop calc, then strain derivatives are needed
!
  if (lprop) then
    lstr = .true.
  endif
!
!  If prop calc, set flag for Born effective charges to be true
!
  if (lprop) then
    lborn = .true.
  endif
!
!  If atomic stresses are requested then strain derivatives are needed
!
  if (latomicstress) then
    lstr = .true.
  endif
!
!  If symmetry is to be lowered from imaginary modes
!  then we need nosym and phonon calc
!
  if (index(keyword,'lowe').ne.0) then
    lsym = .false.
    lphon = .true.
  endif
!
!  If IR intensities or eigenvectors requested, then must be a phonon calc
!
  if (linten.or.leigen) lphon = .true.
!
!  If this is a thermal conductivity calculation then it also must be a phonon calc
!
  if (lthermal) lphon = .true.
!
!  If transition state calc, then lopt and lrfo must be true
!
  if (ltran) then
    morder = 1
    lopt = .true.
    lrfo = .true.
  endif
!
!  If number of processors is greater than 1 set conjugate gradients as optimiser
!
  if (nprocs.gt.1.and..not.llbfgs) lconj = .true.
!
!  Change default update for unit hessian
!
  if (lunit.and.lfit) then
    nfupdate = 100
  endif
!
!  If QEq or SM or Pacha, then leem must be true
!
  if (lqeq.or.lSandM.or.lqtpie.or.lpacha) leem = .true.
!
!  If variable charge then there are no third derivatives
!  but we do need charge second derivatives
!
  if (leem) then
    lnoanald3 = .true.
    lDoQDeriv1 = .false.
    lDoQDeriv2 = .true.
  endif
!
!  If bond order charge potentials or ReaxFF are present turn off symmetry for derivatives
!
  if (nboQ.gt.0.or.lreaxFF.or.lEDIP) then
    lsymdok = .false.
  endif
!
!  If COSMIC then set cosmo to be true
!
  if (lcosmic) lcosmo = .true.
!
!  If COSMO then there are no third derivatives and symmetry
!  must not be enabled for derivatives.
!
  if (lcosmo) then
    lnoanald3 = .true.
    lsymdok = .false.
  endif
!
!  If option to store vectors has been set then reset maxat arrays
!
  if (lStoreVectors) then
    call changemaxat
  endif
!
  return
  end
