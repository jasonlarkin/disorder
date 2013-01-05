      subroutine label(inatin,itype,string)
C
C  Generate atom label from atomic number and type number
C
C  inat   = atomic number
C  itype  = type number
C  string = contains label on exit
C
C  Conditions of use:
C
C  GULP is available free of charge to academic institutions
C  and non-commerical establishments only. Copies should be
C  obtained from the author only and should not be distributed
C  in any form by the user to a third party without the express
C  permission of the author. This notice applies to all parts
C  of the program, except any library routines which are
C  distributed with the code for completeness. All rights for
C  such routines remain with the original distributor.
C
C  No claim is made that this program is free from errors and
C  no liability will be accepted for any loss or damage that
C  may result. The user is responsible for checking the validity
C  of their results.
C
C  Copyright Imperial College 1997
C
C  Julian Gale, Jan 1993
C
      use element
      implicit real(dp) (a-h,o-z), integer(i4) (i-n)
      character*1 space,numstr(10)
      character*2 sym
C
C  Change from character*5 for JRH changes:
      character*(*) string
C
      data numstr/'0','1','2','3','4','5','6','7','8','9'/
      space=' '
      string=' '
      inat=inatin
      if (inat.gt.maxele) inat=inat-maxele
C
C  Check how many characters are in the symbol so that the number
C  is correctly placed into the string
C
      sym=atsym(inat)
      string(1:1)=sym(1:1)
      if (sym(2:2).eq.space) then
        nptr=2
      else
        nptr=3
        string(2:2)=sym(2:2)
      endif
      if (itype.eq.0) return
C
C  Insert number
C
      ihundreds=(itype/100)
      itype=itype-ihundreds*100
      itens=(itype/10)
      iunits=itype-10*itens
      if (ihundreds.gt.0) then
        string(nptr:nptr)=numstr(ihundreds+1)
        nptr=nptr+1
      endif
      if (itens.gt.0.or.ihundreds.gt.0) then
        string(nptr:nptr)=numstr(itens+1)
        nptr=nptr+1
      endif
      string(nptr:nptr)=numstr(iunits+1)
      return
      end
