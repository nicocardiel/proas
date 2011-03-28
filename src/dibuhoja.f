c
c *********************************************************************
c
      subroutine dibuhoja(OPCMEN)
c
      implicit none
      CHARACTER*1 OPCMEN
      real tuini,TUCREPV,TUCREPM,tufin
      real tuini_,tufin_
      integer tu
c
      real pi
      parameter(pi=3.141593)
      integer i
ccc      integer cn
      integer style
      real ri
      real x,y
      real radio
      character*20 cnum
c limites grafica (altura,tu)
      real xmin,xmax,ymin,ymax
c centro grafica (acimut,altura) y radio
      real xcen,ycen,radg
c
      common/blktu/tuini,TUCREPV,TUCREPM,tufin
      common/blktu_/tuini_,tufin_
      common/blkbox/xmin,xmax,ymin,ymax
      common/blkcir/xcen,ycen,radg
      common/blksty/style
      IF(OPCMEN.EQ.'1')THEN
c
c limites grafica (altura,tu) en milimetros
        xmin=110
        xmax=270
        ymin=90
        ymax=180
c centro grafica (acimut,altura) y radio en milimetros
        xcen=145
        ycen=42
        radg=35
      ELSE IF(OPCMEN.EQ.'2')THEN
        XMIN=15
        XMAX=270
        YMIN=10
        YMAX=180
      ELSE IF(OPCMEN.EQ.'3')THEN
        XMIN=15
        XMAX=215
        YMIN=10
        YMAX=180
      ELSE IF(OPCMEN.EQ.'5')THEN
        XMIN=15
        XMAX=270
        YMIN=10
        YMAX=180
      END IF
c
      call pgvport(0.,1.,0.,1.)
      call pgwindow(0.,1.,0.,1.)
      call pgsfs(2)
      IF(OPCMEN.EQ.'1') call pgscf(2)
      call pgsls(1)
      call pgsch(0.75)
c dibujamos un doble borde exterior
      IF((OPCMEN.NE.'3').OR.(OPCMEN.EQ.'5'))THEN
        call pgrect(.01,.99,.01,.99)
        call pgrect(.013,.987,.014,.986)
      END IF
      call pgvport(.013,.987,.014,.986)
c definimos caja interior en milimetros
      call pgwindow(0.,279.5,0.,193.)
c
c caja grafica (altura,tu)
      call pgrect(xmin,xmax,ymin,ymax)
c eje vertical (de 0 a 90 grados)
      do i=0,90,10
        y=ymin+(ymax-ymin)*real(i)/90
        IF((OPCMEN.EQ.'3').OR.(OPCMEN.EQ.'5'))THEN
          CALL PGMOVE(XMIN,Y)
          CALL PGDRAW(XMIN+(XMAX-XMIN)/50.,Y)
          CALL PGMOVE(XMAX,Y)
          CALL PGDRAW(XMAX-(XMAX-XMIN)/50.,Y)
        ELSE
          CALL PGSLS(4)
          CALL PGMOVE(XMIN,Y)
          CALL PGDRAW(XMAX,Y)
          CALL PGSLS(1)
        END IF
        write(cnum,'(i2.2)')i
        call pgptext(xmin-1,y-1.5,0.,1.,cnum(1:2))
      end do
      call pgptext(xmin-8,ymax,90.,1.,'(Altitude \\uo\\d)')
c eje vertical sec(z) para valores concretos
      y=ymin+(ymax-ymin)*75/90
      call pgptext(xmax+3.0*(xmax-xmin)/100.,y,90.,0.5,'sec(z)')
      y=ymax
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.0')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.1)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.1')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.2)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.2')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.3)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.3')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.4)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.4')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.5)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.5')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/2.0)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'2.0')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/3.0)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'3.0')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/4.0)*180./pi)/90
      call pgmove(xmax,y)
      call pgdraw(xmax-(xmax-xmin)/100.,y)
      call pgptext(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'4.0')
c eje horizontal (de tuini_ a tufin_)
      tu=int(tuini_)
      if(real(tu).eq.tuini_)call ejex(tu,OPCMEN)
   10 continue
      tu=tu+1
      if(tu.le.int(tufin_))then
        call ejex(tu,OPCMEN)
        goto 10
      end if
      call pgptext(xmax,ymin-8,0.,1.,'(UT)')
C ocaso del Sol
      x=xmin+(xmax-xmin)*(TUINI-tuini_)/(tufin_-tuini_)
      CALL PGSLS(1)
      CALL PGMOVE(X,YMIN)
      CALL PGDRAW(X,YMAX)
      CALL PGSLS(1)
      CALL PGSCH(0.5)
      CALL PGPTEXT(X,YMAX+4,0.,.5,'sunset')
      CALL FLECHA(X,YMAX,OPCMEN,3.0)
C fin crepusculo astronomico vespertino
      x=xmin+(xmax-xmin)*(TUCREPV-tuini_)/(tufin_-tuini_)
      CALL PGSLS(2)
      CALL PGMOVE(X,YMIN)
      CALL PGDRAW(X,YMAX)
      CALL PGSLS(1)
      CALL PGPTEXT(X,YMAX+10,0.,.5,'end of')
      CALL PGPTEXT(X,YMAX+ 7,0.,.5,'twilight')
      CALL FLECHA(X,YMAX,OPCMEN,6.0)
C inicio crepusculo astronomico matutino
      x=xmin+(xmax-xmin)*(TUCREPM-tuini_)/(tufin_-tuini_)
      CALL PGSLS(2)
      CALL PGMOVE(X,YMIN)
      CALL PGDRAW(X,YMAX)
      CALL PGSLS(1)
      CALL PGPTEXT(X,YMAX+10,0.,.5,'beginning of')
      CALL PGPTEXT(X,YMAX+ 7,0.,.5,'twilight')
      CALL FLECHA(X,YMAX,OPCMEN,6.0)
C fin crepusculo astronomico vespertino
      x=xmin+(xmax-xmin)*(TUFIN-tuini_)/(tufin_-tuini_)
      CALL PGSLS(1)
      CALL PGMOVE(X,YMIN)
      CALL PGDRAW(X,YMAX)
      CALL PGSLS(1)
      CALL PGPTEXT(X,YMAX+4,0.,.5,'sunrise')
      CALL FLECHA(X,YMAX,OPCMEN,3.0)
      CALL PGSCH(0.75)
C
      IF(OPCMEN.EQ.'1')THEN
c
c grafica (acimut,altura)
c radio 0 grados     
        radio=radg*(90-0)/90
        do i=1,360
          ri=real(i)*pi/180.
          x=-(radio*sin(ri))
          y=-(radio*cos(ri))
          x=x+xcen
          y=y+ycen
          call pgpoint(1,x,y,-1)
        end do
c
c radio 30 grados     
        radio=radg*(90-30)/90
        do i=1,72
          ri=10*real(i)/2.*pi/180.
          x=-radio*sin(ri)
          y=-radio*cos(ri)
          x=x+xcen
          y=y+ycen
          call pgpoint(1,x,y,-1)
        end do
c
c radio 60 grados     
        radio=radg*(90-60)/90
        do i=1,36
          ri=20*real(i)/2.*pi/180.
          x=-radio*sin(ri)
          y=-radio*cos(ri)
          x=x+xcen
          y=y+ycen
          call pgpoint(1,x,y,-1)
        end do
c
c ejes de la grafica (acimut,altura)
        call pgmove(xcen,ycen+radg)
        call pgdraw(xcen,ycen-radg)
        call pgmove(xcen-radg,ycen)
        call pgdraw(xcen+radg,ycen)
        call pgptext(xcen+radg+1,ycen-1.5,0.,0.,'E (270\\uo\\d)')
        call pgptext(xcen-radg-1,ycen-1.5,0.,1.,'W (90\\uo\\d)')
        call pgptext(xcen,ycen+radg+1,0.,.5,'N (180\\uo\\d)')
        call pgptext(xcen,ycen-radg-5,0.,.5,'S (0\\uo\\d)')
c eje NORTE-SUR
        do i=10,80,10
          y=radg*(90-real(i))/90+ycen
          call pgmove(xcen-.5,y)
          call pgdraw(xcen+.5,y)
          y=ycen-radg*(90-real(i))/90
          call pgmove(xcen-.5,y)
          call pgdraw(xcen+.5,y)
        end do
c eje OESTE-ESTE
        do i=10,80,10
          x=radg*(90-real(i))/90+xcen
          call pgmove(x,ycen-.5)
          call pgdraw(x,ycen+.5)
          x=xcen-radg*(90-real(i))/90
          call pgmove(x,ycen-.5)
          call pgdraw(x,ycen+.5)
        end do
c
      END IF
C
      end
c
c *********************************************************************
c
      subroutine ejex(tu0,OPCMEN)
c
      implicit none
      integer tu,tu0
      character*1 OPCMEN
      real xmin,xmax,ymin,ymax
!     real tufin,TUCREPV,TUCREPM,tuini
      real tuini_,tufin_
      real x
ccc      real y
      character*20 cnum
c
!     common/blktu/tuini,TUCREPV,TUCREPM,tufin
      common/blktu_/tuini_,tufin_
      common/blkbox/xmin,xmax,ymin,ymax
c
      tu=tu0
      x=xmin+(xmax-xmin)*(real(tu)-tuini_)/(tufin_-tuini_)
      IF((OPCMEN.EQ.'3').OR.(OPCMEN.EQ.'5'))THEN
        CALL PGMOVE(X,YMIN)
        CALL PGDRAW(X,YMIN+(YMAX-YMIN)/100.)
        CALL PGMOVE(X,YMAX)
        CALL PGDRAW(X,YMAX-(YMAX-YMIN)/100.)
      ELSE
        CALL PGSLS(4)
        CALL PGMOVE(X,YMIN)
        CALL PGDRAW(X,YMAX)
        CALL PGSLS(1)
      END IF
      if(tu.ge.24)tu=tu-24
      write(cnum,'(i2.2)')tu
      if(tu.lt.10)then
        call pgptext(x,ymin-4,0.,0.5,cnum(2:2))
      else
        call pgptext(x,ymin-4,0.,0.5,cnum(1:2))
      end if
      end
c
c *********************************************************************
c
      SUBROUTINE FLECHA(X,Y,OPCMEN,DY)
C
      IMPLICIT NONE
C
      CHARACTER*1 OPCMEN
      REAL X,Y,DY
C
      CALL PGMOVE(X,Y+.5)
      CALL PGDRAW(X,Y+.5+DY)
      IF((OPCMEN.NE.'3').AND.(OPCMEN.NE.'5'))THEN
        CALL PGMOVE(X,Y+.5)
        CALL PGDRAW(X+.5,Y+1.5)
        CALL PGMOVE(X,Y+.5)
        CALL PGDRAW(X-.5,Y+1.5)
      END IF
      END
