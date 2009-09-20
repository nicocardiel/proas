c
C $Id: dibuhoja.f 558 2007-10-10 16:10:49Z spr $
C
c *********************************************************************
c
      subroutine dibuhoja(OPCMEN)
c
      implicit none
      INTEGER OPCMEN
      real tuini,TUCREPV,TUCREPM,tufin
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
      common/blkbox/xmin,xmax,ymin,ymax
      common/blkcir/xcen,ycen,radg
      common/blksty/style
      IF(OPCMEN.EQ.1)THEN
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
      ELSE IF(OPCMEN.EQ.2)THEN
        XMIN=15
        XMAX=270
        YMIN=10
        YMAX=180
      ELSE IF(OPCMEN.EQ.3)THEN
        XMIN=15
        XMAX=215
        YMIN=10
        YMAX=180
      ELSE IF(OPCMEN.EQ.5)THEN
        XMIN=15
        XMAX=270
        YMIN=10
        YMAX=180
      END IF
c
      call plvpor(0.,1.,0.,1.)
      call plwind(0.,1.,0.,1.)
cp      call plsfs(2)
      IF(OPCMEN.EQ.1) call plfont(2)
      call pllsty(1)
cp      call plsch(0.75)
c dibujamos un doble borde exterior
      IF((OPCMEN.NE.3).OR.(OPCMEN.EQ.5))THEN
CP        call plrect(.01,.99,.01,.99)
CP        call plrect(.013,.987,.014,.986)
      END IF
      call plvpor(.013,.987,.014,.986)
c definimos caja interior en milimetros
      call plwind(0.,279.5,0.,193.)
c
c caja grafica (altura,tu)
CP      call plrect(xmin,xmax,ymin,ymax)
c eje vertical (de 0 a 90 grados)
      do i=0,90,10
        y=ymin+(ymax-ymin)*real(i)/90
        IF((OPCMEN.EQ.3).OR.(OPCMEN.EQ.5))THEN
          CALL pljoin(XMIN,Y,XMIN+(XMAX-XMIN)/50.,Y)
          CALL pljoin(XMAX,Y,XMAX-(XMAX-XMIN)/50.,Y)
        ELSE
          CALL pllsty(4)
          CALL pljoin(XMIN,Y,XMAX,Y)
          CALL pllsty(1)
        END IF
        write(cnum,'(i2.2)')i
        call plptex(xmin-1,y-1.5,0.,1.,cnum(1:2))
      end do
      call plptex(xmin-8,ymax,90.,1.,'(Altitude \\uo\\d)')
c eje vertical sec(z) para valores concretos
      y=ymin+(ymax-ymin)*75/90
      call plptex(xmax+3.0*(xmax-xmin)/100.,y,90.,0.5,'sec(z)')
      y=ymax
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.0')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.1)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.1')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.2)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.2')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.3)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.3')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.4)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.4')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/1.5)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'1.5')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/2.0)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'2.0')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/3.0)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'3.0')
      y=ymin+(ymax-ymin)*(90-ACOS(1.0/4.0)*180./pi)/90
      call pljoin(xmax,y,xmax-(xmax-xmin)/100.,y)
      call plptex(xmax+0.5*(xmax-xmin)/100.,y-1.5,0.,0.,'4.0')
c eje horizontal (de tuini a tufin)
      tu=int(tuini)
      if(real(tu).eq.tuini)call ejex(tu,OPCMEN)
   10 continue
      tu=tu+1
      if(tu.le.int(tufin))then
        call ejex(tu,OPCMEN)
        goto 10
      end if
      call plptex(xmax,ymin-8,0.,1.,'(UT)')
C ocaso del Sol
      x=xmin+(xmax-xmin)*(TUINI-tuini)/(tufin-tuini)
CP      CALL plSCH(0.5)
      CALL plptex(X,YMAX+6,0.,.5,'sunset')
      CALL FLECHA(X,YMAX,OPCMEN)
C fin crepusculo astronomico vespertino
      x=xmin+(xmax-xmin)*(TUCREPV-tuini)/(tufin-tuini)
      CALL pllsty(2)
      CALL pljoin(X,YMIN,X,YMAX)
      CALL pllsty(1)
      CALL plptex(X,YMAX+9,0.,.5,'end of')
      CALL plptex(X,YMAX+6,0.,.5,'twilight')
      CALL FLECHA(X,YMAX,OPCMEN)
C inicio crepusculo astronomico matutino
      x=xmin+(xmax-xmin)*(TUCREPM-tuini)/(tufin-tuini)
      CALL pllsty(2)
      CALL pljoin(X,YMIN,X,YMAX)
      CALL pllsty(1)
      CALL plptex(X,YMAX+9,0.,.5,'beginning of')
      CALL plptex(X,YMAX+6,0.,.5,'twilight')
      CALL FLECHA(X,YMAX,OPCMEN)
C fin crepusculo astronomico vespertino
      x=xmin+(xmax-xmin)*(TUFIN-tuini)/(tufin-tuini)
      CALL plptex(X,YMAX+6,0.,.5,'sunrise')
      CALL FLECHA(X,YMAX,OPCMEN)
CP      CALL plSCH(0.75)
C
      IF(OPCMEN.EQ.1)THEN
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
          call plpoin(1,x,y,-1)
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
          call plpoin(1,x,y,-1)
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
          call plpoin(1,x,y,-1)
        end do
c
c ejes de la grafica (acimut,altura)
        call pljoin(xcen,ycen+radg,xcen,ycen-radg)
        call pljoin(xcen-radg,ycen,xcen+radg,ycen)
        call plptex(xcen+radg+1,ycen-1.5,0.,0.,'E (270\\uo\\d)')
        call plptex(xcen-radg-1,ycen-1.5,0.,1.,'W (90\\uo\\d)')
        call plptex(xcen,ycen+radg+1,0.,.5,'N (180\\uo\\d)')
        call plptex(xcen,ycen-radg-5,0.,.5,'S (0\\uo\\d)')
c eje NORTE-SUR
        do i=10,80,10
          y=radg*(90-real(i))/90+ycen
          call pljoin(xcen-.5,y,xcen+.5,y)
          y=ycen-radg*(90-real(i))/90
          call pljoin(xcen-.5,y,xcen+.5,y)
        end do
c eje OESTE-ESTE
        do i=10,80,10
          x=radg*(90-real(i))/90+xcen
          call pljoin(x,ycen-.5,x,ycen+.5)
          x=xcen-radg*(90-real(i))/90
          call pljoin(x,ycen-.5,x,ycen+.5)
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
      integer tu,tu0,OPCMEN
      real xmin,xmax,ymin,ymax
      real tufin,TUCREPV,TUCREPM,tuini
      real x
ccc      real y
      character*20 cnum
c
      common/blktu/tuini,TUCREPV,TUCREPM,tufin
      common/blkbox/xmin,xmax,ymin,ymax
c
      tu=tu0
      x=xmin+(xmax-xmin)*(real(tu)-tuini)/(tufin-tuini)
      IF((OPCMEN.EQ.3).OR.(OPCMEN.EQ.5))THEN
        CALL pljoin(X,YMIN,X,YMIN+(YMAX-YMIN)/100.)
        CALL pljoin(X,YMAX,X,YMAX-(YMAX-YMIN)/100.)
      ELSE
        CALL pllsty(4)
        CALL pljoin(X,YMIN,X,YMAX)
        CALL pllsty(1)
      END IF
      if(tu.ge.24)tu=tu-24
      write(cnum,'(i2.2)')tu
      if(tu.lt.10)then
        call plptex(x,ymin-4,0.,0.5,cnum(2:2))
      else
        call plptex(x,ymin-4,0.,0.5,cnum(1:2))
      end if
      end
c
c *********************************************************************
c
      SUBROUTINE FLECHA(X,Y,OPCMEN)
C
      IMPLICIT NONE
C
      INTEGER OPCMEN
      REAL X,Y
C
      CALL pljoin(X,Y+.5,X,Y+5.5)
      IF((OPCMEN.NE.3).AND.(OPCMEN.NE.5))THEN
        CALL pljoin(X,Y+.5,X+.5,Y+1.5)
        CALL pljoin(X,Y+.5,X-.5,Y+1.5)
      END IF
      END
