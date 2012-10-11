C **********************************************************************
C                                                      SUBROUTINE POSSOL
C                                                      *****************
C
      SUBROUTINE POSSOL(IAP)
C
C ---------------------------------------------------------------
C
C
C          Posicion del Sol, J. Meeus
C          Astronomical Formulae for Calculators
C          Cap. 18, pag. 79 y siguientes
C
C          Subrutinas escritas por M. Cornide
C ---------------------------------------------------------------
C IAP=1 ---> coordenadas aparentes
C IAP=0 ---> coordenadas no aparentes
C
      IMPLICIT NONE
C---> argumentos ficticios
      INTEGER IAP
C---> variables globales: ENTRADA
      REAL*8 FJ
C---> variables globales: SALIDA
      REAL ARSUN,DECSUN
C---> variables locales
      REAL*8 PI,RAG
      REAL*8 ALAMBDA,PAR
      REAL*8 AR,DEC
C---> common blocks
      COMMON/BLKFJ2/FJ
      COMMON/BPOSOL/ARSUN,DECSUN
C
      PI=3.141592654D0
      RAG=180.0D0/PI
      CALL PSOL(FJ,IAP,ALAMBDA,PAR)
      CALL ECUAT(FJ,ALAMBDA,0.D0,AR,DEC)
      ARSUN=AR*RAG/15.D0
      DECSUN=DEC*RAG
      END
C ********************************************************************
C
C
      SUBROUTINE PSOL(FJ,IAP,ALON,PAR)
C
C             AL : Long. media
C             AM : Anomalia media
C             EXC: Excentricidad
C             EC : Ecuacion del centro
C             V  : Anomalia verdadera
C             R  : Radiovector
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PI=3.141592654D0
      RAG=180.0D0/PI
      DPI=2.0D0*PI
      T=(FJ-2415020.D0)/36525.D0
C
         AL=279.69668D0+36000.76892D0*T+.0003025D0*T*T
         AM=358.47583D0+35999.04975D0*T-0.000150D0*T*T
     A     +.0000033D0*T*T*T
         AL=DMOD(AL,360.D0)/RAG
         AM=DMOD(AM,360.D0)/RAG
         EXC=.016751014D0-.0000418D0*T-.000000126D0*T*T
         C1=1.919460D0-.004789D0*T-.000014D0*T*T
         C2=.020094D0-.000100D0*T
         EC=(C1*DSIN(AM)+C2*DSIN(2.*AM)+.000293D0*DSIN(3.*AM))/RAG
C        !
         ALON=AL+EC
         V=AM+EC
         R=1.0000002D0*(1.D0-EXC*EXC)/(1.D0+EXC*DCOS(V))
C        !
C        !    Perturbaciones
C        !
         A=(153.23D0+22518.7541D0*T)/RAG
         B=(216.57D0+45037.5082D0*T)/RAG
         C=(312.69D0+32964.3577D0*T)/RAG
         D=(350.74D0+445267.1142D0*T-.00144D0*T*T)/RAG
         E=(231.19D0+20.20D0*T)/RAG
         H=(353.40D0+65928.7155D0*T)/RAG
C        !
         A=DMOD(A,DPI)
         B=DMOD(B,DPI)
         C=DMOD(C,DPI)
         D=DMOD(D,DPI)
         H=DMOD(H,DPI)
         E=DMOD(E,DPI)
C        !
         ALON=ALON+(.00134D0*DCOS(A)+.00154D0*DCOS(B)
     A            +.00200D0*DCOS(C)+.00179D0*DSIN(D)
     A            +.00178D0*DSIN(E))/RAG
         R=R+.00000543D0*DSIN(A)+.00001575D0*DSIN(B)
     A      +.00001627D0*DSIN(C)+.00003076D0*DCOS(D)
     A      +.00000927D0*DSIN(H)
C
         IF(IAP .EQ. 1) THEN
            OM=(259.18D0-1934.142D0*T)/RAG
            OM=DMOD(OM,DPI)
            IF(OM .LT. 0.) OM=OM+DPI
            ALON=ALON-(0.00569D0+0.00479D0*DSIN(OM))/RAG
         ENDIF
         RR=R*1.49597870D11/6.37814D6
         PAR=ASIN(1.D0/RR)*RAG
C
      RETURN
      END
C
C
C
C
      SUBROUTINE EPSILON(FJ,EPSI)
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C
C  ----- Calculo del parametro U:
C
      U=(FJ-2451545.)/3652500.
C
C  ----- Calculo de la oblicuidad de la ecliptica
C
      A1=2.18D0-3375.70D0*U+0.36D0*U**2
      A2=3.51D0+125666.39D0*U+0.10D0*U**2
      EPSI=0.4090928D0-0.0226938D0*U-75.D-7*U**2+96926.D-7*U**3
     2 -2491.D-7*U**4-12104.D-7*U**5+(446*DCOS(A1)+28*DCOS(A2))*1.D-7
      RETURN
      END
C
C
C
C
      SUBROUTINE ECUAT(FJ,AL,B,A,D)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL EPSILON
C
C           Calcula  Ascension Recta y Declinacion
C
      DPI=6.283185307D0
      CALL EPSILON(FJ,EPS)
C
      X0=DCOS(B)*DCOS(AL)
      Y0=DCOS(B)*DSIN(AL)
      Z0=DSIN(B)
C
      X=X0
      Y=Y0*DCOS(EPS)-Z0*DSIN(EPS)
      Z=Y0*DSIN(EPS)+Z0*DCOS(EPS)
C
      A=ATAN2(Y,X)
      IF(A .LT. 0.) A=A+DPI
      D=ASIN(Z)
C
      RETURN
      END
