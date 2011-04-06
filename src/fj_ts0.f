C **********************************************************************
C                                                      SUBROUTINE FJ_TS0
C                                                      *****************
C
      SUBROUTINE FJ_TS0
C
C Calcula la fecha juliana y el Tiempo Sidereo en Greenwich a 0h UT a
C partir de los datos de una fecha concreta. En la fecha juliana se
C tiene en cuenta la fraccion del dia correspondiente a la hora
C traspasada a traves de la variable global HORA.
C Los calculos se realizan en doble precision para no perder informacion
C por redondeo.
C
      IMPLICIT NONE
C---> variables globales: ENTRADA
      REAL ANO,MES,DIA,HORA
C---> variables globales: SALIDA
      REAL TS0
      REAL*8 FJ
C---> variables locales
      INTEGER*4 FECHA
      REAL*8 M,A,B,Y,DT,DTS0
      REAL*8 DANO,DMES,DDIA
C
      COMMON/BLKFJ1/ANO,MES,DIA,HORA
      COMMON/BLKFJ2/FJ
      COMMON/BLKFJ3/TS0
C
      DANO=DBLE(ANO)
      DMES=DBLE(MES)
      DDIA=DBLE(DIA)
      FECHA=INT(ANO)*10000+INT(MES)*100+INT(DIA)
      IF(FECHA.GE.15821015)THEN
        A=DINT(DANO/1.D2)
        B=2.D0-A+DINT(A/4.D0)
      ELSE
        B=0.D0
      END IF
      IF(INT(ANO).GE.0)THEN
        A=0.D0
      ELSE
        A=-.75D0
      END IF
      IF(INT(MES).GE.3)THEN
        Y=DANO
        M=DMES
      ELSE
        Y=DANO-1.D0
        M=DMES+1.2D1
      END IF
      FJ=DINT(365.25D0*Y+A)+DINT(30.6001D0*(M+1.D0))+DDIA+B+
     +   1720994.5D0
      DT=(FJ-2451545.D0)/36525.D0
      DTS0=6.D0*3.6D3+41.D0*6.D1+50.54851D0+8640184.812866D0*DT
     +     +.093104D0*DT*DT-6.2D-6*DT*DT*DT
      DTS0=DMOD(DTS0,8.64D4)
      DTS0=DTS0/8.64D4
      DTS0=DTS0*2.4D1
      TS0=REAL(DTS0)
      IF(TS0.LT.0.)TS0=TS0+24.
      IF(TS0.GT.24.)TS0=TS0-24.
      FJ=FJ+DBLE(HORA/24.)
      END
