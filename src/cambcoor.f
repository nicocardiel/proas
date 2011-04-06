C **********************************************************************
C                                                    SUBROUTINE CAMBCOOR
C                                                    *******************
C
      SUBROUTINE CAMBCOOR(HOR,DEC,ACI,ALT)
C
C Transformaciones de coordenadas:
C (angulo horario,declinacion) ----> (acimut,altura)
C
      IMPLICIT NONE
C---> parametros
      REAL PI
      PARAMETER(PI=3.141593)
C---> argumentos ficticios: ENTRADA
      REAL HOR,DEC
C---> argumentos ficticios: SALIDA
      REAL ACI,ALT
C---> variables globales
      REAL LAT,LONG
C---> variables locales
      REAL HORR,DECR
      REAL LATR
      REAL SENOAC,COSEAC
C---> common blocks
      COMMON/BLKOBS/LAT,LONG
C
      HORR=HOR*PI/180.*15.
      DECR=DEC*PI/180.
      LATR=LAT*PI/180.
      SENOAC=COS(DECR)*SIN(HORR)
      COSEAC=-(SIN(DECR)*COS(LATR))+COS(DECR)*SIN(LATR)*COS(HORR)
      ACI=ATAN2(SENOAC,COSEAC)
      ACI=ACI*180./PI
      IF(ACI.LT.0.)ACI=ACI+360.
      ALT=ASIN(SIN(LATR)*SIN(DECR)+COS(LATR)*COS(DECR)*COS(HORR))
      ALT=ALT*180./PI
      END
