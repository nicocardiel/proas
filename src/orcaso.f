C **********************************************************************
C                                                      SUBROUTINE ORCASO
C                                                      *****************
C
      SUBROUTINE ORCASO(DEC,DISZEN)
C
C Calculamos para un objeto concreto, su visibilidad, ortos, ocasos, 
C culminacion y condiciones en las cuales obtiene una distancia
C cenital igual a DISZEN.
C
      IMPLICIT NONE
C---> argumentos ficticios: ENTRADA
      REAL DEC,DISZEN
C---> variables globales: ENTRADA
      REAL LAT,LONG
C---> variables globales: SALIDA
      REAL HORTO,HOCASO
      REAL AORTO,AOCASO
      REAL ALTCUL
      CHARACTER*1 ESTADO,DONCUL      
C---> parametros locales
      REAL PI
      PARAMETER(PI=3.141593)
C---> variables locales
      REAL LATR,DECR
      REAL COSENOH,COSENOA
      REAL ALTR
C---> common blocks
      COMMON/BLKOBS/LAT,LONG
      COMMON/BLKOOC1/ESTADO,DONCUL
      COMMON/BLKOOC2/HORTO,HOCASO,AORTO,AOCASO,ALTCUL
C
C Determinamos visibilidad de los objetos y determinamos su ESTADO
C segun el siguiente criterio
C     ESTADO = I ==> Objeto INVISIBLE
C     ESTADO = C ==> Objeto CIRCUMPOLAR
C     ESTADO = O ==> Objeto con ORTOS y OCASOS
      IF(LAT.GE.0)THEN
        IF(DEC.GE.90.-LAT)THEN
          ESTADO='C'
        ELSE IF(DEC.LE.LAT-90.)THEN
          ESTADO='I'
        ELSE
          ESTADO='O'
        END IF
      ELSE
        IF(DEC.LE.-(90.+LAT))THEN
          ESTADO='C'
        ELSE IF(DEC.GE.90.+LAT)THEN
          ESTADO='I'
        ELSE
          ESTADO='O'
        END IF
      END IF
      LATR=LAT*PI/180.
      DECR=DEC*PI/180.
      ALTR=(90-DISZEN)*PI/180.
      IF(ESTADO.EQ.'O')THEN
        COSENOH=(SIN(ALTR)-SIN(LATR)*SIN(DECR))/(COS(LATR)*COS(DECR))
        if(cosenoh.lt.-1.) cosenoh=-1.
        if(cosenoh.gt.+1.) cosenoh=+1.
        COSENOA=(SIN(ALTR)*SIN(LATR)-SIN(DECR))/(COS(ALTR)*COS(LATR))
        if(cosenoa.lt.-1.) cosenoa=-1.
        if(cosenoa.gt.+1.) cosenoa=+1.
        HOCASO=ACOS(COSENOH)
        HOCASO=HOCASO*180./PI
        AOCASO=ACOS(COSENOA)
        AOCASO=AOCASO*180./PI
        HORTO=360.-HOCASO
        AORTO=360.-AOCASO
      END IF
      HORTO=HORTO/15.
      HOCASO=HOCASO/15.
C Determinamos la altura en las culminaciones superiores
      IF(ESTADO.NE.'I')THEN
        IF(LATR.GE.0)THEN
          IF(DEC.LE.LAT)THEN
            ALTCUL=90.-(LATR-DECR)*180./PI
            DONCUL='S'
          ELSE
            ALTCUL=90.-(DECR-LATR)*180./PI
            DONCUL='N'
          END IF
        ELSE
          IF(DEC.LE.LAT)THEN
            ALTCUL=90.-ABS(DECR-LATR)*180./PI
            DONCUL='S'
          ELSE
            ALTCUL=90.-ABS(LATR-DECR)*180./PI
            DONCUL='N'
          END IF
        END IF
      END IF
      END
