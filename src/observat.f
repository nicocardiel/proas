C **********************************************************************
C                                                    SUBROTUINE OBSERVAT
C                                                    *******************
      SUBROUTINE OBSERVAT(OBS,LAT,LONG,ALTOBS)
C
C Longitud y latitud (grados), y altura (metros) de los observatorios
C con el formato DD.MMSS
C Por convenio longitudes son positivas hacia el Este y negativas hacia
C el Oeste.
C
      IMPLICIT NONE
      INCLUDE 'proas.inc'
C---> argumentos ficticios: SALIDA
      REAL LAT,LONG
      REAL ALTOBS
      CHARACTER*15 OBS
C---> parametros locales
      INTEGER NOBS
      PARAMETER(NOBS=13)
C---> variables locales
      INTEGER I,NOPC
      CHARACTER*15 OBSER(NOBS)
C
      DATA OBSER/'CALAR ALTO','EL TEIDE','MADRID','LA PALMA',
     + 'SAN PEDRO','LICK','LA SILLA','PARANAL','MAUNA KEA','McDonald',
     + 'KPNO','LLANO DEL HATO','CERRO TOLOLO'/
C
      WRITE(*,104)
      WRITE(*,101)'ENTER OBSERVATORY'
      WRITE(*,*)
      WRITE(*,160)0,'Observatory data through keyboard (Longitude,'//
     +              'Latitude,Height)'
      DO I=1,NOBS
        IF(I.LE.9)THEN
          WRITE(*,160)I,OBSER(I)
        ELSE
          WRITE(*,161)I,OBSER(I)
        END IF
      END DO
    5 CONTINUE
      WRITE(*,*)
      WRITE(*,100)'Observatory number '
      NOPC=READI('1')
      IF((NOPC.LT.0).OR.(NOPC.GT.NOBS))THEN
        WRITE(*,101)'ERROR: invalid option. Try again.'
        GOTO 5
      END IF
C
      IF(NOPC.EQ.0)THEN
        WRITE(*,*)
        WRITE(*,100)'Observatory name (max. 15 characters)'
        OBS=READC('@','@')
        WRITE(*,100)'Latitude (+DD.MMSS)'
        LAT=READF('@')
        WRITE(*,100)'Longitude (+DD.MMSS, +E, -W)'
        LONG=READF('@')
        WRITE(*,100)'Height (metres)'
        ALTOBS=READF('@')
      ELSE IF(NOPC.EQ.1)THEN
C Calar Alto
        LAT=37.1305
        LONG=-2.3240
        ALTOBS=2165.
      ELSE IF(NOPC.EQ.2)THEN
C Izaña
        LAT=28.1732
        LONG=-16.2945
        ALTOBS=2400.
      ELSE IF(NOPC.EQ.3)THEN
C Madrid
        LAT=40.2430
        LONG=-3.4115
        ALTOBS=656.
      ELSE IF(NOPC.EQ.4)THEN
C La Palma
        LAT=28.4540
        LONG=-17.5247
        ALTOBS=2334.
      ELSE IF(NOPC.EQ.5)THEN
C San Pedro (datos aproximados extraidos de un mapa mundi)
        LAT=32.0
        LONG=-115
        ALTOBS=2000.
      ELSE IF(NOPC.EQ.6)THEN
C Lick (datos facilitados por Jesus Gallego)
        LAT=37.2036
        LONG=-123.3812
        ALTOBS=1283.
      ELSE IF(NOPC.EQ.7)THEN
C La Silla
        LAT=-29.1500
        LONG=-70.4400
        ALTOBS=2400.0
      ELSE IF(NOPC.EQ.8)THEN
C Paranal
        LAT=-24.3732
        LONG=-70.2410
        ALTOBS=2635.43
      ELSEIF(NOPC.EQ.9)THEN
C UKIRT
        LAT=19.4932
        LONG=-155.2824
        ALTOBS=4194.
      ELSEIF(NOPC.EQ.10)THEN
C McDonald
        LAT=30.4018
        LONG=-104.0118
        ALTOBS=2075.
      ELSEIF(NOPC.EQ.11)THEN
C KPNO
        LAT=31.5748
        LONG=-111.3600
        ALTOBS=2120.
      ELSEIF(NOPC.EQ.12)THEN
C Llano del Hato (Venezuela)
        LAT=08.4724
        LONG=-70.8667
        ALTOBS=3610.
      ELSEIF(NOPC.EQ.13)THEN
C Cerro Tololo (La Serena, Chile)
        LAT=-30.1416
        LONG=-70.4401
        ALTOBS=2738.
      END IF
      IF(NOPC.NE.0)OBS=OBSER(NOPC)
  100 FORMAT(A,$)
  101 FORMAT(A)
  104 FORMAT(79('='))
  160 FORMAT(1X,'(',I1,')',1X,A)
  161 FORMAT('(',I2,')',1X,A)
      END
