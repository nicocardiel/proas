C
C **********************************************************************
C
C            PROAS: PROGRAMACION DE OBSERVACIONES ASTRONOMICAS
C
C             Versión inicial: 17-diciembre-1997  (N. Cardiel)
C
C **********************************************************************

      PROGRAM PROAS
C
      IMPLICIT NONE
      INCLUDE 'proas.inc'
      INCLUDE 'version.inc'
C
C-----------------------------------------------------------------------
C parametros
      INTEGER NOBJE,NPTOS
      PARAMETER(NOBJE=1000,NPTOS=100)
C
C NOBJE=numero maximo de objetos a observar
C NPTOS=numero maximo de puntos a dibujar en las graficas
C
      REAL PI
      PARAMETER(PI=3.141593)
C-----------------------------------------------------------------------
C numero de puntos que con representamos lineas
      INTEGER NPTOSF
C contadores genericos en bucles
      INTEGER I,K,KK
C numero de objetos leidos
      INTEGER MT
C entero aleatorio entre 1 y 20
      INTEGER IALEAT
C numero de objetos por carta
      INTEGER NOBJEG
C numero de objeto dibujado en una misma grafica
      INTEGER NBODY
C-----------------------------------------------------------------------
C semilla para el generador de numeros aleatorios
      INTEGER*4 NSEED
C funcion externa en C para generar numeros aleatorios
      EXTERNAL RANRED
      REAL RANRED
C-----------------------------------------------------------------------
C latitud, longitud y altura del observatorio
      REAL LAT,LONG,LAT0,LONG0,ALTOBS
      REAL ALH,ALM,ALS
C fecha
      INTEGER IANO,IMES,IDIA
      REAL ANO,MES,MONTH,DIA,HORA
C elementos precesionales
      REAL GIO,ZETA,TETA
C matriz de coeficientes para transformacion precesional
      REAL M(3,3)
C ascension recta y declinacion de los objetos
      REAL AR(NOBJE),DEC(NOBJE)
      REAL ARINI(NOBJE),DECINI(NOBJE)
      REAL AR0,DEC0
C ascension recta y declinacion del Sol
      REAL ARSUN,DECSUN
C coordenadas rectangulares
      REAL X(3),X0(3) 
C fechas iniciales y finales de equinoccio de las coordenadas
      REAL TII,TFF,TI,TF
C tiempo sidereo en Greenwich a 0h T.U.
      REAL TS0
ccc      REAL ATS0H,ATS0M,ATS0S
C angulos horarios y azimuts de ortos y ocasos
      REAL HORTO,HOCASO
      REAL AORTO,AOCASO
C altura de culminaciones
      REAL ALTCUL
C radio terrestre para el observatorio
      REAL REARTH
C semiejes del elipsoide WGS-84
      REAL SEMIA,SEMIB
C angulo de depresion del horizonte debido a la altura
      REAL ANGHOA
C distancia cenital
      REAL DISZEN
C tiempo universal de ortos y ocasos
      REAL TUORTO1,TUORTO2
      REAL TUOCASO1,TUOCASO2
C tiempo universal de fin y principio de crepusculos
      REAL TUCREPV1,TUCREPV2
      REAL TUCREPM1,TUCREPM2
      REAL TUCREPV,TUCREPM
C duracion de la noche cerrada
      REAL DNOCHE
C tiempo universal para calculo de altura de los objetos
      REAL TUINI,TUFIN,TU(NPTOS)
C numero de horas antes puesta sol/despues salida del sol, para diagramas
      REAL DELTAT
C valores de TUINI y TUFIN corregidos de DELTAT
      REAL TUINI_,TUFIN_
C tiempo universal de la culminacion de los objetos
      REAL TUCULM
C tiempo sidereo local y tiempo sidereo en Greenwich
      REAL TSL,TSG
C angulo horario de los objetos para un instante TU determinado
      REAL HOR
C coordenadas horizontales de los objetos
      REAL ACIMUT(NPTOS),ALTURA(NPTOS)
C numero aleatorio entre 0.0 (inclusive) y 1.0 (exclusive)
      REAL RALEAT
C puntero para la posicion de los datos de cada objeto
      REAL YPGP
C-----------------------------------------------------------------------
C fecha juliana
      DOUBLE PRECISION FJ
C-----------------------------------------------------------------------
C numero de opcion de entrada de datos
      CHARACTER*1 NOPC
C opcion en el menu principal del programa
      CHARACTER*1 OPCMEN
C opcion salida grafica
      CHARACTER*1 OPCOUT
C opcion de grabar fichero con objetos y posiciones
      CHARACTER*1 OPC
C signo
      CHARACTER*1 SIGNO
C estado de visibilidad de un objeto
      CHARACTER*1 ESTADO
C direccion de culminaciones
      CHARACTER*1 DONCUL
C nombre de los meses
      CHARACTER*10 MESNAME(12)
C dispositivo salida grafica
      CHARACTER*10 SALIDAG
C nombre de los observatorios
      CHARACTER*15 OBS
C nombre de los objetos a observar
      CHARACTER*30 NOM(NOBJE),NOM0
C nombre del fichero con los datos de los objetos y linea del fichero
      CHARACTER*80 FILNAM,LINEA
C-----------------------------------------------------------------------
C variable logica que determina la existencia del fichero de datos
      LOGICAL FILEX
C variables logicas para determinar si existe fichero de control
      LOGICAL LRUN,LMANUAL
C variable temporal para bucles
      LOGICAL LOOP
C-----------------------------------------------------------------------
      COMMON/BLK1/LINEA
      COMMON/BLK2A/NOM0
      COMMON/BLK2B/AR0,DEC0
      COMMON/BLKFJ1/ANO,MES,DIA,HORA
      COMMON/BLKFJ2/FJ
      COMMON/BLKFJ3/TS0
      COMMON/BPOSOL/ARSUN,DECSUN
      COMMON/BLKOBS/LAT,LONG
      COMMON/BLKOOC1/ESTADO,DONCUL
      COMMON/BLKOOC2/HORTO,HOCASO,AORTO,AOCASO,ALTCUL
      COMMON/BLKTU/TUINI,TUCREPV,TUCREPM,TUFIN
      COMMON/BLKTU_/TUINI_,TUFIN_
      COMMON/BALEAT/IALEAT,NBODY
      COMMON/BLKAAN/TU,ACIMUT,ALTURA,NPTOSF
C-----------------------------------------------------------------------
      NOPC='1'
      OPCMEN='1'
      OPCOUT='1'
C
      DATA MESNAME/'JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE',
     +             'JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER',
     +             'DECEMBER'/
C
      WRITE(*,*)
      WRITE(*,104)
      WRITE(*,101)'                                   '//
     + 'Proas: planning of astronomical observations'
      WRITE(*,101)'                           '//
     + '   Initial Version 18-December-1997, Nicolas Cardiel'
      WRITE(*,101)'                                   '//
     + '         Departamento de Astrofisica, U.C.M.'
      WRITE(*,100) 'Current version: '
      WRITE(*,*) VERSION
C                                                 **********************
C                                                 ESCOGEMOS OBSERVATORIO
C                                                 **********************
      CALL OBSERVAT(OBS,LAT0,LONG0,ALTOBS)
      CALL DECIMAL(LAT0,LAT)
      CALL DECIMAL(LONG0,LONG)
      LONG=LONG/15.
C                                                   ********************
C                                                   INTRODUCIMOS OBJETOS
C                                                   ********************
   10 CONTINUE
      WRITE(*,104)
      WRITE(*,101)'COORDINATES OF THE TARGET LIST:'
      WRITE(*,*)
      WRITE(*,101)'(1) enter object data through keyboard'
      WRITE(*,101)'(2) enter object data from file'
      WRITE(*,*)
      WRITE(*,100)'Option '
      NOPC=READC(NOPC,'12')
      IF(NOPC.EQ.'2')THEN
        WRITE(*,100)'Object data file name'
        FILNAM=READC('@','@')
        INQUIRE(FILE=FILNAM,EXIST=FILEX)
        IF(FILEX)THEN
        ELSE
          WRITE(*,101)'ERROR: this file does not exist.'
          WRITE(*,100)'Press <CR> to continue...'
          READ(*,*)
          CALL LRUNX(LRUN,LMANUAL)
          IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
          GOTO 10
        END IF
      END IF
C
      MT=0
      IF(NOPC.EQ.'1')THEN
   20   CONTINUE
        WRITE(*,100)'Object name (<end> to exit, max. 30 characters) '
        NOM0=READC('end','@')
        CALL CHLOWER(NOM0)
        IF(NOM0.EQ.'end') GOTO 22
        MT=MT+1
        NOM(MT)=NOM0
        WRITE(*,100)'Right ascension (HH.MMSS)'
        ARINI(MT)=READF('@')
        WRITE(*,100)'Declination    (+DD.MMSS)'
        DECINI(MT)=READF('@')
        GOTO 20
      ELSE
        OPEN(UNIT=33,FILE=FILNAM,STATUS='OLD',ERR=990)
   21   CONTINUE
        READ(33,'(A)',END=25)LINEA
        CALL TRASPASA
        MT=MT+1
        NOM(MT)=NOM0
        CALL DECIMAL(AR0,ARINI(MT))
        CALL DECIMAL(DEC0,DECINI(MT))
        WRITE(*,170)NOM(MT),AR0,DEC0
        GOTO 21
      END IF
   22 CONTINUE
      IF(MT.EQ.0)THEN
        WRITE(*,*)
        WRITE(*,101)'ERROR: no. of objects = 0'
        WRITE(*,100)'Press <CR> to continue...'
        READ(*,*)
        CALL LRUNX(LRUN,LMANUAL)
        IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
        GOTO 10
      END IF      
      WRITE(*,100)'Do you want to create an output file with'
      WRITE(*,100)' current object list (y/n) '
      OPC=READC('n','yn')
      IF(OPC.EQ.'y')THEN
        WRITE(*,100)'File name? '
        READ(*,'(A)')FILNAM
        OPEN(UNIT=33,FILE=FILNAM,STATUS='UNKNOWN',ERR=990)
        DO I=1,MT
          WRITE(33,150)NOM(I),ARINI(I),DECINI(I)
        END DO
        CLOSE(UNIT=33)
      END IF
      DO I=1,MT
        CALL DECIMAL(ARINI(I),ARINI(I))
        CALL DECIMAL(DECINI(I),DECINI(I))
      END DO
      GOTO 30
   25 CONTINUE
      CLOSE(UNIT=33)
   30 CONTINUE
      IF(MT.EQ.0)THEN
        WRITE(*,*)
        WRITE(*,101)'ERROR: no. of objects = 0'
        WRITE(*,100)'Press <CR> to continue...'
        READ(*,*)
        CALL LRUNX(LRUN,LMANUAL)
        IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
        GOTO 10
      END IF
      WRITE(*,*)
      WRITE(*,'(A,I3)')'Total number of objets: ',MT
      WRITE(*,*)
      WRITE(*,104)
      WRITE(*,100)'Equinox (for all the objects) '
      TII=READF('2000.0')
C                                                   ********************
C                                                   FECHA DE OBSERVACION
C                                                   ********************
      WRITE(*,104)
   40 CONTINUE
      WRITE(*,101)'ENTER OBSERVING DATE:'
      WRITE(*,*)
      WRITE(*,100)'YEAR          '
      IANO=READI('@')
      ANO=REAL(IANO)
      WRITE(*,100)'MONTH (number)' 
      IMES=READI('@')
      IF((IMES.LT.1).OR.(IMES.GT.12))THEN
        WRITE(*,101)'ERROR: invalid month number.'
        WRITE(*,100)'Press <CR> to continue...'
        READ(*,*)
        CALL LRUNX(LRUN,LMANUAL)
        IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
        GOTO 40
      END IF
      MES=REAL(IMES)
      WRITE(*,100)'DAY           '
      IDIA=READI('@')
      DIA=REAL(IDIA)
C                                                              *********
C                                                              PRECESION
C                                                              *********
      IF(INT(MES).LE.2)THEN
        MONTH=AINT((MES-1)*63./2.)
      ELSE
        MONTH=AINT((MES+1)*30.6)-63.
      END IF
      MONTH=MONTH+DIA
      TFF=ANO+MONTH/365.25
      TI=(TII-2000.)/100.
      TF=(TFF-2000.-100.*TI)/100.
C elementos precesionales en grados
      GIO=((2306.2181+1.39656*TI-.000139*TI*TI)*TF+(.30188-.000344*TI)
     +    *TF*TF+.017998*TF*TF*TF)/3600.
      ZETA=GIO+((.7928+.00041*TI)*TF*TF+.000205*TF*TF*TF)/3600.
      TETA=((2004.3109-.8533*TI-.000217*TI*TI)*TF-(.42665+.000217*TI)
     +     *TF*TF-.041833*TF*TF*TF)/3600.
C los pasamos a radianes
      GIO=GIO*PI/180.
      ZETA=ZETA*PI/180.
      TETA=TETA*PI/180.
C matriz de rotacion
      M(1,1)=-(SIN(GIO)*SIN(ZETA))+COS(GIO)*COS(TETA)*COS(ZETA)
      M(1,2)=-(COS(GIO)*SIN(ZETA))-(SIN(GIO)*COS(ZETA)*COS(TETA))
      M(1,3)=-(SIN(TETA)*COS(ZETA))
      M(2,1)=SIN(GIO)*COS(ZETA)+COS(GIO)*COS(TETA)*SIN(ZETA)
      M(2,2)=COS(GIO)*COS(ZETA)-SIN(GIO)*COS(TETA)*SIN(ZETA)
      M(2,3)=-(SIN(TETA)*SIN(ZETA))
      M(3,1)=COS(GIO)*SIN(TETA)
      M(3,2)=-(SIN(GIO)*SIN(TETA))
      M(3,3)=COS(TETA)
C coordenadas rectangulares del objeto
      DO 410,I=1,MT
        X0(1)=COS(DECINI(I)*PI/180.)*COS(ARINI(I)*15.*PI/180.)
        X0(2)=COS(DECINI(I)*PI/180.)*SIN(ARINI(I)*15.*PI/180.)
        X0(3)=SIN(DECINI(I)*PI/180.)
C cambio a coordenadas de la epoca
        DO 405,K=1,3
          X(K)=0.
          DO 400,KK=1,3
            X(K)=X(K)+X0(KK)*M(K,KK)
  400     CONTINUE
  405   CONTINUE
        AR(I)=ATAN2(X(2),X(1))
        DEC(I)=ASIN(X(3))
        AR(I)=AR(I)*180./PI/15.
        IF(AR(I).LT.0.)AR(I)=AR(I)+24.
        DEC(I)=DEC(I)*180./PI
  410 CONTINUE
C                                                   ********************
C                                                   FECHA JULIANA Y TSG0
C                                                   ********************
C Calculamos la Fecha Juliana(FJ) y el TS a 0h TU en Greenwich(TS0)
      HORA=0.
      CALL FJ_TS0
C                                         ******************************
C                                         POSICION DEL SOL, ORTO Y OCASO
C                                         ******************************
C Iteramos para calcular la posicion del Sol en el instante del ocaso y
C del orto. Tomamos como valor inicial la posicion a 0h T.U. del dia
C de la observacion.
      CALL POSSOL(0)
C DISZEN: refraccion 34'
C         semidiametro del Sol: 16'
C         depresion del horizonte: usamos elipsoide WGS-84
      SEMIA=6378.137
      SEMIB=6356.752
      REARTH=1/SQRT(COS(LAT*PI/180.)*COS(LAT*PI/180.)/SEMIA/SEMIA+
     +       SIN(LAT*PI/180.)*SIN(LAT*PI/180.)/SEMIB/SEMIB)
      ANGHOA=ACOS(REARTH/(REARTH+ALTOBS/1000.))
      ANGHOA=ANGHOA*180./PI
      DISZEN=90.+34./60.+16./60.+ANGHOA
      CALL ORCASO(DECSUN,DISZEN)
      IF(ESTADO.EQ.'C')THEN
        WRITE(*,101)'Sol temporalmente circumpolar sin ortos ni ocasos'
        STOP
      ELSE IF(ESTADO.EQ.'I')THEN
        WRITE(*,101)'Sol temporalmente invisible sin ortos ni ocasos'
        STOP
      END IF
C ocaso del Sol (calculamos el ocaso que tiene lugar a partir de las 0h
C de T.U. del dia de la observacion).
      CALL TIEMPOU(ARSUN,HOCASO,LONG,TS0,TUOCASO1)
  200 CONTINUE
      HORA=TUOCASO1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,DISZEN)
      CALL TIEMPOU(ARSUN,HOCASO,LONG,TS0,TUOCASO2)
ccc     type*,'tuocaso1,2: ',tuocaso1,tuocaso2
      IF(ABS(TUOCASO1-TUOCASO2).GT.12.0)THEN
        IF(TUOCASO1.GT.TUOCASO2)THEN
          TUOCASO2=TUOCASO2+24.
        ELSE
          TUOCASO1=TUOCASO1+24.
        END IF
      END IF
      IF(ABS(TUOCASO1-TUOCASO2).GT.0.0003)THEN
        TUOCASO1=TUOCASO2
        GOTO 200
      END IF
C crepusculo astronomico vespertino (utilizamos como valor inicial en
C la iteracion el instante del ocaso)
      TUCREPV1=TUOCASO2
  201 CONTINUE
      HORA=TUCREPV1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,108.0+ANGHOA)
      IF(ESTADO.EQ.'C')THEN
        WRITE(*,101)'Crepusculo toda la noche'
        STOP
      END IF
      CALL TIEMPOU(ARSUN,HOCASO,LONG,TS0,TUCREPV2)
ccc     type*,'tucrepv1,2: ',tucrepv1,tucrepv2
      IF(ABS(TUCREPV1-TUCREPV2).GT.12.0)THEN
        IF(TUCREPV1.GT.TUCREPV2)THEN
          TUCREPV2=TUCREPV2+24.
        ELSE
          TUCREPV1=TUCREPV1+24.
        END IF
      END IF
      IF(ABS(TUCREPV2-TUCREPV1).GT.0.0003)THEN
        TUCREPV1=TUCREPV2
        GOTO 201
      END IF
C orto del Sol (calculamos el orto que tiene lugar a partir de las 0h
C de T.U. del dia de la observacion)
      TUORTO1=TUOCASO2
  210 CONTINUE
      HORA=TUORTO1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,DISZEN)
      CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUORTO2)
ccc     type*,'tuorto1,2: ',tuorto1,tuorto2
      IF(ABS(TUORTO1-TUORTO2).GT.12.0)THEN
        IF(TUORTO1.GT.TUORTO2)THEN
          TUORTO2=TUORTO2+24.
        ELSE
          TUORTO1=TUORTO1+24.
        END IF
      END IF
      IF(ABS(TUORTO1-TUORTO2).GT.0.0003)THEN
        TUORTO1=TUORTO2
        GOTO 210
      END IF
C crepusculo astronomico matutino (utilizamos como valor inicial en
C la iteracion el instante del orto)
      TUCREPM1=TUORTO2
  211 CONTINUE
      HORA=TUCREPM1
      CALL FJ_TS0
      CALL POSSOL(0)
      CALL ORCASO(DECSUN,108.0+ANGHOA)
      CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUCREPM2)
ccc     type*,'tucrepm1,2: ',tucrepm1,tucrepm2
      IF(ABS(TUCREPM1-TUCREPM2).GT.12.0)THEN
        IF(TUCREPM1.GT.TUCREPM2)THEN
          TUCREPM2=TUCREPM2+24.
        ELSE
          TUCREPM1=TUCREPM1+24.
        END IF
      END IF
      IF(ABS(TUCREPM2-TUCREPM1).GT.0.0003)THEN
        TUCREPM1=TUCREPM2
        GOTO 211
      END IF
C si el orto calculado es anterior al ocaso quiere decir que tenemos
C que recalcular el orto que ocurre para el dia siguiente al dia
C de la observacion.
      IF(TUORTO2.LT.TUOCASO2)THEN
        TUORTO1=TUORTO2
  220   CONTINUE
        HORA=TUORTO1+24
        CALL FJ_TS0
C TS 0h T.U. del dia siguiente al de la observacion
        TS0=TS0+24.*2.737909E-3
        IF(TS0.GT.24.)TS0=TS0-24.
C posicion del Sol para el dia siguiente al de la observacion
        CALL POSSOL(0)
        CALL ORCASO(DECSUN,DISZEN)
        CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUORTO2)
ccc     type*,'tuorto1,2: ',tuorto1,tuorto2

        IF(ABS(TUORTO1-TUORTO2).GT.12.0)THEN
           IF(TUORTO1.GT.TUORTO2)THEN
             TUORTO2=TUORTO2+24.
           ELSE
             TUORTO1=TUORTO1+24.
           END IF
        END IF


        IF(ABS(TUORTO1-TUORTO2).GT.0.0003)THEN
          TUORTO1=TUORTO2
          GOTO 220
        END IF
C crepusculo astronomico matutino (utilizamos como valor inicial en
C la iteracion el instante del crepusculo para el dia anterior)
        TUCREPM1=TUCREPM2
  221   CONTINUE
        HORA=TUCREPM1+24
        CALL FJ_TS0
C TS 0h T.U. del dia siguiente al de la observacion
        TS0=TS0+24.*2.737909E-3
        IF(TS0.GT.24.)TS0=TS0-24.
C posicion del Sol para el dia siguiente al de la observacion
        CALL POSSOL(0)
        CALL ORCASO(DECSUN,108.0+ANGHOA)
        CALL TIEMPOU(ARSUN,HORTO,LONG,TS0,TUCREPM2)
ccc     type*,'tucrepm1,2: ',tucrepm1,tucrepm2
        IF(ABS(TUCREPM2-TUCREPM1).GT.0.0003)THEN
          TUCREPM1=TUCREPM2
          GOTO 221
        END IF
      END IF
C
C limites temporales en los cuales trabajaremos
      TUINI=TUOCASO2
      TUCREPV=TUCREPV2
      TUCREPM=TUCREPM2
      TUFIN=TUORTO2
ccc     type*,TUINI,TUCREPV,TUCREPM,TUFIN
      IF(TUCREPV.LT.TUINI) TUCREPV=TUCREPV+24
      IF(TUCREPM.LT.TUCREPV) TUCREPM=TUCREPM+24
      IF(TUFIN.LT.TUCREPM) TUFIN=TUFIN+24
      LOOP=.TRUE.
      DO WHILE(LOOP)
        WRITE(*,100)'Expansion time (hours) before sunset/after '//
     +   'sunrise '
        DELTAT=READF('0.0')
        IF(DELTAT.GE.0.0) LOOP=.FALSE.
        IF(LOOP)THEN
          WRITE(*,101) 'ERROR: this number must be >= 0.0'
          WRITE(*,*)
        END IF
      END DO
      TUINI_=TUINI-DELTAT
      TUFIN_=TUFIN+DELTAT
ccc     type*,TUINI,TUCREPV,TUCREPM,TUFIN
C recalculamos la FJ y TS0 para el dia de la observacion a 0h TU
      HORA=0.
      CALL FJ_TS0
C                                                       ****************
C                                                       MENU DE OPCIONES
C                                                       ****************
  300 CONTINUE
      WRITE(*,104)
      WRITE(*,*)
      WRITE(*,101)'(1) plot composite chart'
      WRITE(*,101)'(2) plot only altitude vs UT from option (1)'
      WRITE(*,101)'(3) plot altitude vs UT (draft)'
      WRITE(*,101)'(4) change observing date'
      WRITE(*,101)'(0) STOP'
      WRITE(*,*)
      WRITE(*,100)'Option '
      OPCMEN=READC(OPCMEN,'01234')
      IF(OPCMEN.EQ.'0')GOTO 560
      IF(OPCMEN.EQ.'4') GOTO 40
      IF(OPCMEN.EQ.'1')THEN
        WRITE(*,100)'No. of objets/plot '
        NOBJEG=READILIM('10',1,10)
        NPTOSF=100
      ELSE IF(OPCMEN.EQ.'3')THEN
        WRITE(*,100)'No. of objets/plot '
        NOBJEG=READILIM('40',1,40)
ccc     IF(NOBJEG.GT.40) NOBJEG=40
ccc     WRITE(*,100)'Num. puntos que definen la trayectoria '//
ccc     +   'de los objetos '
ccc     NPTOSF=READILIM('50',1,100)
        NPTOSF=100
      ELSE
        WRITE(*,100)'No. of objets/plot '
        NOBJEG=READI('@')
ccc     NPTOSF=50
        NPTOSF=100
ccc        OPEN(UNIT=33,FILE='proas.dat',STATUS='UNKNOWN')
      END IF
      IF(NOBJEG.LT.1)NOBJEG=1
C
  310 CONTINUE
      WRITE(*,104)
      WRITE(*,101)'GRAPHIC OUTPUT: '
      WRITE(*,*)
      WRITE(*,101)'(1) /xserve (terminal)'
      WRITE(*,101)'(2) /ps (PostScript)'
      WRITE(*,100)'(3) show list with available graphic output'
      WRITE(*,100)' devices'
      WRITE(*,*)
      WRITE(*,100)'Option '
      OPCOUT=READC(OPCOUT,'123')
      WRITE(*,*)
C la semilla de los numeros aleatorios es -1 para inicializar el generador en
C la primera llamada
      NSEED=-1
C
      IF(OPCOUT.EQ.'1')THEN
        SALIDAG='/xserve'
      ELSE IF(OPCOUT.EQ.'2')THEN
        SALIDAG='/ps'
      ELSE IF(OPCOUT.EQ.'3')THEN
        SALIDAG='?'
      END IF
C                                        *******************************
C                                        DIBUJAMOS CARTA CON LOS OBJETOS
C                                        *******************************
      CALL PGBEGIN(0,SALIDAG,1,1)
      CALL PGASK(.FALSE.)
      DO 550,I=1,MT
        IF(REAL(I-1)/NOBJEG.EQ.REAL((I-1)/NOBJEG))THEN
          IF(I.NE.1)THEN
            IF(OPCOUT.NE.'2')THEN
              WRITE(*,100)'Press <CR> to continue...'
              READ(*,*)
              CALL LRUNX(LRUN,LMANUAL)
              IF((LRUN).OR.(LMANUAL)) WRITE(*,*)
            END IF
            CALL PGPAGE
          END IF
          NBODY=0.
          CALL DIBUHOJA(OPCMEN)
C***>
            CALL PGSCH(0.7)
C observatorio
            WRITE(LINEA,'(A29)')'Observatory: '//OBS
          IF(OPCMEN.EQ.'1')THEN
            CALL PGPTEXT(200.,72.,0.,0.,LINEA(1:29))
          ELSE IF(OPCMEN.EQ.'2')THEN
            CALL PGPTEXT(80.,185.,0.,0.,LINEA(1:29))
ccc         WRITE(33,104)
ccc         WRITE(33,101)LINEA(1:29)
          ELSE
            CALL PGPTEXT(50.,185.,0.,0.,LINEA(1:29))
          END IF
          IF(OPCMEN.EQ.'1')THEN
C latitud
            CALL PGPTEXT(239.,68.,0.,1.,'Latitude:')
            WRITE(LINEA,'(F8.4)')LAT0
            IF(LINEA(1:1).EQ.' ') LINEA(1:1)='+'
            LINEA(1:18)=LINEA(1:3)//'\\uo\\d '
     +       //LINEA(5:6)//CHAR(39)//' '//LINEA(7:8)//'"'
            CALL PGPTEXT(240.,68.,0.,0.,' '//LINEA(1:18))
C longitud
            CALL PGPTEXT(239.,64.,0.,1.,'Longitude:')
            WRITE(LINEA,'(F9.4)')LONG0
            LINEA(1:18)=LINEA(1:4)//'\\uo\\d '
     +       //LINEA(6:7)//CHAR(39)//' '//LINEA(8:9)//'"'
            CALL PGPTEXT(240.,64.,0.,0.,LINEA(1:18))
C altura
            CALL PGPTEXT(239.,60.,0.,1.,'Height:')
            WRITE(LINEA,'(F5.0,A2)')ALTOBS,' m'
            CALL PGPTEXT(240.,60.,0.,0.,LINEA(1:7))
          END IF
C fecha
            WRITE(LINEA,'(A7,I4.4,A2,I2.2,A11)')'DATE: ',
     +           INT(ANO),', ',INT(DIA),' '//MESNAME(INT(MES))
          IF(OPCMEN.EQ.'1')THEN
            CALL PGPTEXT(200.,52.,0.,0.,LINEA(1:29))
          ELSE IF(OPCMEN.EQ.'2')THEN
            CALL PGPTEXT(150.,185.,0.,0.,LINEA(1:29))
ccc         WRITE(33,101)LINEA(1:29)
          ELSE
            CALL PGPTEXT(120.,185.,0.,0.,LINEA(1:29))
          END IF
          IF(OPCMEN.EQ.'1')THEN
C fecha juliana
            CALL PGPTEXT(239.,48.,0.,1.,'JD (at 0h UT):')
            WRITE(LINEA,'(F9.1)')FJ
            CALL PGPTEXT(240.,48.,0.,0.,LINEA(1:9))
C TSG(0h TU)
            CALL PGPTEXT(239.,44.,0.,1.,'GST (at 0h UT):')
            CALL SEXAG(TS0,ALH,ALM,ALS)
            WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),
     +           'm ',INT(ALS),'s'
            CALL PGPTEXT(240.,44.,0.,0.,LINEA(1:11))
C TSL(0h TU)
            TSL=TS0+LONG
            IF(TSL.GT.24)TSL=TSL-24.
            IF(TSL.LE.0.)TSL=TSL+24.
            CALL SEXAG(TSL,ALH,ALM,ALS)
            CALL PGPTEXT(239.,40.,0.,1.,'LST (at 0h UT):')
            WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),
     +           'm ',INT(ALS),'s'
            CALL PGPTEXT(240.,40.,0.,0.,LINEA(1:11))
C ocaso del Sol
            CALL PGPTEXT(239.,32.,0.,1.,'sunset:')
            CALL SEXAG(TUINI,ALH,ALM,ALS)
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),
     +           'h ',INT(ALM),'m'
            CALL PGPTEXT(240.,32.,0.,0.,LINEA(1:7)//' (UT)')
C fin crepusculo astronomico vespertino
            CALL PGPTEXT(239.,28.,0.,1.,'end of twilight:')
            IF(TUCREPV.GT.24)THEN
              CALL SEXAG(TUCREPV-24.,ALH,ALM,ALS)
            ELSE
              CALL SEXAG(TUCREPV,ALH,ALM,ALS)
            END IF
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),
     +           'h ',INT(ALM),'m'
            CALL PGPTEXT(240.,28.,0.,0.,LINEA(1:7)//' (UT)')
C inicio crepusculo astronomico matutino
            CALL PGPTEXT(239.,24.,0.,1.,'beginning of twilight:')
            IF(TUCREPM.GT.24.)THEN
              CALL SEXAG(TUCREPM-24,ALH,ALM,ALS)
            ELSE
              CALL SEXAG(TUCREPM,ALH,ALM,ALS)
            END IF
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),
     +           'h ',INT(ALM),'m'
            CALL PGPTEXT(240.,24.,0.,0.,LINEA(1:7)//' (UT)')
C orto del Sol
            CALL PGPTEXT(239.,20.,0.,1.,'sunrise:')
            IF(TUFIN.GT.24)THEN
              CALL SEXAG(TUFIN-24.,ALH,ALM,ALS)
            ELSE
              CALL SEXAG(TUFIN,ALH,ALM,ALS)
            END IF
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),
     +           'h ',INT(ALM),'m'
            CALL PGPTEXT(240.,20.,0.,0.,LINEA(1:7)//' (UT)')
C duracion de la noche cerrada
            DNOCHE=TUCREPM-TUCREPV
            IF(TUCREPM.LT.TUCREPV)DNOCHE=24+DNOCHE
            CALL PGPTEXT(249.,16.,0.,1.,'Night time free of twilight:')
            CALL SEXAG(DNOCHE,ALH,ALM,ALS)
            WRITE(LINEA,'(I2.2,A2,I2.2,A1)')INT(ALH),
     +           'h ',INT(ALM),'m'
            CALL PGPTEXT(250.,16.,0.,0.,LINEA(1:7))
C copywrite
            LINEA='\\(0274) cardiel@fis.ucm.es (UCM)'
            CALL PGSCH(0.45)
            CALL PGPTEXT(279.,1.,0.,1.,LINEA(1:TRUELEN(LINEA)))
C***>
          END IF
        END IF
        RALEAT=RANRED(NSEED)
        IALEAT=INT(RALEAT*20)+1
        NBODY=NBODY+1
        DO 520,K=1,NPTOSF
C instante en T.U. en que calculamos la posicon
          TU(K)=TUINI_+(TUFIN_-TUINI_)*REAL(K-1)/REAL(NPTOSF-1)
C calculamos el TSG para el instante TU
          TSG=TS0+TU(K)+TU(K)*2.737909E-3
C TSL para el instante TU
          TSL=TSG+LONG
C angulo horario
          HOR=TSL-AR(I)
C pasamos a coordenadas horizontales
          CALL CAMBCOOR(HOR,DEC(I),ACIMUT(K),ALTURA(K))
  520   CONTINUE
        CALL DIBUOBJE(OPCMEN)
        IF(OPCMEN.EQ.'1')THEN
C***>
          CALL PGSCH(0.6)
C objeto
          WRITE(LINEA,'(A8,I2,A17)')'OBJECT #',NBODY,': '//NOM(I)
          YPGP=1.5+(10.-REAL(NBODY))*19.+14.
          CALL PGPTEXT(3.,YPGP,0.,0.,LINEA(1:38))
C ascension recta epoca inicial
          YPGP=YPGP-4.0
          CALL PGPTEXT(3.,YPGP,0.,0.,'\\ga:')
          CALL SEXAG(ARINI(I),ALH,ALM,ALS)
          WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),
     +         'm ',INT(ALS),'s'
          CALL PGPTEXT(8.,YPGP,0.,0.,LINEA(1:11))
C declinacion epoca inicial
          CALL PGPTEXT(35.,YPGP,0.,0.,'\\gd:')
          CALL SEXAG(DECINI(I),ALH,ALM,ALS)
          SIGNO='+'
          IF(DECINI(I).LT.0)THEN
            SIGNO='-'
            ALH=ABS(ALH)
            ALM=ABS(ALM)
            ALS=ABS(ALS)
          END IF
          WRITE(LINEA,'(A1,I2.2,A,I2.2,A2,I2.2,A1)')SIGNO,INT(ALH),
     +         '\\uo\\d ',INT(ALM),CHAR(39)//' ',INT(ALS),'"'
          CALL PGPTEXT(40.,YPGP,0.,0.,LINEA(1:18))
C epoca inicial
          WRITE(LINEA,'(A1,F6.1,A1)')'(',TII,')'
          CALL PGPTEXT(65.,YPGP,0.,0.,LINEA(1:8))
C ascension recta epoca final
          YPGP=YPGP-4.0
          CALL PGPTEXT(3.,YPGP,0.,0.,'\\ga:')
          CALL SEXAG(AR(I),ALH,ALM,ALS)
          WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),
     +         'm ',INT(ALS),'s'
          CALL PGPTEXT(8.,YPGP,0.,0.,LINEA(1:11))
C declinacion epoca final
          CALL PGPTEXT(35.,YPGP,0.,0.,'\\gd:')
          CALL SEXAG(DEC(I),ALH,ALM,ALS)
          SIGNO='+'
          IF(DEC(I).LT.0)THEN
            SIGNO='-'
            ALH=ABS(ALH)
            ALM=ABS(ALM)
            ALS=ABS(ALS)
          END IF
          WRITE(LINEA,'(A1,I2.2,A,I2.2,A2,I2.2,A1)')SIGNO,INT(ALH),
     +         '\\uo\\d ',INT(ALM),CHAR(39)//' ',INT(ALS),'"'
          CALL PGPTEXT(40.,YPGP,0.,0.,LINEA(1:18))
C epoca final
          WRITE(LINEA,'(A1,F6.1,A1)')'(',TFF,')'
          CALL PGPTEXT(65.,YPGP,0.,0.,LINEA(1:8))
C condiciones de la culminacion
          YPGP=YPGP-4.0
          CALL ORCASO(DEC(I),90+34./60.+ANGHOA)
          IF(ESTADO.EQ.'I')THEN
            CALL PGPTEXT(3.,YPGP,0.,0.,'Object is INVISIBLE')
          ELSE
            CALL PGPTEXT(3.,YPGP,0.,0.,'Culmination: ')
            WRITE(LINEA,'(F4.1,1X,A1)')ALTCUL,DONCUL
            CALL PGPTEXT(30.,YPGP,0.,0.,LINEA(1:6))
            CALL TIEMPOU(AR(I),0.,LONG,TS0,TUCULM)
            CALL SEXAG(TUCULM,ALH,ALM,ALS)
            WRITE(LINEA,'(2(I2.2,A2),I2.2,A1)')INT(ALH),'h ',INT(ALM),
     +           'm ',INT(ALS),'s'
          END IF
          CALL PGPTEXT(45.,YPGP,0.,0.,LINEA(1:11))
C***>
          IF((REAL(I)/NOBJEG.EQ.REAL(I/NOBJEG)).OR.(I.EQ.MT))THEN
          ELSE
            CALL PGSLS(3)
            CALL PGMOVE(1.,YPGP-2.)
            CALL PGDRAW(80.,YPGP-2.)
            CALL PGSLS(1)
          END IF
        END IF
        IF(OPCMEN.EQ.'2')THEN
ccc       WRITE(33,'(A8,I2,A17)')'Object #',NBODY,': '//NOM(I)
        END IF
        IF(OPCMEN.EQ.'3')THEN
          WRITE(LINEA,'(A,I2,A)')'#',NBODY,': '//NOM(I)
          YPGP=(41.-REAL(NBODY))*4.+14.
          CALL PGSCH(0.6)
          CALL PGPTEXT(225.,YPGP,0.,0.,LINEA(1:38))
        END IF
  550 CONTINUE
      CALL PGEND
      GOTO 300
  560 CONTINUE
ccc      CLOSE(UNIT=33)
C                                   ************************************
C                                   FORMATOS, ERRORES Y FIN DEL PROGRAMA
C                                   ************************************
  100 FORMAT(A,$)
  101 FORMAT(A)
  102 FORMAT(17X,'OBJETO',23X,'A.R.(',F6.1,')DEC.')
  103 FORMAT(17X,6('='),22X,4('='),8X,4('='))
  104 FORMAT(79('='))
  105 FORMAT(A,F8.4)
  110 FORMAT(A14,F6.1,A11,F6.1,A41)
  150 FORMAT(A30,',',F8.4,',',F8.4)
  160 FORMAT(6X,I2,4X,A20)
  170 FORMAT(A40,2(4X,F8.4))
  190 FORMAT(A,3(1X,I2.2,A1))
C
  900 CONTINUE
      STOP
  990 WRITE(*,101)'I/O ERROR with file: '//FILNAM
      STOP
      END
