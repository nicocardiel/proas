C **********************************************************************
C                                                     SUBROUTINE DECIMAL
C                                                     ******************
C
      SUBROUTINE DECIMAL(A,B)
C
C Paso de DD.MMSS a DD.dddd
C Utilizamos variables INTEGER*4 para evitar los errores de redondeo.
C Es posible hacer una llamada a la subrutina en la que los argumentos
C verdaderos correspondientes a las argumentos ficticios A y B
C coincidan.
C
      IMPLICIT NONE
C---> argumentos ficticios: ENTRADA
      REAL A
C---> argumentos ficticios: SALIDA
      REAL B
C---> variables locales
      INTEGER*4 AA,A1,A2,A3
      CHARACTER*1 SIGNO
C
      SIGNO='+'
      IF(A.LT.0)SIGNO='-'
      AA=ABS(NINT(A*10000))
      A1=AA/10000
      A2=AA-A1*10000
      A2=A2/100
      A3=AA-A1*10000-A2*100
      B=REAL(A1)+REAL(A2)/60.+REAL(A3)/3600.
      IF(SIGNO.EQ.'-')B=-B
      END
