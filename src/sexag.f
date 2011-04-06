C **********************************************************************
C                                                       SUBROUTINE SEXAG
C                                                       ****************
C
      SUBROUTINE SEXAG(A,B,C,D)
C
C Paso de DD.dddd a DD.MMSS
C
      IMPLICIT NONE
C---> argumentos ficticios: ENTRADA
      REAL A
C---> argumentos ficticios: SALIDA
      REAL B,C,D
C---> variables locales
      REAL AA,BR
C
      AA=ABS(A)
      B=AINT(AA)
      BR=(AA-B)*60.
      C=AINT(BR)
      D=ANINT((BR-AINT(BR))*60)
      IF(D.GE.60.)THEN
        C=C+1.
        D=0.
        IF(C.GE.60.)THEN
          B=B+1.
          C=0.
        END IF
      END IF
C hay que tener cuidado con la siguiente asignacion de signos
C (necesaria por si B=0 o/y C=0)
      IF(A.LT.0.)THEN
        B=-B
        C=-C
        D=-D
      END IF
      END
