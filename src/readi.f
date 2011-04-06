C------------------------------------------------------------------------------
C                                                                 File: readi.f
C------------------------------------------------------------------------------
Comment
C
C INTEGER FUNCTION READI(CDEF)
C
C Input: CDEF
C Output: READI (function)
C
C Return an integer number entered by the user through the keyboard.
C
C CHARACTER*(*) CDEF -> character string with default value for READI
C               ('@' if there is no default)
C
Comment
C------------------------------------------------------------------------------
	INTEGER FUNCTION READI(CDEF)
	IMPLICIT NONE
	CHARACTER*(*) CDEF
C
	INTEGER I,L1,L2
	INTEGER N
	INTEGER NERR
	INTEGER TRUEBEG,TRUELEN
	CHARACTER*1 C
	CHARACTER*255 CDUMMY
	CHARACTER*255 CADENA
	LOGICAL LRUN,LMANUAL
C------------------------------------------------------------------------------
	NERR=0
10	IF(CDEF.NE.'@')THEN
	  L1=TRUEBEG(CDEF)
	  IF(L1.NE.0)THEN
	    L2=TRUELEN(CDEF)
	    WRITE(*,100)'['
	    WRITE(*,100)CDEF(L1:L2)
	    WRITE(*,100)'] ? '
	  END IF
	ELSE
	  WRITE(*,100)'? '
	END IF
	READ(*,'(A)',ERR=20)CADENA
        IF(TRUELEN(CADENA).GE.10) CALL ENDPROGRAM(CADENA(1:10))
	IF(TRUELEN(CADENA).EQ.0)THEN
	  IF(CDEF.EQ.'@')THEN
	    GOTO 10
	  END IF
	  CADENA=CDEF
	END IF
	DO I=1,TRUELEN(CADENA)
	  C=CADENA(I:I)
	  IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
	    GOTO 20
	  END IF
	END DO
	READ(CADENA,*,ERR=20) N
	READI=N
	CALL LRUNX(LRUN,LMANUAL)
	IF(LRUN)THEN
	  WRITE(CDUMMY,*) N
	  CALL RMBLANK(CDUMMY,CDUMMY,L2)
	  WRITE(*,101) CDUMMY(1:L2)
	END IF
	IF(LMANUAL)THEN
	  WRITE(CDUMMY,*) N
	  CALL RMBLANK(CDUMMY,CDUMMY,L2)
          WRITE(*,101) '\\ttshade{'//CDUMMY(1:L2)//'}'
	END IF
	RETURN
20	WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
	IF(CDEF.EQ.'@') WRITE(*,100)'? '
	NERR=NERR+1
	IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
	GOTO 10
100	FORMAT(A,$)
101	FORMAT(A)
	END
