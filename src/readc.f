C------------------------------------------------------------------------------
C                                                                 File: readc.f
C------------------------------------------------------------------------------
Comment
C
C CHARACTER*(*) FUNCTION READC(CDEF,CVAL)
C
C Input: CDEF,CVAL
C Output: READC (function)
C
C Return a character string entered by the user through the keyboard.
C
C CHARACTER*(*) CDEF -> character with default value for READC
C               ('@' if there is no default)
C CHARACTER*(*) CVAL -> character string with valid characters
C               ('@' if all characters are valid)
C
Comment
C------------------------------------------------------------------------------
        CHARACTER*(*) FUNCTION READC(CDEF,CVAL)
        IMPLICIT NONE
        CHARACTER*(*) CDEF,CVAL
C
        INTEGER I,L1,L2
        INTEGER TRUEBEG,TRUELEN
        INTEGER NERR
        CHARACTER*255 CADENA
        LOGICAL LRUN,LMANUAL
C------------------------------------------------------------------------------
        NERR=0
10      IF(CDEF.NE.'@')THEN
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
        READ(*,'(A)',ERR=20) CADENA
        IF(TRUELEN(CADENA).GE.10) CALL ENDPROGRAM(CADENA(1:10))
        IF(CVAL.EQ.'@')THEN
          IF(TRUELEN(CADENA).EQ.0)THEN
            IF(CDEF.EQ.'@')THEN
              GOTO 10
            END IF
            CADENA=CDEF(L1:L2)
          END IF
        ELSE
          IF(TRUELEN(CADENA).EQ.0)THEN
            IF(CDEF.EQ.'@')THEN
              GOTO 10
            END IF
            CADENA=CDEF(L1:L2)
          ELSE
            DO I=1,TRUELEN(CADENA)
              IF(INDEX(CVAL,CADENA(I:I)).EQ.0)THEN
                WRITE(*,101)'ERROR: invalid character(s). Try again.'
                IF(CDEF.EQ.'@') WRITE(*,100)'? '
                NERR=NERR+1
                IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
                GOTO 10
              END IF
            END DO
          END IF
        END IF
        READC=CADENA
        CALL LRUNX(LRUN,LMANUAL)
        IF(LRUN)THEN
          CALL RMBLANK(CADENA,CADENA,L2)
          WRITE(*,101)CADENA(1:L2)
        END IF
        IF(LMANUAL)THEN
          CALL RMBLANK(CADENA,CADENA,L2)
          WRITE(*,101) '\\ttshade{'//CADENA(1:L2)//'}'
        END IF
        RETURN
20      WRITE(*,101)'ERROR: invalid entry. Try again.'
        IF(CDEF.EQ.'@') WRITE(*,100)'? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
