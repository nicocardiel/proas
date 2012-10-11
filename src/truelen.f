C------------------------------------------------------------------------------
C                                                               File: truelen.f
C------------------------------------------------------------------------------
Comment
C
C INTEGER FUNCTION TRUELEN(CADENA)
C
C Input: CADENA
C Output: TRUELEN (function)
C
C Return the position of the last non-blank character in CADENA (ignoring also
C control characters with ASCII value < 32)
C
C CHARACTER*(*) CADENA -> input character string
C
Comment
C------------------------------------------------------------------------------
        INTEGER FUNCTION TRUELEN(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        L=LEN(CADENA)
C
        DO I=L,1,-1
          IF(ICHAR(CADENA(I:I)).GT.32)THEN
            TRUELEN=I
            RETURN
          END IF
        END DO
        TRUELEN=0
        END
