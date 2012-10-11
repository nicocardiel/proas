C------------------------------------------------------------------------------
C                                                            File: endprogram.f
C
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE ENDPROGRAM(CADENA)
C
C Input: CADENA
C
C Stop the program
C
C CHARACTER*10 CADENA -> if CADENA='endprogram' STOP
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE ENDPROGRAM(CADENA)
        IMPLICIT NONE
        CHARACTER*10 CADENA
        CHARACTER*1 CSTOP
        IF(CADENA(1:10).EQ.'endprogram')THEN
          WRITE(*,100)'Do you want to STOP the program (y/n) [n] ?'
          READ(*,'(A)') CSTOP
          IF(CSTOP.EQ.'y')THEN
            CALL PGEND
            STOP
          END IF
        END IF
100     FORMAT(A,$)
        END
