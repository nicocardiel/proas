C------------------------------------------------------------------------------
C                                                                 File: lrunx.f
C
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE LRUNX(LRUN,LMANUAL)
C
C Output: LRUNX,LMANUAL
C
C Determine whether files .running_RUN and .running_MANUAL exist in current
C directory
C
C LOGICAL LRUN -> .TRUE. if file .running_RUN exist (.FALSE. otherwise)
C LOGICAL LMANUAL -> .FALSE. if file .running_MANUAL exist (.FALSE. otherwise)
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE LRUNX(LRUN,LMANUAL)
        IMPLICIT NONE
        LOGICAL LRUN,LMANUAL
        INQUIRE(FILE='.running_RUN',EXIST=LRUN)
        INQUIRE(FILE='.running_MANUAL',EXIST=LMANUAL)
        END
