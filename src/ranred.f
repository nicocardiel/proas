C------------------------------------------------------------------------------
Comment
C
C REAL FUNCTION RANRED(NSEED)
C
C Input: NSEED
C Output: RANRED (function)
C
C Return a random number in the range [0,1) using the intrinsic fortran
C function RAND(). If NSEED<0 a previous call to SRAND(TIME()) is also 
C performed.
C
C INTEGER NSEED -> NSEED=0: RANRED returns the next random number in the
C                           sequence.
C                  NSEED<0: RANRED performs a previous call to the
C                           intrinsic fortran function SRAND(TIME()), and
C                           generates a random number in the new sequence.
C                           In this case NSEED returns 0.
C                  NSEED>0: RANRED performs a previous call to the
C                           intrinsic fortran function SRAND(NSEED), and
C                           generates a random number in the new sequence.
C                           In this case NSEED returns 0.
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION RANRED(NSEED)
        IMPLICIT NONE
        INTEGER NSEED
C------------------------------------------------------------------------------
        IF(NSEED.EQ.0)THEN
          RANRED=RAND()
          RETURN
        ELSEIF(NSEED.LT.0)THEN
          CALL SRAND(TIME())
        ELSEIF(NSEED.GT.0)THEN
          CALL SRAND(NSEED)
        END IF
        NSEED=0
        RANRED=RAND()
C
        END
