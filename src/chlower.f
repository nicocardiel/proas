C
C     Copyright 1997-2009 Nicolás Cardiel
C
C     This file is part of Proas
C
C     Proas is free software: you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation, either version 2 of the License, or
C     (at your option) any later version.
C
C     Proas is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with Proas.  If not, see <http://www.gnu.org/licenses/>.
C
C
C     $Id: chlower.f 558 2007-10-10 16:10:49Z spr $
C
C     SUBROUTINE CHLOWER(CADENA)
C
C     Input: CADENA
C     Output: CADENA
C
C     Upper case characters in CADENA are transformed to lower case
C
C     CHARACTER*(*) CADENA -> character string to be transformed
C
C------------------------------------------------------------------------------
      SUBROUTINE CHLOWER(CADENA)
      IMPLICIT NONE
      CHARACTER*(*) CADENA
C
      INTEGER I,N
C------------------------------------------------------------------------------
      DO I=1,LEN(CADENA)
        N=ICHAR(CADENA(I:I))
        IF((N.GE.65).AND.(N.LE.90)) CADENA(I:I)=CHAR(N+32)
      END DO
      END
