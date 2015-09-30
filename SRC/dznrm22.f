*> \brief \b DZNRM22
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at 
*            http://www.netlib.org/lapack/explore-html/ 
*
*  Definition:
*  ===========
*
*       SUBROUTINE DZNRM22(N,X,INCX,ALPHA)
* 
*       .. Scalar Arguments ..
*       INTEGER INCX,N
*       DOUBLE PRECISION ALPHA
*       ..
*       .. Array Arguments ..
*       COMPLEX*16 X(*)
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DZNRM22 returns the euclidean norm of a vector via the function
*> name, so that
*>
*>    ALPHA := sqrt( x**H*x )
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee 
*> \author Univ. of California Berkeley 
*> \author Univ. of Colorado Denver 
*> \author NAG Ltd. 
*> \author Jack Poulson
*
*> \date September 2015
*
*> \ingroup single_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  -- This version written on 25-October-1982.
*>     Modified on 14-October-1993 to inline the call to ZLASSQ.
*>     Sven Hammarling, Nag Ltd.
*>  -- Modified by Jack Poulson in September 2015 to return result in argument
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DZNRM22(N,X,INCX,ALPHA)
*
*  -- Reference BLAS level1 routine (version 3.4.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2011
*
*     .. Scalar Arguments ..
      INTEGER INCX,N
      DOUBLE PRECISION ALPHA
*     ..
*     .. Array Arguments ..
      COMPLEX*16 X(*)
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0E+0,ZERO=0.0E+0)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION SCALE,SSQ,TEMP
      INTEGER IX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC ABS,DIMAG,DBLE,SQRT
*     ..
      IF (N.LT.1 .OR. INCX.LT.1) THEN
          ALPHA = ZERO
      ELSE
          SCALE = ZERO
          SSQ = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL ZLASSQ( N, X, INCX, SCALE, SSQ )
*
          DO 10 IX = 1,1 + (N-1)*INCX,INCX
              IF (DBLE(X(IX)).NE.ZERO) THEN
                  TEMP = ABS(DBLE(X(IX)))
                  IF (SCALE.LT.TEMP) THEN
                      SSQ = ONE + SSQ* (SCALE/TEMP)**2
                      SCALE = TEMP
                  ELSE
                      SSQ = SSQ + (TEMP/SCALE)**2
                  END IF
              END IF
              IF (DIMAG(X(IX)).NE.ZERO) THEN
                  TEMP = ABS(DIMAG(X(IX)))
                  IF (SCALE.LT.TEMP) THEN
                      SSQ = ONE + SSQ* (SCALE/TEMP)**2
                      SCALE = TEMP
                  ELSE
                      SSQ = SSQ + (TEMP/SCALE)**2
                  END IF
              END IF
   10     CONTINUE
          ALPHA = SCALE*SQRT(SSQ)
      END IF
*
      RETURN
*
*     End of DZNRM22.
*
      END
