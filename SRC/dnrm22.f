*> \brief \b DNRM22
*
*  =========== DOCUMENTATION ===========
*
*  Definition:
*  ===========
*
*       SUBROUTINE DNRM22(N,X,INCX,ALPHA)
* 
*       .. Scalar Arguments ..
*       INTEGER INCX,N
*       DOUBLE PRECISION ALPHA
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION X(*)
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DNRM22 returns the euclidean norm of a vector via the function
*> name, so that
*>
*>    ALPHA := sqrt( x'*x ).
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
*>     Modified on 14-October-1993 to inline the call to DLASSQ.
*>     Sven Hammarling, Nag Ltd.
*>  -- Modified by Jack Poulson on 2015 to return the result in an argument
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DNRM22(N,X,INCX,ALPHA)
*
*  -- Reference BLAS level1 routine (version 3.4.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*  -- Jack Poulson --
*     September 2015
*
*     .. Scalar Arguments ..
      INTEGER INCX,N
      DOUBLE PRECISION ALPHA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*)
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0E+0,ZERO=0.0E+0)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION ABSXI,SCALE,SSQ
      INTEGER IX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC ABS,SQRT
*     ..
      IF (N.LT.1 .OR. INCX.LT.1) THEN
          ALPHA = ZERO
      ELSE IF (N.EQ.1) THEN
          ALPHA = ABS(X(1))
      ELSE
          SCALE = ZERO
          SSQ = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
          DO 10 IX = 1,1 + (N-1)*INCX,INCX
              IF (X(IX).NE.ZERO) THEN
                  ABSXI = ABS(X(IX))
                  IF (SCALE.LT.ABSXI) THEN
                      SSQ = ONE + SSQ* (SCALE/ABSXI)**2
                      SCALE = ABSXI
                  ELSE
                      SSQ = SSQ + (ABSXI/SCALE)**2
                  END IF
              END IF
   10     CONTINUE
          ALPHA = SCALE*SQRT(SSQ)
      END IF
*
      RETURN
*
*     End of DNRM22.
*
      END
