*> \brief \b DDOT2
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at 
*            http://www.netlib.org/lapack/explore-html/ 
*
*  Definition:
*  ===========
*
*       SUBROUTINE DDOT2(N,SX,INCX,SY,INCY,ALPHA)
* 
*       .. Scalar Arguments ..
*       INTEGER INCX,INCY,N
*       DOUBLE PRECISION ALPHA
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION SX(*),SY(*)
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    DDOT2 forms the dot product of two vectors.
*>    uses unrolled loops for increments equal to one.
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
*>     jack dongarra, linpack, 3/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DDOT2(N,SX,INCX,SY,INCY,ALPHA)
*
*  -- Reference BLAS level1 routine (version 3.4.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2011
*
*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
      DOUBLE PRECISION ALPHA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION SX(*),SY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,IX,IY,M,MP1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MOD
*     ..
      ALPHA = 0.0e0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
*
*        clean-up loop
*
         M = MOD(N,5)
         IF (M.NE.0) THEN
            DO I = 1,M
               ALPHA = ALPHA + SX(I)*SY(I)
            END DO
            IF (N.LT.5) THEN
            RETURN
            END IF
         END IF
         MP1 = M + 1
         DO I = MP1,N,5
          ALPHA = ALPHA + SX(I)*SY(I) + SX(I+1)*SY(I+1) +
     $            SX(I+2)*SY(I+2) + SX(I+3)*SY(I+3) + SX(I+4)*SY(I+4)
         END DO
      ELSE
*
*        code for unequal increments or equal increments
*          not equal to 1
*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            ALPHA = ALPHA + SX(IX)*SY(IY)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
      END
