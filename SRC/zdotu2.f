*> \brief \b ZDOTU2
*
*  =========== DOCUMENTATION ===========
*
*  Definition:
*  ===========
*
*       SUBROUTINE ZDOTU2(N,ZX,INCX,ZY,INCY,ALPHA)
* 
*       .. Scalar Arguments ..
*       INTEGER INCX,INCY,N
*       COMPLEX*16 ALPHA
*       ..
*       .. Array Arguments ..
*       COMPLEX*16 ZX(*),ZY(*)
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    ZDOTU2 forms the dot product of two vectors.
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
*> \ingroup complex16_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, 3/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE ZDOTU2(N,ZX,INCX,ZY,INCY,ALPHA)
*
*  -- Reference BLAS level1 routine (version 3.4.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*  -- Jack Poulson --
*     September 2015
*
*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
      COMPLEX*16 ALPHA
*     ..
*     .. Array Arguments ..
      COMPLEX*16 ZX(*),ZY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,IX,IY
*     ..
      ALPHA = (0.0d0,0.0d0)
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
         DO I = 1,N
            ALPHA = ALPHA + ZX(I)*ZY(I)
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
            ALPHA = ALPHA + ZX(IX)*ZY(IY)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
      END
