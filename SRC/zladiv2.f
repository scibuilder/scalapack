*> \brief \b ZLADIV2 performs complex division in real arithmetic, avoiding unnecessary overflow. Note that this version, created by Jack Poulson, avoids returning a complex value since this frequently causes problems when interfacing different Fortran libraries (e.g., it caused pzlahqr to return many of the eigenvalues shifted left by one due to zladiv returning 1 instead of 0 within zlanv2).
*
*  Definition:
*  ===========
*
*       SUBROUTINE ZLADIV2( X, Y, Z )
* 
*       .. Scalar Arguments ..
*       COMPLEX*16         X, Y, Z
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> ZLADIV2 outputs Z = X / Y, where X and Y are complex.  
*> The computation of X / Y
*> will not overflow on an intermediary step unless the results
*> overflows.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] X
*> \verbatim
*>          X is COMPLEX*16
*> \endverbatim
*>
*> \param[in] Y
*> \verbatim
*>          Y is COMPLEX*16
*> \endverbatim
*>
*> \param[out] Z
*> \verbatim
*>          Z is COMPLEX*16
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
*> \ingroup complex16OTHERauxiliary
*
*  =====================================================================
      SUBROUTINE ZLADIV2( X, Y, Z )
*
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     September 2012
*
*  -- Modified by Jack Poulson on September 2015
*
*     .. Scalar Arguments ..
      COMPLEX*16         X, Y, Z
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      DOUBLE PRECISION   ZI, ZR
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLADIV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, DIMAG
*     ..
*     .. Executable Statements ..
*
      CALL DLADIV( DBLE( X ), DIMAG( X ), DBLE( Y ), DIMAG( Y ), ZR,
     $             ZI )
      Z = DCMPLX( ZR, ZI )
*
      RETURN
*
*     End of ZLADIV2
*
      END
