/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 1.3.36
 * 
 * This file is not intended to be easily readable and contains a number of 
 * coding conventions designed to improve portability and efficiency. Do not make
 * changes to this file unless you know what you are doing--modify the SWIG 
 * interface file instead. 
 * ----------------------------------------------------------------------------- */

#ifdef __cplusplus
template < typename T > class SwigValueWrapper
{
  T              *tt;
public:
SwigValueWrapper ():tt (0)
  {
  }
  SwigValueWrapper (const SwigValueWrapper < T > &rhs):tt (new T (*rhs.tt))
  {
  }
  SwigValueWrapper (const T & t):tt (new T (t))
  {
  }
  ~SwigValueWrapper ()
  {
    delete          tt;
  }
  SwigValueWrapper & operator= (const T & t)
  {
    delete          tt;
    tt = new T (t);
    return *this;
  }
  operator        T & () const
  {
    return *tt;
  }
  T              *operator& ()
  {
    return tt;
  }
private:
  SwigValueWrapper & operator= (const SwigValueWrapper < T > &rhs);
};

template < typename T > T SwigValueInit ()
{
  return T ();
}
#endif

/* -----------------------------------------------------------------------------
 *  This section contains generic SWIG labels for method/variable
 *  declarations/attributes, and other compiler dependent labels.
 * ----------------------------------------------------------------------------- */

/* template workaround for compilers that cannot correctly implement the C++ standard */
#ifndef SWIGTEMPLATEDISAMBIGUATOR
#if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x560)
#define SWIGTEMPLATEDISAMBIGUATOR template
#elif defined(__HP_aCC)
/* Needed even with `aCC -AA' when `aCC -V' reports HP ANSI C++ B3910B A.03.55 */
/* If we find a maximum version that requires this, the test would be __HP_aCC <= 35500 for A.03.55 */
#define SWIGTEMPLATEDISAMBIGUATOR template
#else
#define SWIGTEMPLATEDISAMBIGUATOR
#endif
#endif

/* inline attribute */
#ifndef SWIGINLINE
#if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#define SWIGINLINE inline
#else
#define SWIGINLINE
#endif
#endif

/* attribute recognised by some compilers to avoid 'unused' warnings */
#ifndef SWIGUNUSED
#if defined(__GNUC__)
#if !(defined(__cplusplus)) || (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4))
#define SWIGUNUSED __attribute__ ((__unused__))
#else
#define SWIGUNUSED
#endif
#elif defined(__ICC)
#define SWIGUNUSED __attribute__ ((__unused__))
#else
#define SWIGUNUSED
#endif
#endif

#ifndef SWIGUNUSEDPARM
#ifdef __cplusplus
#define SWIGUNUSEDPARM(p)
#else
#define SWIGUNUSEDPARM(p) p SWIGUNUSED
#endif
#endif

/* internal SWIG method */
#ifndef SWIGINTERN
#define SWIGINTERN static SWIGUNUSED
#endif

/* internal inline SWIG method */
#ifndef SWIGINTERNINLINE
#define SWIGINTERNINLINE SWIGINTERN SWIGINLINE
#endif

/* exporting methods */
#if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#ifndef GCC_HASCLASSVISIBILITY
#define GCC_HASCLASSVISIBILITY
#endif
#endif

#ifndef SWIGEXPORT
#if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#if defined(STATIC_LINKED)
#define SWIGEXPORT
#else
#define SWIGEXPORT __declspec(dllexport)
#endif
#else
#if defined(__GNUC__) && defined(GCC_HASCLASSVISIBILITY)
#define SWIGEXPORT __attribute__ ((visibility("default")))
#else
#define SWIGEXPORT
#endif
#endif
#endif

/* calling conventions for Windows */
#ifndef SWIGSTDCALL
#if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#define SWIGSTDCALL __stdcall
#else
#define SWIGSTDCALL
#endif
#endif

/* Deal with Microsoft's attempt at deprecating C standard runtime functions */
#if !defined(SWIG_NO_CRT_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_CRT_SECURE_NO_DEPRECATE)
#define _CRT_SECURE_NO_DEPRECATE
#endif

/* Deal with Microsoft's attempt at deprecating methods in the standard C++ library */
#if !defined(SWIG_NO_SCL_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_SCL_SECURE_NO_DEPRECATE)
#define _SCL_SECURE_NO_DEPRECATE
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#if defined(_WIN32) || defined(__CYGWIN32__)
#define DllExport   __declspec( dllexport )
#define SWIGSTDCALL __stdcall
#else
#define DllExport
#define SWIGSTDCALL
#endif

#ifdef __cplusplus
#include <new>
#endif

/* Callback for returning strings to Ada without leaking memory */

typedef char   *(SWIGSTDCALL * SWIG_AdaStringHelperCallback) (const char *);
static SWIG_AdaStringHelperCallback SWIG_ada_string_callback = NULL;

/* probably obsolete ...
#ifdef __cplusplus
extern "C" 
#endif
DllExport void SWIGSTDCALL SWIGRegisterStringCallback_CORE_MODULE(SWIG_AdaStringHelperCallback callback) {
  SWIG_ada_string_callback = callback;
}
*/

/* Contract support */
/*
#define SWIG_contract_assert(nullreturn, expr, msg) if (!(expr)) {SWIG_AdaThrowException(SWIG_AdaArgumentOutOfRangeException, msg); return nullreturn; } else
*/

#define protected public
#define private   public

extern          "C"
{
#include "../c/c_math.h"
}

#undef protected
#undef private
#ifdef __cplusplus
extern          "C"
{
#endif
  DllExport void *SWIGSTDCALL Ada_new_Vector_2__SWIG_0 ()
  {
    void           *jresult;
    Vector_2       *result = 0;

                    result = (Vector_2 *) new Vector_2 ();
                    jresult = (void *) result;

                    return jresult;

  }

  DllExport void *SWIGSTDCALL Ada_new_Vector_2__SWIG_1 (float jarg1,
							float jarg2)
  {
    void           *jresult;
    Real            arg1;
    Real            arg2;
    Vector_2       *result = 0;

    arg1 = (Real) jarg1;

    arg2 = (Real) jarg2;

    result = (Vector_2 *) new Vector_2 (arg1, arg2);
    jresult = (void *) result;

    return jresult;

  }

  DllExport void SWIGSTDCALL Ada_delete_Vector_2 (void *jarg1)
  {
    Vector_2       *arg1 = (Vector_2 *) 0;

    arg1 = (Vector_2 *) jarg1;

    delete          arg1;

  }

  DllExport void *SWIGSTDCALL Ada_new_Vector_3__SWIG_0 ()
  {
    void           *jresult;
    Vector_3       *result = 0;

    result = (Vector_3 *) new Vector_3 ();
    jresult = (void *) result;

    return jresult;

  }

  DllExport void *SWIGSTDCALL Ada_new_Vector_3__SWIG_1 (float jarg1,
							float jarg2,
							float jarg3)
  {
    void           *jresult;
    Real            arg1;
    Real            arg2;
    Real            arg3;
    Vector_3       *result = 0;

    arg1 = (Real) jarg1;

    arg2 = (Real) jarg2;

    arg3 = (Real) jarg3;

    result = (Vector_3 *) new Vector_3 (arg1, arg2, arg3);
    jresult = (void *) result;

    return jresult;

  }

  DllExport void SWIGSTDCALL Ada_delete_Vector_3 (void *jarg1)
  {
    Vector_3       *arg1 = (Vector_3 *) 0;

    arg1 = (Vector_3 *) jarg1;

    delete          arg1;

  }

  DllExport void *SWIGSTDCALL Ada_new_Triangle__SWIG_0 ()
  {
    void           *jresult;
    Triangle       *result = 0;

    result = (Triangle *) new Triangle ();
    jresult = (void *) result;

    return jresult;

  }

  DllExport void *SWIGSTDCALL Ada_new_Triangle__SWIG_1 (float jarg1,
							float jarg2,
							float jarg3)
  {
    void           *jresult;
    Real            arg1;
    Real            arg2;
    Real            arg3;
    Triangle       *result = 0;

    arg1 = (Real) jarg1;

    arg2 = (Real) jarg2;

    arg3 = (Real) jarg3;

    result = (Triangle *) new Triangle (arg1, arg2, arg3);
    jresult = (void *) result;

    return jresult;

  }

  DllExport void SWIGSTDCALL Ada_delete_Triangle (void *jarg1)
  {
    Triangle       *arg1 = (Triangle *) 0;

    arg1 = (Triangle *) jarg1;

    delete          arg1;

  }

  DllExport void *SWIGSTDCALL Ada_new_Matrix_3x3__SWIG_0 ()
  {
    void           *jresult;
    Matrix_3x3     *result = 0;

    result = (Matrix_3x3 *) new Matrix_3x3 ();
    jresult = (void *) result;

    return jresult;

  }

  DllExport void *SWIGSTDCALL Ada_new_Matrix_3x3__SWIG_1 (float *jarg1)
  {
    void           *jresult;
    Real           *arg1 = (Real *) 0;
    Matrix_3x3     *result = 0;

    arg1 = (Real *) jarg1;

    result = (Matrix_3x3 *) new Matrix_3x3 (arg1);
    jresult = (void *) result;

    return jresult;

  }

  DllExport void *SWIGSTDCALL Ada_new_Matrix_3x3__SWIG_2 (float jarg1,
							  float jarg2,
							  float jarg3,
							  float jarg4,
							  float jarg5,
							  float jarg6,
							  float jarg7,
							  float jarg8,
							  float jarg9)
  {
    void           *jresult;
    Real            arg1;
    Real            arg2;
    Real            arg3;
    Real            arg4;
    Real            arg5;
    Real            arg6;
    Real            arg7;
    Real            arg8;
    Real            arg9;
    Matrix_3x3     *result = 0;

    arg1 = (Real) jarg1;

    arg2 = (Real) jarg2;

    arg3 = (Real) jarg3;

    arg4 = (Real) jarg4;

    arg5 = (Real) jarg5;

    arg6 = (Real) jarg6;

    arg7 = (Real) jarg7;

    arg8 = (Real) jarg8;

    arg9 = (Real) jarg9;

    result =
      (Matrix_3x3 *) new Matrix_3x3 (arg1, arg2, arg3, arg4, arg5, arg6, arg7,
				     arg8, arg9);
    jresult = (void *) result;

    return jresult;

  }

  DllExport void SWIGSTDCALL Ada_delete_Matrix_3x3 (void *jarg1)
  {
    Matrix_3x3     *arg1 = (Matrix_3x3 *) 0;

    arg1 = (Matrix_3x3 *) jarg1;

    delete          arg1;

  }

  DllExport void *SWIGSTDCALL Ada_new_Matrix_4x4__SWIG_0 ()
  {
    void           *jresult;
    Matrix_4x4     *result = 0;

    result = (Matrix_4x4 *) new Matrix_4x4 ();
    jresult = (void *) result;

    return jresult;

  }

  DllExport void *SWIGSTDCALL Ada_new_Matrix_4x4__SWIG_1 (float *jarg1)
  {
    void           *jresult;
    Real           *arg1 = (Real *) 0;
    Matrix_4x4     *result = 0;

    arg1 = (Real *) jarg1;

    result = (Matrix_4x4 *) new Matrix_4x4 (arg1);
    jresult = (void *) result;

    return jresult;

  }

  DllExport void SWIGSTDCALL Ada_delete_Matrix_4x4 (void *jarg1)
  {
    Matrix_4x4     *arg1 = (Matrix_4x4 *) 0;

    arg1 = (Matrix_4x4 *) jarg1;

    delete          arg1;

  }

#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
extern          "C"
{
#endif
  extern Vector_2 gnat_new_Vector_2__SWIG_0 ()
  {
    return Vector_2 ();
  }

  extern Vector_2 gnat_new_Vector_2__SWIG_1 (Real x, Real y)
  {
    return Vector_2 (x, y);
  }

  extern Vector_3 gnat_new_Vector_3__SWIG_0 ()
  {
    return Vector_3 ();
  }

  extern Vector_3 gnat_new_Vector_3__SWIG_1 (Real x, Real y, Real z)
  {
    return Vector_3 (x, y, z);
  }

  extern Triangle gnat_new_Triangle__SWIG_0 ()
  {
    return Triangle ();
  }

  extern Triangle gnat_new_Triangle__SWIG_1 (Real a, Real b, Real c)
  {
    return Triangle (a, b, c);
  }

  extern Matrix_3x3 gnat_new_Matrix_3x3__SWIG_0 ()
  {
    return Matrix_3x3 ();
  }

  extern Matrix_3x3 gnat_new_Matrix_3x3__SWIG_1 (Real * First)
  {
    return Matrix_3x3 (First);
  }

  extern Matrix_3x3 gnat_new_Matrix_3x3__SWIG_2 (Real m00, Real m01, Real m02,
						 Real m10, Real m11, Real m12,
						 Real m20, Real m21, Real m22)
  {
    return Matrix_3x3 (m00, m01, m02, m10, m11, m12, m20, m21, m22);
  }

  extern Matrix_4x4 gnat_new_Matrix_4x4__SWIG_0 ()
  {
    return Matrix_4x4 ();
  }

  extern Matrix_4x4 gnat_new_Matrix_4x4__SWIG_1 (Real * First)
  {
    return Matrix_4x4 (First);
  }

#ifdef __cplusplus
}
#endif
