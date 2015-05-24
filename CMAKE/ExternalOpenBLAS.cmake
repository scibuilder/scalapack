#
#  Copyright 2009-2015, Jack Poulson
#  All rights reserved.
#
include(ExternalProject)
include(ElLibraryName)
include(CheckFortranFunctionExists)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(GFORTRAN_PATHS /usr/lib
                     /usr/lib64
                     /usr/local/lib
                     /usr/local/lib64
                     /usr/lib/openmpi/lib
                     /usr/lib/gcc/x86_64-linux-gnu/4.8
                     /usr/lib/gcc/x86_64-linux-gnu/4.9
                     /lib/x86_64-linux-gnu
                     /usr/lib/x86_64-linux-gnu
                     /usr/lib/openblas-base
                     /usr/lib64/openblas-base
                     /usr/local/lib/gcc/4.8
                     /usr/local/lib/gcc/4.9)
  if(NOT GFORTRAN_LIB)
    find_library(GFORTRAN_LIB NAMES gfortran PATHS ${GFORTRAN_PATHS})
    if(NOT GFORTRAN_LIB)
      if(APPLE)
        message(FATAL_ERROR "Could not find gfortran library; please consider setting the GFORTRAN_LIB variable. If you installed gfortran via homebrew, please see the issue filed at https://github.com/Homebrew/homebrew/issues/8539")
      else()
        message(FATAL_ERROR "Could not find gfortran library; please consider setting the GFORTRAN_LIB variable.")
      endif()
    endif()
  endif()
  if(NOT CMAKE_THREAD_LIBS_INIT)
    set(CMAKE_THREAD_PREFER_PTHREAD ON)
    find_package(Threads)
    if(NOT CMAKE_USE_PTHREADS_INIT)
      message(FATAL_ERROR "Could not find a pthreads library")
    endif()
  endif()
  if(NOT STD_MATH_LIB)
    find_library(STD_MATH_LIB m)
    if(NOT STD_MATH_LIB)
      message(FATAL_ERROR "Could not find standard math library")
    endif()
  endif()
  set(GNU_ADDONS ${GFORTRAN_LIB} ${CMAKE_THREAD_LIBS_INIT} ${STD_MATH_LIB})
else()
  set(GNU_ADDONS)
endif()

if(NOT BUILD_OPENBLAS)
  find_library(OpenBLAS NAMES openblas PATHS ${MATH_PATHS})
  if(OpenBLAS)
    set(CMAKE_REQUIRED_LIBRARIES ${OpenBLAS} ${GNU_ADDONS})
    check_fortran_function_exists(dgemm  HAVE_DGEMM_OPENBLAS)
    check_fortran_function_exists(dsytrd HAVE_DSYTRD_OPENBLAS)
    if(HAVE_DGEMM_OPENBLAS)
      set(HAVE_OPENBLAS_BLAS TRUE)
    else()
      message(WARNING "OpenBLAS was found as ${OpenBLAS}, but BLAS support was not detected")
    endif()
    if(HAVE_DSYTRD_OPENBLAS)
      set(HAVE_OPENBLAS_LAPACK TRUE)
    else()
      message(WARNING "OpenBLAS was found as ${OpenBLAS}, but LAPACK support was not detected")
    endif()
  endif()
endif()

if(HAVE_OPENBLAS_BLAS AND HAVE_OPENBLAS_LAPACK)
  set(OPENBLAS_LIBS ${OpenBLAS} ${GNU_ADDONS})
  set(HAVE_OPENBLAS TRUE)
  set(BUILT_OPENBLAS FALSE) 
  message(STATUS "Using OpenBLAS+LAPACK found at ${OpenBLAS}")
elseif(NOT MSVC)
  if(NOT DEFINED OPENBLAS_URL)
    set(OPENBLAS_URL https://github.com/xianyi/OpenBLAS.git)
  endif()
  message(STATUS "Will pull OpenBLAS from ${OPENBLAS_URL}")

  set(OPENBLAS_SOURCE_DIR ${PROJECT_BINARY_DIR}/download/OpenBLAS/source)
  set(OPENBLAS_BINARY_DIR ${PROJECT_BINARY_DIR}/download/OpenBLAS/build)

  if(APPLE)
    if(NOT OPENBLAS_ARCH_COMMAND)
      # This is a hack but is a good default for modern Mac's
      set(OPENBLAS_ARCH_COMMAND TARGET=SANDYBRIDGE)
    endif()
  else()
    if(NOT OPENBLAS_ARCH_COMMAND)
      set(OPENBLAS_ARCH_COMMAND)
    endif()
  endif()

  ExternalProject_Add(project_openblas
    PREFIX ${CMAKE_INSTALL_PREFIX}
    GIT_REPOSITORY ${OPENBLAS_URL}
    GIT_TAG "v0.2.14"
    STAMP_DIR ${OPENBLAS_BINARY_DIR}/stamp
    BUILD_IN_SOURCE 1
    SOURCE_DIR ${OPENBLAS_SOURCE_DIR}
    TMP_DIR    ${OPENBLAS_BINARY_DIR}/tmp
    INSTALL_DIR ${CMAKE_INSTALL_PREFIX}
    CONFIGURE_COMMAND ""
    UPDATE_COMMAND "" 
    BUILD_COMMAND ${CMAKE_MAKE_PROGRAM} CC=${CMAKE_C_COMPILER} FC=${CMAKE_Fortran_COMPILER} ${OPENBLAS_ARCH_COMMAND} libs netlib shared
    INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install PREFIX=<INSTALL_DIR>
  )

  # Extract the installation directory
  ExternalProject_Get_Property(project_openblas install_dir)

  # Add a target for libopenblas (either shared or static)
  if(BUILD_SHARED_LIBS)
    add_library(libopenblas SHARED IMPORTED)
  else()
    add_library(libopenblas STATIC IMPORTED)
  endif()
  El_library_name(openblas_name openblas)
  set(OPENBLAS_LIB ${install_dir}/lib/${openblas_name})
  set_property(TARGET libopenblas PROPERTY IMPORTED_LOCATION ${OPENBLAS_LIB})

  set(OPENBLAS_LIBS ${OPENBLAS_LIB} ${GNU_ADDONS})
  set(HAVE_OPENBLAS TRUE)
  set(BUILT_OPENBLAS TRUE)
else()
  set(HAVE_OPENBLAS FALSE)
  set(BUILT_OPENBLAS FALSE)
endif()
