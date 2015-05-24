cmake_minimum_required(VERSION 2.8)
project(SCALAPACK C Fortran)

# Add the CMake directory for custon CMake modules
set(CMAKE_MODULE_PATH "${SCALAPACK_SOURCE_DIR}/CMAKE" ${CMAKE_MODULE_PATH})

option(BUILD_SHARED_LIBS "Build shared libraries" ON)

# Configure the warning and code coverage suppression file
configure_file( 
  "${SCALAPACK_SOURCE_DIR}/CMAKE/CTestCustom.cmake.in"
  "${SCALAPACK_BINARY_DIR}/CTestCustom.cmake"
  COPYONLY
)

if(UNIX)
  if("${CMAKE_Fortran_COMPILER}" MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fltconsistency -fp_port")
  endif()
endif()

#
#   MPI
#
set(CMAKE_PREFIX_PATH "${MPI_BASE_DIR};${CMAKE_PREFIX_PATH}")

find_package(MPI)
if(NOT MPI_C_FOUND)
  message(FATAL_ERROR "MPI C compiler was not found and is required")
endif()
if(NOT MPI_Fortran_FOUND)
  message(FATAL_ERROR "MPI Fortran compiler was not found and is required")
endif()
include_directories(${MPI_C_INCLUDE_PATH})
include_directories(${MPI_Fortran_INCLUDE_PATH})
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${MPI_C_COMPILE_FLAGS}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}")

macro(SCALAPACK_install_library lib)
  install(TARGETS ${lib} EXPORT scalapack-targets
    ARCHIVE DESTINATION lib${LIB_SUFFIX}
    LIBRARY DESTINATION lib${LIB_SUFFIX}
    RUNTIME DESTINATION Testing)
endmacro()

# --------------------------------------------------
# Testing
SET(DART_TESTING_TIMEOUT 600)

enable_testing()
include(CTest)
enable_testing()
# --------------------------------------------------

# Organize output files.  On Windows this also keeps .dll files next
# to the .exe files that need them, making tests easy to run.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${SCALAPACK_BINARY_DIR}/TESTING)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${SCALAPACK_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${SCALAPACK_BINARY_DIR}/lib)

# --------------------------------------------------
# Check for any necessary platform specific compiler flags
include(CheckBLACSCompilerFlags)
CheckBLACSCompilerFlags()

set(prefix ${CMAKE_INSTALL_PREFIX})
set(libdir ${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX})
set(PKG_CONFIG_DIR ${libdir}/pkgconfig)

# --------------------------------------------------
# BLACS Internal variables
#
#   Fortran Mangling, MPI Tests and BLACS settings
#
include(FortranMangling)
COMPILE(install_COMPILED)

FORTRAN_MANGLING(CDEFS)
message(STATUS "=========")

# --------------------------------------------------
# Compiler Flags
add_definitions("-D${CDEFS}")

# --------------------------------------------------
# ScaLAPACK needs BLAS and LAPACK

option(USE_OPTIMIZED_LAPACK_BLAS "Whether or not to search for optimized LAPACK and BLAS libraries on your machine (if not found, Reference LAPACK and BLAS will be downloaded and installed)" ON)

message(STATUS "CHECKING BLAS AND LAPACK LIBRARIES")

if(LAPACK_LIBRARIES)
  include(CheckFortranFunctionExists)
  message(STATUS "--> LAPACK supplied by user is ${LAPACK_LIBRARIES}.")
  set(CMAKE_REQUIRED_LIBRARIES ${LAPACK_LIBRARIES})
  check_fortran_function_exists("dgesv" LAPACK_FOUND)
  unset(CMAKE_REQUIRED_LIBRARIES)
  if(LAPACK_FOUND)
    message(STATUS 
      "--> LAPACK supplied by user is WORKING, will use ${LAPACK_LIBRARIES}.")
  else()
    message(STATUS 
      "--> LAPACK supplied by user is NOT working.")
  endif()
endif()

if(BUILD_BLIS_LAPACK)
  set(PREFER_BLIS_LAPACK TRUE)
  set(DISABLE_OPENBLAS TRUE)
elseif(BUILD_OPENBLAS)
  set(PREFER_OPENBLAS TRUE)
  set(DISABLE_BLIS_LAPACK TRUE)
endif()

if(APPLE)
  if(PREFER_OPENBLAS AND NOT LAPACK_FOUND)
    include(ExternalOpenBLAS)
    if(HAVE_OPENBLAS)
      set(LAPACK_FOUND TRUE)
      set(LAPACK_LIBRARIES ${OPENBLAS_LIBS})
    elseif(BUILD_OPENBLAS)
      message(FATAL_ERROR "Demand that OpenBLAS be built could not be met")
    endif()
  endif()
  if(PREFER_BLIS_LAPACK AND NOT LAPACK_FOUND)
    include(ExternalBLISLAPACK)
    if(HAVE_BLIS_LAPACK)
      set(LAPACK_FOUND TRUE)
      set(LAPACK_LIBRARIES ${BLIS_LAPACK_LIBS})
    elseif(BUILD_BLIS_LAPACK)
      set(FATAL_ERROR "Demand that BLIS+LAPACK be built could not be met")
    endif()
  endif()
  if(NOT LAPACK_FOUND)
    set(CMAKE_REQUIRED_LIBRARIES "-framework vecLib")
    check_fortran_function_exists(dpotrf HAVE_DPOTRF_VECLIB)
    set(CMAKE_REQUIRED_LIBRARIES "-framework Accelerate")
    check_fortran_function_exists(dpotrf HAVE_DPOTRF_ACCELERATE)
    set(CMAKE_REQUIRED_LIBRARIES)
    if(HAVE_DPOTRF_VECLIB)
      set(LAPACK_LIBRARIES "-framework vecLib")
      message(STATUS "Using Apple vecLib framework.")
    elseif(EL_HAVE_DPOTRF_ACCELERATE)
      set(LAPACK_LIBRARIES "-framework Accelerate")
      message(STATUS "Using Apple Accelerate framework.")
    endif()
  endif()
endif()

if(NOT LAPACK_FOUND AND NOT DISABLE_OPENBLAS)
  include(ExternalOpenBLAS)
  if(HAVE_OPENBLAS)
    set(LAPACK_FOUND TRUE)
    set(LAPACK_LIBRARIES ${OPENBLAS_LIBS})
  elseif(BUILD_OPENBLAS)
    message(FATAL_ERROR "Demand that OpenBLAS be built could not be met")
  endif()
endif()
if(NOT LAPACK_FOUND AND NOT DISABLE_BLIS)
  include(ExternalBLISLAPACK)  
  if(HAVE_BLIS_LAPACK)
    set(LAPACK_FOUND TRUE)
    set(LAPACK_LIBRARIES ${BLIS_LAPACK_LIBS})
  elseif(BUILD_BLIS_LAPACK)
    set(FATAL_ERROR "Demand that BLIS+LAPACK be built could not be met")
  endif()
endif()
if(NOT LAPACK_FOUND)
  message(STATUS 
    "--> Searching for optimized LAPACK and BLAS libraries on your machine.")
  find_package(LAPACK)
endif()

message(STATUS "BLAS library: ${BLAS_LIBRARIES}")
message(STATUS "LAPACK library: ${LAPACK_LIBRARIES}")
message(STATUS "=========")

# --------------------------------------------------
# Subdirectories that need to be processed
   
macro(append_subdir_files variable dirname)
get_directory_property(holder DIRECTORY ${dirname} DEFINITION ${variable})
foreach(depfile ${holder})
  list(APPEND ${variable} "${dirname}/${depfile}")
endforeach()
endmacro()


#
# BLACS
#
add_subdirectory(BLACS)
append_subdir_files(blacs "BLACS/SRC")

#
# TOOLS
#
add_subdirectory(TOOLS)
append_subdir_files(tools TOOLS)
append_subdir_files(tools-C TOOLS)
append_subdir_files(extra_lapack "TOOLS/LAPACK")

#
# PBLAS
#
add_subdirectory(PBLAS)
append_subdir_files(pblas "PBLAS/SRC")
append_subdir_files(pblas-F "PBLAS/SRC")
append_subdir_files(pbblas "PBLAS/SRC/PBBLAS")
append_subdir_files(ptzblas "PBLAS/SRC/PTZBLAS")
append_subdir_files(ptools "PBLAS/SRC/PTOOLS")

#
# REDIST
#
add_subdirectory(REDIST)
append_subdir_files(redist "REDIST/SRC")

#
# SRC
#
add_subdirectory(SRC)
append_subdir_files(src "SRC")
append_subdir_files(src-C "SRC")

if(UNIX)
  add_library(scalapack ${blacs} ${tools} ${tools-C} ${extra_lapack} ${pblas} ${pblas-F} ${ptzblas} ${ptools} ${pbblas} ${redist} ${src} ${src-C})
  if(BUILT_OPENBLAS)
    add_dependencies(scalapack project_openblas)
  endif()
  if(BUILT_BLIS_LAPACK)
    add_dependencies(scalapack project_blis_lapack)
  endif()
  target_link_libraries(scalapack ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${MPI_C_LIBRARIES} ${MPI_Fortran_LIBRARIES})
  if(MPI_LINK_FLAGS)
    set_target_properties(scalapack PROPERTIES LINK_FLAGS ${MPI_LINK_FLAGS})
  endif()
  scalapack_install_library(scalapack)
else() # Need to separate Fortran and C Code
  add_library(scalapack ${blacs} ${tools-C} ${pblas} ${ptools} ${redist} ${src-C})
  if(BUILT_OPENBLAS)
    add_dependencies(scalapack project_openblas)
  endif()
  if(BUILT_BLIS_LAPACK)
    add_dependencies(scalapack project_blis_lapack)
  endif()
  target_link_libraries(scalapack ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${MPI_C_LIBRARIES})
  add_library(scalapack-F ${pblas-F} ${pbblas} ${ptzblas} ${tools} ${src} ${extra_lapack} )
  if(BUILT_OPENBLAS)
    add_dependencies(scalapack-F project_openblas)
  endif()
  if(BUILT_BLIS_LAPACK)
    add_dependencies(scalapack-F project_blis_lapack)
  endif()
  target_link_libraries(scalapack-F ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${MPI_Fortran_LIBRARIES})
  if(MPI_LINK_FLAGS)
    set_target_properties(scalapack PROPERTIES LINK_FLAGS ${MPI_LINK_FLAGS})
    set_target_properties(scalapack-F PROPERTIES LINK_FLAGS ${MPI_LINK_FLAGS})
  endif()
  scalapack_install_library(scalapack)
  scalapack_install_library(scalapack-F)
endif()
add_subdirectory(TESTING)

# --------------------------------------------------
# CPACK Packaging 

set(CPACK_PACKAGE_NAME "ScaLAPACK")
set(CPACK_PACKAGE_VENDOR "University of Tennessee, Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "ScaLAPACK- Linear Algebra Package")
set(SCALAPACK_VERSION 2.0.2)
set(CPACK_PACKAGE_VERSION_MAJOR 2)
set(CPACK_PACKAGE_VERSION_MINOR 0)
set(CPACK_PACKAGE_VERSION_PATCH 2)
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "SCALAPACK")
if(WIN32 AND NOT UNIX)
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  set(CPACK_NSIS_HELP_LINK "http:\\\\\\\\http://icl.cs.utk.edu/lapack-forum")
  set(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.netlib.org/scalapack")
  set(CPACK_NSIS_CONTACT "scalapack@eecs.utk.edu")
  set(CPACK_NSIS_MODIFY_PATH ON)
  set(CPACK_NSIS_DISPLAY_NAME "SCALAPACK-${SCALAPACK_VERSION}")
  set(CPACK_PACKAGE_RELOCATABLE "true")
else()
  set(CPACK_GENERATOR "TGZ")
  set(CPACK_SOURCE_GENERATOR TGZ)
  set(CPACK_SOURCE_PACKAGE_FILE_NAME "scalapack-${SCALAPACK_VERSION}" )
  set(CPACK_SOURCE_IGNORE_FILES ~$ .svn ${CPACK_SOURCE_IGNORE_FILES} )
endif()
include(CPack)

# --------------------------------------------------

export(TARGETS scalapack FILE scalapack-targets.cmake)

configure_file(
  ${SCALAPACK_SOURCE_DIR}/CMAKE/scalapack-config-version.cmake.in
  ${SCALAPACK_BINARY_DIR}/scalapack-config-version.cmake @ONLY)
configure_file(
  ${SCALAPACK_SOURCE_DIR}/CMAKE/scalapack-config-build.cmake.in
  ${SCALAPACK_BINARY_DIR}/scalapack-config.cmake @ONLY)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/CMAKE/scalapack.pc.in 
  ${CMAKE_CURRENT_BINARY_DIR}/scalapack.pc)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/scalapack.pc 
  DESTINATION ${PKG_CONFIG_DIR})

configure_file(
  ${SCALAPACK_SOURCE_DIR}/CMAKE/scalapack-config-install.cmake.in
  ${SCALAPACK_BINARY_DIR}/CMakeFiles/scalapack-config.cmake @ONLY)
install(FILES
  ${SCALAPACK_BINARY_DIR}/CMakeFiles/scalapack-config.cmake
  ${SCALAPACK_BINARY_DIR}/scalapack-config-version.cmake
  DESTINATION lib/cmake/scalapack-${SCALAPACK_VERSION})

install(EXPORT scalapack-targets
  DESTINATION lib/cmake/scalapack-${SCALAPACK_VERSION})
