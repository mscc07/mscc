#**************************************************************************
# INSTRUCTIONS
#**************************************************************************
# Always use CMake Version 3.2 or higher
#**************************************************************************
cmake_minimum_required(VERSION 3.22)
#**********************************************************************************************************
set(MANUAL_INSTALL OFF)
#**********************************************************************************************************
IF(MANUAL_INSTALL)
     set(MATH_ROOT "path to the lapack library") 
     set(FFTW_ROOT "path to the lapack library")
     set(LIBXC_ROOT "path to the lapack library")
else()
     set(MATH_ROOT ${CMAKE_BINARY_DIR}/lapack/)
     set(FFTW_ROOT ${CMAKE_BINARY_DIR}/fftw3/)
     set(LIBXC_ROOT ${CMAKE_BINARY_DIR}/libxc/)
endif()
#**********************************************************************************************************
option(USE_INTEL "Use Intel compilers" OFF)
option(USE_GNU "Use GNU compilers" OFF)
#********************************************************************************************************
option(USE_OPENACC "Use GNU compilers" OFF)
set(USE_MKL OFF)
#*************************************************************************************************************
if((USE_INTEL AND USE_GNU) OR (NOT USE_INTEL AND NOT USE_GNU))
    message(FATAL_ERROR "Please specify either -DUSE_INTEL=ON or -DUSE_GNU=ON. type 'cmake -DUSE_GNU=ON ..' or 'cmake -DUSE_INTEL=ON ..'")
endif()


if(USE_INTEL)
    set(CMAKE_C_COMPILER "mpiicc")
    set(CMAKE_CXX_COMPILER "mpiicpc")
    set(CMAKE_Fortran_COMPILER "mpiifort")
elseif(USE_GNU)
    set(CMAKE_C_COMPILER "mpicc")
    set(CMAKE_CXX_COMPILER "mpicxx")
    set(CMAKE_Fortran_COMPILER "mpif90")
elseif(USE_OPENACC)
    set(CMAKE_C_COMPILER "nvc")
    set(CMAKE_CXX_COMPILER "nvc++")
    set(CMAKE_Fortran_COMPILER "nvfortran")
endif()

#****************************************************************************************************************************
# LIBRARY version and url
#****************************************************************************************************************************
set(FFTW_VERSION "3.3.9")
set(FFTW_URL "http://www.fftw.org/fftw-${FFTW_VERSION}.tar.gz")

# Set LAPACK version 

set(LAPACK_VERSION "3.10.0")
set(LAPACK_URL "https://github.com/Reference-LAPACK/lapack/archive/v${LAPACK_VERSION}.tar.gz")


# Set libxc version 
set(LIBXC_VERSION "5.2.2")
set(LIBXC_URL "https://gitlab.com/libxc/libxc/-/archive/${LIBXC_VERSION}/libxc-${LIBXC_VERSION}.tar.bz2")

#Set yaml
set(REMOTE_URL "https://github.com/jbeder/yaml-cpp")

#**************************************************************************
project(amdkiit VERSION 1.0.1 LANGUAGES CXX C Fortran)
message(STATUS " ")
message(STATUS " *********************** ")
message(STATUS " ")

find_package(MPI REQUIRED)

message(STATUS " ")
message(STATUS " *********************** ")

message(STATUS " PROJECT VERSION: ${CMAKE_PROJECT_VERSION} ")

string(TIMESTAMP CURRENT_DATE "%d-%m-%Y")
message(STATUS " DATE: ${CURRENT_DATE} ")

string(TIMESTAMP CURRENT_TIME "%H:%M:%S")
message(STATUS " TIME: ${CURRENT_TIME} ")

configure_file(version.h.in ../version.f90)

#***************************************************************************************************************
# DOWNLOAD YAML-CPP
#***************************************************************************************************************
set(DELETE_DIR "${CMAKE_BINARY_DIR}/fftw3-prefix/")
file(REMOVE_RECURSE ${DELETE_DIR}/)
set(DELETE_DIR "${CMAKE_BINARY_DIR}/fftw3/")
file(REMOVE_RECURSE ${DELETE_DIR}/)

set(DELETE_DIR "${CMAKE_BINARY_DIR}/lapack-prefix/")
file(REMOVE_RECURSE ${DELETE_DIR}/)
set(DELETE_DIR "${CMAKE_BINARY_DIR}/lapack/")
file(REMOVE_RECURSE ${DELETE_DIR}/)

set(DELETE_DIR "${CMAKE_BINARY_DIR}/libxc-prefix/")
file(REMOVE_RECURSE ${DELETE_DIR}/)
set(DELETE_DIR "${CMAKE_BINARY_DIR}/libxc/")
file(REMOVE_RECURSE ${DELETE_DIR}/)


set(DOWNLOAD_DIR "${CMAKE_BINARY_DIR}/../yaml-cpp")
file(REMOVE_RECURSE ${DOWNLOAD_DIR}/)
execute_process(
    COMMAND git clone ${REMOTE_URL} ${DOWNLOAD_DIR}
    RESULT_VARIABLE GIT_RESULT
)

if(NOT GIT_RESULT EQUAL 0)
    message(FATAL_ERROR "Failed to clone repository. Git returned error code ${GIT_RESULT}")
endif()
#*****************************************************************************************************************
# Intel Fortran
#*****************************************************************************************************************
if( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" )
   message(STATUS " ")
#******************************************************************************************************************
include(ExternalProject)
#*****************************************************************************************************************
# Download and extract :
#       FFTW
#*****************************************************************************************************************
if(MANUAL_INSTALL STREQUAL "OFF")
if(USE_MKL STREQUAL "OFF")
ExternalProject_Add(
    fftw3
    DOWNLOAD_COMMAND wget ${FFTW_URL}
    COMMAND tar -zxvf fftw-${FFTW_VERSION}.tar.gz
    COMMAND ${CMAKE_COMMAND} -E remove_directory -f fftw3/
    COMMAND ${CMAKE_COMMAND} -E rename  fftw-${FFTW_VERSION} fftw3
    CONFIGURE_COMMAND CC=mpiicc CXX=mpiicpc FC=mpiifort <SOURCE_DIR>/configure --enable-mpi --enable-threads --enable-openmp --prefix=${CMAKE_BINARY_DIR}/fftw3
    BUILD_COMMAND make CC=mpiicc CXX=mpiicpc FC=mpiifort
    INSTALL_COMMAND make install
)
#*****************************************************************************************************************
## LAPACK
#*****************************************************************************************************************
ExternalProject_Add(
    lapack
    DOWNLOAD_COMMAND wget ${LAPACK_URL}
    COMMAND tar -zxvf  v${LAPACK_VERSION}.tar.gz
    COMMAND ${CMAKE_COMMAND} -E rename lapack-${LAPACK_VERSION} lapack
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
)
ExternalProject_Add_Step(
    lapack
    copy_make_inc
    COMMAND ${CMAKE_COMMAND} -E copy make.inc.example make.inc
    COMMAND sed -i "s/^CC = gcc/CC = mpiicc/" make.inc
    COMMAND sed -i "s/^FC = gfortran/FC = mpiifort/" make.inc
    COMMAND sed -i "s/^FFLAGS = -O2 -frecursive/FFLAGS = -O2 -recursive/" make.inc
    COMMAND sed -i "s/^FFLAGS_NOOPT = -O0 -frecursive/FFLAGS_NOOPT = -O0 -recursive/" make.inc
    COMMAND sed -i "s/^TIMER = INT_ETIME/#TIMER = INT_ETIME/" make.inc
    COMMAND sed -i "s/^#TIMER = INT_CPU_TIME/TIMER = INT_CPU_TIME/" make.inc
    DEPENDEES download
    WORKING_DIRECTORY <SOURCE_DIR>
)
ExternalProject_Add_Step(
    lapack
    build_blaslib
    COMMAND make blaslib  
    COMMAND make lapacklib 
    COMMAND mkdir ${CMAKE_BINARY_DIR}/lapack/
    COMMAND cp  liblapack.a  ${CMAKE_BINARY_DIR}/lapack/
    COMMAND cp  librefblas.a  ${CMAKE_BINARY_DIR}/lapack/
    DEPENDEES copy_make_inc
    WORKING_DIRECTORY <SOURCE_DIR>
)
endif() #MKL 
#**************************************************************************
#   libxc
#**************************************************************************
ExternalProject_Add(
    libxc
    DOWNLOAD_COMMAND wget ${LIBXC_URL}
    COMMAND tar -xjf  libxc-${LIBXC_VERSION}.tar.bz2
    COMMAND ${CMAKE_COMMAND} -E remove_directory -f libxc/ 
    COMMAND ${CMAKE_COMMAND} -E rename libxc-${LIBXC_VERSION} libxc
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    WORKING_DIRECTORY <SOURCE_DIR>
 )
 ExternalProject_Add_Step(
     libxc
     autoconfig_step
     COMMAND autoreconf -i
     DEPENDEES download
     WORKING_DIRECTORY <SOURCE_DIR>
 )
 ExternalProject_Add_Step(
     libxc
     config_step
     COMMAND CC=icc CFLAGS=-std=c99 FC=ifort ./configure --prefix=${CMAKE_BINARY_DIR}/libxc/ 
     COMMAND make
     COMMAND make check
     COMMAND make install
     DEPENDEES autoconfig_step
     WORKING_DIRECTORY <SOURCE_DIR>
 )
#**************************************************************************
#     set(MATH_ROOT ${CMAKE_BINARY_DIR}/lapack/) # change path for GNU COMPILER
#     set(FFTW_ROOT ${CMAKE_BINARY_DIR}/fftw3/)
#     set(LIBXC_ROOT ${CMAKE_BINARY_DIR}/libxc/)
#***************************************************************************
endif() #Manual install
   include_directories(${FFTW_ROOT}include)
   include_directories(${LIBXC_ROOT}include)

#**************************************************************************
if(USE_MKL)
     set(MATH_ROOT $ENV{MKLROOT})
     include_directories(${MATH_ROOT}/include)
     set(MATH_LIB "-L${MATH_ROOT}/lib/intel64/ -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lmkl_scalapack_lp64 -lmkl_blacs_intelmpi_lp64")  
 else()
     set(MATH_LIB "-L${MATH_ROOT} -llapack -lrefblas -lgfortran")
     include_directories(${FFTW_ROOT}include)
     set(FFTW_LIB "-L${FFTW_ROOT}lib -lfftw3")
 endif()
   include_directories(${LIBXC_ROOT}/include)
   set(LIBXC_LIB "-L${LIBXC_ROOT}lib -lxcf90 -lxc")
   
   set (LIBS "${MPI_Fortran_LIBRARIES} ${MATH_LIB} ${FFTW_LIB} ${LIBXC_LIB}")
   message(STATUS " *********************** ")     
   message(STATUS " MATH LIBS:" ${MATH_LIB})
   message(STATUS " ")
   message(STATUS " FFTW LIBS:" ${FFTW_LIB})
   message(STATUS " ")
   message(STATUS " LIBXC LIBS:" ${LIBXC_LIB})   
   message(STATUS " *********************** ")
   message(STATUS " ")
 
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -pg -O0 -g -traceback -fpe0 -check all -heap-arrays")
#set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -pg -O2 -g -qopt-report=5 -simd -shared-intel -ldl -xCORE-AVX512 -axCORE-AVX512") #intel advisor

#******************************************************************************************************************
# GNU
#******************************************************************************************************************
elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")

   message(STATUS " FORTRAN COMPILER: " ${CMAKE_Fortran_COMPILER_ID})
   message(STATUS " ")
#******************************************************************************************************************
if(MANUAL_INSTALL STREQUAL "OFF")
include(ExternalProject)
# Download and extract FFTW
ExternalProject_Add(
    fftw3
    DOWNLOAD_COMMAND wget ${FFTW_URL}
    COMMAND tar -zxvf fftw-${FFTW_VERSION}.tar.gz
    COMMAND ${CMAKE_COMMAND} -E rename fftw-${FFTW_VERSION} fftw3
    CONFIGURE_COMMAND <SOURCE_DIR>/configure --enable-mpi --enable-threads --enable-openmp --prefix=${CMAKE_BINARY_DIR}/fftw3
    BUILD_COMMAND make
    INSTALL_COMMAND make install
    COMMAND ${CMAKE_COMMAND} -E remove_directory -f fftw3-prefix/
)

## LAPACK
ExternalProject_Add(
    lapack
    DOWNLOAD_COMMAND wget ${LAPACK_URL}
    COMMAND tar -zxvf  v${LAPACK_VERSION}.tar.gz
    COMMAND ${CMAKE_COMMAND} -E rename lapack-${LAPACK_VERSION} lapack
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
)
ExternalProject_Add_Step(
    lapack
    copy_make_inc
    COMMAND ${CMAKE_COMMAND} -E copy make.inc.example make.inc
    DEPENDEES download
    WORKING_DIRECTORY <SOURCE_DIR>
)
ExternalProject_Add_Step(
    lapack
    build_blaslib
    COMMAND make blaslib
    COMMAND make lapacklib
    COMMAND mkdir ${CMAKE_BINARY_DIR}/lapack/
    COMMAND cp  liblapack.a  ${CMAKE_BINARY_DIR}/lapack/
    COMMAND cp  librefblas.a  ${CMAKE_BINARY_DIR}/lapack/
    DEPENDEES copy_make_inc
    WORKING_DIRECTORY <SOURCE_DIR>
)

# LIBXC
ExternalProject_Add(
    libxc
    DOWNLOAD_COMMAND wget ${LIBXC_URL}
    COMMAND tar -xjf  libxc-${LIBXC_VERSION}.tar.bz2
    COMMAND ${CMAKE_COMMAND} -E rename libxc-${LIBXC_VERSION} libxc
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    WORKING_DIRECTORY <SOURCE_DIR>
)
ExternalProject_Add_Step(
    libxc
    autoconfig_step
    COMMAND autoreconf -i
    DEPENDEES download
    WORKING_DIRECTORY <SOURCE_DIR>
)
ExternalProject_Add_Step(
    libxc
    config_step
    COMMAND ./configure --prefix=${CMAKE_BINARY_DIR}/libxc/ CC=c99
    COMMAND make
    COMMAND make check
    COMMAND make install
    DEPENDEES autoconfig_step
    WORKING_DIRECTORY <SOURCE_DIR>
)
#***************************************************************************
# Set LIBRARY path here
#***************************************************************************
#     set(MATH_ROOT ${CMAKE_BINARY_DIR}/lapack/) # change path for GNU COMPILER
#     set(FFTW_ROOT ${CMAKE_BINARY_DIR}/fftw3/)
#     set(LIBXC_ROOT ${CMAKE_BINARY_DIR}/libxc/)
#***************************************************************************
endif() #Manual install
   include_directories(${FFTW_ROOT}include)
   include_directories(${LIBXC_ROOT}include)

   set(MATH_LIB "-L${MATH_ROOT} -llapack -lrefblas")
   set(FFTW_LIB "-L${FFTW_ROOT}lib -lfftw3")
   set(LIBXC_LIB "-L${LIBXC_ROOT}lib -lxcf90 -lxc")

   message(STATUS " *********************** ")
   message(STATUS " MATH LIBS:" ${MATH_LIB})
   message(STATUS " ")
   message(STATUS " FFTW LIBS:" ${FFTW_LIB})
   message(STATUS " ")
   message(STATUS " LIBXC LIBS:" ${LIBXC_LIB})
   message(STATUS " ")
   
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -pg -O0 -g -fbacktrace -ffpe-trap=zero -fcheck=all")
#**************************************************************************
# OPENACC
#**************************************************************************

 elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
     message(STATUS " FORTRAN COMPILER: " ${CMAKE_Fortran_COMPILER_ID})
     message(STATUS " ")
#***************************************************************************
if(MANUAL_INSTALL STREQUAL "OFF")
 include(ExternalProject)
 set(CMAKE_Fortran_FLAGS "-O2 -Mrecursive")
 # Download and extract FFTW
 ExternalProject_Add(
     fftw3
     URL ${FFTW_URL}
     CONFIGURE_COMMAND <SOURCE_DIR>/configure --enable-mpi --enable-threads --enable-openmp --prefix=${CMAKE_BINARY_DIR}/fftw3
     BUILD_COMMAND make
     INSTALL_COMMAND make install
 )

## LAPACK
 ExternalProject_Add(
     lapack
     URL ${LAPACK_URL}
     CONFIGURE_COMMAND ""
     BUILD_COMMAND ""
     INSTALL_COMMAND ""
 )
 ExternalProject_Add_Step(
     lapack
     copy_make_inc
     COMMAND ${CMAKE_COMMAND} -E copy make.inc.example make.inc
     COMMAND sed -i "s/^CC = gcc/CC = nvc/" make.inc
     COMMAND sed -i "s/^FC = gfortran/CC = nvfortran/" make.inc
     COMMAND sed -i "s/^FFLAGS = -O2 -frecursive/FFLAGS = -O2 -Mrecursive/" make.inc
     COMMAND sed -i "s/^FFLAGS_NOOPT = -O0 -frecursive/FFLAGS_NOOPT = -O0 -Mrecursive/" make.inc
     COMMAND sed -i "s/^TIMER = INT_ETIME/#TIMER = INT_ETIME/" make.inc
     COMMAND sed -i "s/^#TIMER = INT_CPU_TIME/TIMER = INT_CPU_TIME/" make.inc
     DEPENDEES download
     WORKING_DIRECTORY <SOURCE_DIR>
 )
 ExternalProject_Add_Step(
     lapack
     build_blaslib
     COMMAND make blaslib
     COMMAND make lapacklib
     COMMAND mkdir ${CMAKE_BINARY_DIR}/lapack/
     COMMAND cp  liblapack.a  ${CMAKE_BINARY_DIR}/lapack/
     COMMAND cp  librefblas.a  ${CMAKE_BINARY_DIR}/lapack/
     DEPENDEES copy_make_inc
     WORKING_DIRECTORY <SOURCE_DIR>
 )
# LIBXC
 ExternalProject_Add(
     libxc
     URL ${LIBXC_URL}
     CONFIGURE_COMMAND ""
     BUILD_COMMAND ""
     INSTALL_COMMAND ""
     WORKING_DIRECTORY <SOURCE_DIR>
 )
 ExternalProject_Add_Step(
     libxc
     autoconfig_step
     COMMAND autoreconf -i
     DEPENDEES download
     WORKING_DIRECTORY <SOURCE_DIR>
 )
 ExternalProject_Add_Step(
     libxc
     config_step
     COMMAND ./configure --prefix=${CMAKE_BINARY_DIR}/libxc/ CC=c99
     COMMAND make
     COMMAND make check
     COMMAND make install
     DEPENDEES autoconfig_step
     WORKING_DIRECTORY <SOURCE_DIR>
 )
 #***************************************************************************
 # Set LIBRARY path here
 #***************************************************************************
 #     set(MATH_ROOT ${CMAKE_BINARY_DIR}/lapack/) # change path for GNU COMPILER
 #     set(FFTW_ROOT ${CMAKE_BINARY_DIR}/fftw3/)
 #     set(LIBXC_ROOT ${CMAKE_BINARY_DIR}/libxc/)
 #***************************************************************************
endif() #Manual intsall
    include_directories(${FFTW_ROOT}include)
    include_directories(${LIBXC_ROOT}include)
    include_directories(${MPI_INCLUDE_PATH})

    set(MATH_LIB "-L${MATH_ROOT} -llapack -lrefblas")
    set(FFTW_LIB "-L${FFTW_ROOT}lib -lfftw3")
    set(LIBXC_LIB "-L${LIBXC_ROOT}lib -lxcf90 -lxc")

    set (LIBS "${MPI_Fortran_LIBRARIES}" "${MATH_LIB}" "${FFTW_LIB}" "${LIBXC_LIB}")
    message(STATUS " *********************** ")
    message(STATUS " MATH LIBS:" ${MATH_LIB})
    message(STATUS " ")
    message(STATUS " FFTW LIBS:" ${FFTW_LIB})
    message(STATUS " ")
    message(STATUS " LIBXC LIBS:" ${LIBXC_LIB})
    message(STATUS " ")
    message(STATUS " MPI LIBS:" ${MPI_Fortran_LIBRARIES})
    message(STATUS " *********************** ")
    message(STATUS " ")

 set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -acc -fast -Minfo=accel -O0 -g ") #-fbacktrace -ffpe-trap=zero -fcheck=all")

else()
   message(FATAL_ERROR "could not find any compiler")
endif()
#*******************************************************************************************
#NOTHING TO CHANGE
#*******************************************************************************************
set(CMAKE_BUILD_TYPE Debug)

include(FortranCInterface)
FortranCInterface_VERIFY(CXX)


add_subdirectory(yaml-cpp EXCLUDE_FROM_ALL)

set(CMAKE_CXX_STANDARD 11)
add_library(fortran_yaml_cpp yaml_types.f90 yaml.f90 yaml.cpp)
target_link_libraries(fortran_yaml_cpp yaml-cpp)
target_include_directories(fortran_yaml_cpp PUBLIC yaml-cpp/include)

set(LIB "-L/${CMAKE_BINARY_DIR}/yaml-cpp/lib64/libyaml-cpp.a")
add_executable(${CMAKE_PROJECT_NAME}.x lbfgs.f print_logo_amdkiit.f90 yamlread.f90 system_data_types.f90 max_parameter_pp.f90 kinds.f90 atom_info.f90 atomic_basis.f90 cg.f90 constants.f90 density.f90 exp_igr.f90 fft_interface.f90 fft_support.f90 gradient.f90 gvectors.f90 interpolgrid.f90 kinds.f90 sbt.f90 main.f90 math.f90 max_parameter_pp.f90 mympi.f90 nuclear_grad.f90 apply_pbc.f90 potential.f90 pseudopotential.f90 read_pp_file.f90 readpsp.f90 readstring.f90 set_cell.f90 system_data_types.f90 total_energy.f90 wfn_initialize.f90 xc.f90 bfgs_mod.f90 function_val.f90 backtracking.f90 readupf.f90 spline_inter.f90 spline.f90 spherical_func.f90 bessel_func.f90 init_module.f90 do_opt.f90 version.f90 eprint.f90 set_environment.f90 random.f90 md_tools.f90 update_velver.f90 molecular_dynamics.f90)
set_target_properties(${CMAKE_PROJECT_NAME}.x PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries(${CMAKE_PROJECT_NAME}.x fortran_yaml_cpp)
target_link_libraries(${CMAKE_PROJECT_NAME}.x ${FFTW_LIB})
target_link_libraries(${CMAKE_PROJECT_NAME}.x ${MATH_LIB})
target_link_libraries(${CMAKE_PROJECT_NAME}.x ${LIBXC_LIB})
#target_link_libraries(${CMAKE_PROJECT_NAME}.x -lmpi)
#target_link_libraries(${CMAKE_PROJECT_NAME}.x ${MPI_Fortran_LIBRARIES})

target_include_directories(${CMAKE_PROJECT_NAME}.x PRIVATE ${FFTW_ROOT}include)
target_include_directories(${CMAKE_PROJECT_NAME}.x PRIVATE ${LIBXC_ROOT}include)
#=============================
# Define the version number macro
target_compile_definitions(${CMAKE_PROJECT_NAME}.x PRIVATE VERSION="${CMAKE_PROJECT_VERSION}")
