# WRF-LES
WRF-LES installation on Ubuntu 20.04

## 1. Environment
Ubuntu 20.04
gcc 9.4.0
g++ 9.4.0
gfortran 9.4.0

### 1.1 Install
```bash
pip install gfortran gcc g++ csh make m4 perl
```
```bash
which gfortran
which cpp
which gcc
which g++
gcc --version
```

### 1.2 测试环境
```bash
cd ~
mkdir Build_WRF
cd Build WRF
mkdir TESTS
cd TESTS
```
拷贝Fortran_C_tests.tar，或从[官网](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php)下载
```bash
tar -xf Fortran_C_tests.tar
```
Test #1: Fixed Format Fortran Test: TEST_1_fortran_only_fixed.f
```bash
gfortran TEST_1_fortran_only_fixed.f
./a.out
```
SUCCESS test 1 fortran only fixed format

Test #2: Free Format Fortran: TEST_2_fortran_only_free.f90

```bash
gfortran TEST_2_fortran_only_free.f90
./a.out
```
Assume Fortran 2003: has FLUSH, ALLOCATABLE, derived type, and ISO C Binding
SUCCESS test 2 fortran only free format

Test #3: C: TEST_3_c_only.c
```bash
gcc TEST_3_c_only.c
./a.out
```
SUCCESS test 3 c only

Test #4: Fortran Calling a C Function (our gcc and gfortran have different defaults, so we force both to always use 64 bit [-m64] when combining them): TEST_4_fortran+c_c.c, and TEST_4_fortran+x_f.f90
```bash
gcc -c -m64 TEST_4_fortran+c_c.c
gfortran -c -m64 TEST_4_fortran+c_f.f90
gfortran -m64 TEST_4_fortran+c_f.o TEST_4_fortran+c_c.o
./a.out
```
C function called by Fortran
Values are xx = 2.00 and ii = 1
SUCCESS test 4 fortran calling c

Test #5:csh In the command line, type:
```bash
./TEST_csh.csh
```

SUCCESS csh test

Test #6:perl In the command line, type:
```bash
./TEST_perl.pl
```
SUCCESS perl test

Test #7:sh In the command line, type:
```bash
./TEST_sh.sh
```
SUCCESS sh test

### 1.3 创建环境
```bash
cd ~/Build_WRF
mkdir LIBRARIES
cd LIBRARIES
```

#### 1.3.1 修改环境变量
```bash
vim ~/.bashrc
```
```text
# WRF
export DIR=/home/why/Build_WRF/LIBRARIES
export CC=gcc
export CXX=g++
export FC=gfortran
export FCFLAGS=-m64
export F77=gfortran
export FFLAGS=-m64
#zlib
export LDFLAGS=-L$DIR/grib2/lib
export CPPFLAGS=-I$DIR/grib2/include
#netcdf
export PATH=$DIR/netcdf/bin:$PATH
export NETCDF=$DIR/netcdf
#mpich
export PATH=$DIR/mpich/bin:$PATH
#WPS
export JASPERLIB=$DIR/grib2/lib
export JASPERINC=$DIR/grib2/include
export WRF_DIR=/home/why/Build_WRF/WRF-4.1.2
```
```bash
source ~/.bashrc
```

#### 1.3.2 zlib install
拷贝zlib-1.2.9.tar.gz
```bash
tar xzvf zlib-1.2.9.tar.gz
cd zlib-1.2.9
./configure --prefix=$DIR/grib2
make -j4
make install
cd .. 
```

#### 1.3.3 libpng install
拷贝 libpng-1.2.50.tar.gz
```bash
tar xzvf libpng-1.2.50.tar.gz
cd libpng-1.2.50
./configure --prefix=$DIR/grib2
make -j4
make install
cd ..
```

#### 1.3.4 jasper install
拷贝 jasper-1.900.1.tar.gz
```bash
tar xzvf jasper-1.900.1.tar.gz 
cd jasper-1.900.1
./configure --prefix=$DIR/grib2
make -j4
make install
cd ..
```

#### 1.3.5 NetCDF install
拷贝 netcdf-4.1.3.tar.gz
```bash
tar xzvf netcdf-4.1.3.tar.gz
cd netcdf-4.1.3
./configure --prefix=$DIR/netcdf --disable-dap --disable-netcdf-4 --disable-shared
make -j4
make install
cd ..
```

#### 1.3.6 mpich install
拷贝 mpich-3.0.4.tar.gz
```bash
tar xzvf mpich-3.0.4.tar.gz
cd mpich-3.0.4
./configure --prefix=$DIR/mpich
make -j4
make install
cd ..
```

### 1.4 Library Compatibility Tests
拷贝 Fortran_C_NETCDF_MPI_tests.tar 或从[官网](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php)下载。
```bash
tar -xf Fortran_C_NETCDF_MPI_tests.tar
```
**Test #1:** Fortran + C + NetCDF

The NetCDF-only test requires the include file from the NETCDF package be in this directory. Copy the file here:
```bash
cp ${NETCDF}/include/netcdf.inc .
```
Compile the Fortran and C codes for the purpose of this test (the -c option says to not try to build an executable). Type the following commands:
```bash
gfortran -c 01_fortran+c+netcdf_f.f
gcc -c 01_fortran+c+netcdf_c.c
gfortran 01_fortran+c+netcdf_f.o 01_fortran+c+netcdf_c.o \
        -L${NETCDF}/lib -lnetcdff -lnetcdf
./a.out
```

C function called by Fortran
Values are xx = 2.00 and ii = 1
SUCCESS test 1 fortran + c + netcdf

**Test #2:** Fortran + C + NetCDF + MPI

The NetCDF+MPI test requires include files from both of these packages be in this directory, but the MPI scripts automatically make the mpif.h file available without assistance, so no need to copy that one. Copy the NetCDF include file here:
```bash
cp ${NETCDF}/include/netcdf.inc .
```
Note that the MPI executables mpif90 and mpicc are used below when compiling. Issue the following commands:
```bash
mpif90 -c 02_fortran+c+netcdf+mpi_f.f
mpicc -c 02_fortran+c+netcdf+mpi_c.c
mpif90 02_fortran+c+netcdf+mpi_f.o \
02_fortran+c+netcdf+mpi_c.o \
        -L${NETCDF}/lib -lnetcdff -lnetcdf
mpirun ./a.out
```
C function called by Fortran
Values are xx = 2.00 and ii = 1
status = 2
SUCCESS test 2 fortran + c + netcdf + mpi

## 2. Building WRF v4.1.2

```bash
cd .. 
tar xzvf WRF-4.1.2.tar.gz
cd WRF-4.1.2
./configure
```
输入 33 （GNU 编译）

输入 1 （Nest选项）
```bash
./compile em_real >& log.compile
```
```bash
ls -ls main/*.exe
```
出现4个.exe即为成功

## 3. Building WPS
```bash
tar xzvf WPS-4.1.tar.gz
cd WPS-4.1
./clean -a
./configure
```
输入 1
```bash
./compile >& log.compile
```
```bash
ls -ls *.exe
```
出现3个.exe即为成功

## 4. Run WRF-LES
运行WRF-LES需要两步，
分别为运行ideal.exe和wrf.exe

两个可执行程序可以在main文件夹或者test/em_les文件夹中找到

### 4.1 Compile WRF-LES
#### 4.1.1 Registry.EM
```bash
vim ~/Build_WRF/WRF-4.1.2/Registry/Registry.EM
```
在第36-38行作以下修改
```text
state   real    plume     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "plume"         "plume"     "Dimensionless"

package   tracer_test1  tracer_opt==2       -             tracer:tr17_1,tr17_2,tr17_3,tr17_4,tr17_5,tr17_6,tr17_7,tr17_8,plume
```
```bash
source ~.bashrc
```

#### 4.1.2 solve_EM.F
```bash
vim ~/Build_WRF/WRF-4.1.2/dyn_em/solve_EM.F
```
在第267行添加以下内容
```text
! Edits (2024.10.15)
!----------------------------------------------------
!------ Tracers -------------------------------------
!----------------------------------------------------

   IF (config_flags%tracer_opt .eq. 2) THEN
     DO j = (jde + jds)/2, (jde + jds)/2,  1
       DO i = 1*(ide + ids)/6, 1*(ide + ids)/6, 1
          IF ( ips .LE. i .and. ipe .GE. i .and. jps .LE. j .and. jpe .GE. j ) THEN
            tracer(i, 1, j, P_plume) = 1.
          END IF
       END DO
     END DO
   END IF

!----------------------------------------------------
!------ end -----------------------------------------
!----------------------------------------------------
```
```bash
source ~/.bashrc
```

#### 4.1.3 module_initialize_ideal.F
```bash
vim ~/Build_WRF/WRF-4.1.2/dyn_em/module_initialize_ideal.F
```
这个文件主要用于控制羽流初始排放情况，详细修改需参考[Peisipand](https://peisipand.github.io/2022/06/10/WRF_Compiling_and_running_LES/)的博客。

实际使用的时候发现不修改这个文件也可以生成持续排放的羽流。
```bash
source ~/.bashrc
```
#### 4.1.4 compile LES
```bash
cd ~/Build_WRF/WRF-4.1.2
./clean -a
./compile em_les >& log.compile
```
输入33; 再输入1。
```bash
ls -ls main/*.exe
```
出现2个.exe即为成功

#### 4.1.5 input_sounding
- 第一行三个数据依次为
    + 气压 surface Pressure (mb)
    + 温度 surface potential Temperature (K)
    + 水汽 Surface vapor mixing ratio (g/kg)

- 第二行开始为
    + 海拔 Height (m)
    + 位温 Potential temperature (K)
    + 水汽 Water vapor Mixing ratio (g/kg)
    + U风 U wind speed (m/s)
    + V风 V wind speed (m/s)

[参考网站](https://home.chpc.utah.edu/~u0631741/wrf-sfire/WRF_Sfire_in_ideal_cases.pdf)
```text
 1000.00     300.00       0.00
   10.00     300.00       0.00       1.00      0.00
  250.00     300.45       0.00       1.00      0.00
  750.00     301.25       0.00       1.00      0.00
 1250.00     302.00       0.00       1.00      0.00
 1750.00     303.93       0.00       1.00      0.00
 2250.00     305.00       0.00       1.00      0.00
 2750.00     306.00       0.00       1.00      0.00
 3250.00     308.00       0.00       1.00      0.00
 3750.00     310.00       0.00       1.00      0.00     
 4250.00     311.00       0.00       1.00      0.00
 4750.00     313.00       0.00       1.00      0.00
 ```
 #### 4.1.6 namelist.input
 ```text
  &time_control
 run_days                            = 0,
 run_hours                           = 2,
 run_minutes                         = 00,
 run_seconds                         = 00,
 start_year                          = 0001, 0001, 0001, 
 start_month                         = 01,   01,   01,  
 start_day                           = 01,   01,   01, 
 start_hour                          = 00,   00,   00,  
 start_minute                        = 00,   00,   00,  
 start_second                        = 00,   00,   00,  
 end_year                            = 0001, 0001, 0001, 
 end_month                           = 01,   01,   01,   
 end_day                             = 01,   01,   01,  
 end_hour                            = 02,   02,   02, 
 end_minute                          = 00,   00,   00,  
 end_second                          = 00,   00,   00,  
 history_outname                     = '/home/why/Data/WRF_OUT/wrfout_d<domain>_<date>'
 history_interval_m                  = 00,   00,   00,  
 history_interval_s                  = 30,   30,   30,  
 frames_per_outfile                  = 1,    1,    1, 
 restart                             = .false.,
 restart_interval_m                  = 60,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = 0, ! 积分步长的整数部分
 time_step_fract_num                 = 3, ! 积分步长的分数部分
 time_step_fract_den                 = 10, ! 表示 0.3 > 0.18
 max_dom                             = 1,
 s_we                                = 1,     1,    1,
 e_we                                = 202,   202,  202,
 s_sn                                = 1,     1,    1,
 e_sn                                = 202,   202,  202,
 s_vert                              = 1,     1,    1,
 e_vert                              = 51,    51,   51,
 dx                                  = 30,    
 dy                                  = 30,   
 ztop                                = 5000,  5000,  5000,
 grid_id                             = 1,     2,     3,
 parent_id                           = 1,     1,     2,
 i_parent_start                      = 1,     316,   15,  
 j_parent_start                      = 1,     166,   30,   
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 0,
 smooth_option                       = 0
 /

 &physics
 mp_physics                          = 0,     0,      0, 
 ra_lw_physics                       = 0,     0,      0, 
 ra_sw_physics                       = 0,     0,      0,  
 radt                                = 0,     0,      0, 
 sf_sfclay_physics                   = 1,     1,      1,
 sf_surface_physics                  = 0,     0,      0,   
 bl_pbl_physics                      = 0,     0,      0, 
 bldt                                = 0,     0,      0, 
 cu_physics                          = 0,     0,      0, 
 cudt                                = 0,     0,      0,   
 isfflx                              = 2,
 num_soil_layers                     = 5,
 /

 &fdda
 /

 &dynamics
 tracer_opt                          = 2,      2,      2    ! Create Plume
 rk_ord                              = 3,
 diff_opt                            = 2,      2,      2,
 km_opt                              = 2,      2,      2,
 damp_opt                            = 0,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.2,    0.2,    0.2,    
 khdif                               = 1.,      1.,     1.,   
 kvdif                               = 1.,      1.,     1.,   
 c_s                                 = 0.18
 c_k                                 = 0.10
 mix_isotropic                       = 1
 smdiv                               = 0.1,    0.1,     0.1, 
 emdiv                               = 0.01,   0.01,    0.01,
 epssm                               = 0.1,    0.1,     0.1, 
 tke_heat_flux                       = 0.24,   0.24,    0.24, 
 time_step_sound                     = 6,      6,       6, 
 h_mom_adv_order                     = 5,      5,       5, 
 v_mom_adv_order                     = 3,      3,       3,  
 h_sca_adv_order                     = 5,      5,       5,
 v_sca_adv_order                     = 3,      3,        3,  
 mix_full_fields                     = .true., .true.,  .true., 
 non_hydrostatic                     = .true., .true.,  .true., 
 pert_coriolis                       = .true., .true.,  .true., 
 use_theta_m                         = 1,
 /

 &bdy_control
 periodic_x                          = .false., .false.,.false.,
 symmetric_xs                        = .false.,.false.,.false.,
 symmetric_xe                        = .false.,.false.,.false.,
 open_xs                             = .true.,.false.,.false.,
 open_xe                             = .true.,.false.,.false.,
 periodic_y                          = .false., .false.,.false.,
 symmetric_ys                        = .false.,.false.,.false.,
 symmetric_ye                        = .false.,.false.,.false.,
 open_ys                             = .true.,.false.,.false.,
 open_ye                             = .true.,.false.,.false.,
 nested                              = .false., .true.,  .true., 
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
 
 &ideal
 ideal_case = 9
 /
 ```
### 4.2 Run WRF-LES
```bash
./ideal.exe
```
```bash
./wrf.exe
```