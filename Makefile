
include ./makefile_libpath


MOD_path = -I./MODULE
LIB_path = -L./MODULE
LIB_name = -lMODULE_CRTM
LIB_BASE = $(LIB_path) $(LIB_name)


gfc = gfortran -ffree-form -c
gf  = gfortran


CRTM_PROw = CRTM_Main_wrf.f90
CRTM_OBJw = CRTM_Main_wrf.o
CRTM_PRO = CRTM_Main.f90
CRTM_OBJ = CRTM_Main.o

NCEP025_PRO = CRTM_NCEP025.f90
NCEP025_OBJ = CRTM_NCEP025.o

default:
	@echo 'make NCEP025  = make the CRTM Program for NCEP 0.25 degree data'
	@echo 'make CRTM     = make the CRTM Program'

CRTM:
	$(gfc) $(MOD_path) $(CRTM_PRO)
	$(gf)  $(CRTM_OBJ) $(LIB_BASE) $(LIB_CRTM) -o CRTM_Main


CRTMw:
	$(gfc) $(MOD_path) $(CRTM_PROw)
	$(gf)  $(CRTM_OBJw) $(LIB_BASE) $(LIB_CRTM) -o CRTM_Main_w


NCEP025: $(NCEP025_OBJ)
	$(gf) $(NCEP025_OBJ) $(LIB_BASE) $(LIB_CRTM) -o CRTM_NCEP025
	rm $(NCEP025_OBJ)

CRTM_NCEP025.o:
	$(gfc) $(MOD_path) $(NCEP025_PRO)

clean:
	rm *.gif
