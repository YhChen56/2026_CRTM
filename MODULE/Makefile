
include ../makefile_libpath

gfc = gfortran -ffree-form -c


OBJ = Module_a_function.o \
      Module_AngleCalculate.o \
      Module_CRTM_Parameters.o \
      Module_CRTM_ModelDataLoad.o \
      Module_CRTM_ValueTransport.o \
      Module_CRTM_Check.o \
      Module_CRTM_Compute.o \
      Module_CRTM_DataArray.o \
      Module_GeoCount.o \


libMOD:
	$(gfc) Module_a_function.f90
	$(gfc) Module_AngleCalculate.f90
	$(gfc) Module_CRTM_Parameters.f90
	$(gfc) Module_CRTM_ModelDataLoad.f90
	$(gfc) Module_CRTM_ValueTransport.f90
	$(gfc) Module_CRTM_Check.f90
	$(gfc) $(MOD_path_CRTM) Module_CRTM_Compute.f90
	$(gfc) Module_CRTM_DataArray.f90
	$(gfc) Module_GeoCount.f90
	ar crv libMODULE_CRTM.a $(OBJ)
clean:
	rm libMODULE_CRTM.a  2>/dev/null
	rm *.mod             2>/dev/null
	rm *.o               2>/dev/null

