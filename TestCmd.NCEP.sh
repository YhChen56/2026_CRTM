
PGname=./CRTM_Main
SetFile=CRTM_SETTING_NCEP025_IR
#SetFile=CRTM_SETTING_NCEP025_VIS
InFile=/data4/crtm/NCEP_GFS025/CRTM_input_Data/2017090400/CRTMinput.ncepp25.i2017090400.f2017090406
OutFile=CRTMout.ncep.dat
${PGname} ${SetFile} ${InFile} ${OutFile}

