make CRTM

if [[ $? -eq 0 ]]
then
 PGname=./CRTM_Main
# SetFile=CRTM_SETTING_WRF_VIS
 SetFile=CRTM_SETTING_WRF_IR
 InFile=/data4/crtm/WRF/CRTM_input_Data/2017090400/CRTMinput.wrfd01.i2017090400.f2017090401
 OutFile=CRTMout.wrf.dat
 ${PGname} ${SetFile} ${InFile} ${OutFile}
fi
