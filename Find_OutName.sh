 CRTM_Input=$1
 [[ ${CRTM_Input} == '' ]] && exit


 CRTM_Input_Name=`basename ${CRTM_Input}`
 CRTM_Input_Dir=`dirname ${CRTM_Input}`



### NCEP 0.25 Degree:
#CRTM_Input=/data4/crtm/NCEP_GFS025/CRTM_input_Data/2017090600/CRTMinput.ncepp25.i2017090600.f2017090712
#CRTM_Output=/data4/crtm/NCEP_GFS025/CRTM_output_Data/2017090600/CRTMoutput.ncepp25.i2017090600.f2017090712
 CRTM_Output_Name=''
 CRTM_Output_Dir=''
 replace_str0='/data4/crtm/NCEP_GFS025/CRTM_input_Data/'
 replace_str1='/data4/crtm/NCEP_GFS025/CRTM_output_Data/'
 len0=`expr length ${replace_str0}`
 len1=$(( ${len0} + 1 ))
 str_look=`expr substr ${CRTM_Input_Dir} 1 ${len0}`
 str_keep=`expr substr ${CRTM_Input_Dir} ${len1} 250`
 [[ ${str_look} == ${replace_str0} ]] &&  CRTM_Output_Dir=${replace_str1}${str_keep}/


 replace_str0='CRTMinput.'
 replace_str1='CRTMoutput.'
 len0=`expr length ${replace_str0}`
 len1=$(( ${len0} + 1 ))
 str_look=`expr substr ${CRTM_Input_Name} 1 ${len0}`
 str_keep=`expr substr ${CRTM_Input_Name} ${len1} 250`
 [[ ${str_look} == ${replace_str0} ]] &&  CRTM_Output_Name=${replace_str1}${str_keep}

 CRTM_Output=${CRTM_Output_Dir}${CRTM_Output_Name}
 [[ ${CRTM_Output_Dir} != '' && ${CRTM_Output_Name} != '' ]] && echo ${CRTM_Output}




### WRF:
CRTM_Input=/data4/crtm/WRF/CRTM_input_Data/2017091100/CRTMinput.wrfd01.i2017091100.f2017091412
CRTM_Output=/data4/crtm/WRF/CRTM_output_Data/2017091100/CRTMoutput.wrfd01.i2017091100.f2017091412
 CRTM_Output_Name=''
 CRTM_Output_Dir=''
 replace_str0='/data4/crtm/WRF/CRTM_input_Data/'
 replace_str1='/data4/crtm/WRF/CRTM_output_Data/'
 len0=`expr length ${replace_str0}`
 len1=$(( ${len0} + 1 ))
 str_look=`expr substr ${CRTM_Input_Dir} 1 ${len0}`
 str_keep=`expr substr ${CRTM_Input_Dir} ${len1} 250`
 [[ ${str_look} == ${replace_str0} ]] &&  CRTM_Output_Dir=${replace_str1}${str_keep}/


 replace_str0='CRTMinput.'
 replace_str1='CRTMoutput.'
 len0=`expr length ${replace_str0}`
 len1=$(( ${len0} + 1 ))
 str_look=`expr substr ${CRTM_Input_Name} 1 ${len0}`
 str_keep=`expr substr ${CRTM_Input_Name} ${len1} 250`
 [[ ${str_look} == ${replace_str0} ]] &&  CRTM_Output_Name=${replace_str1}${str_keep}

 CRTM_Output=${CRTM_Output_Dir}${CRTM_Output_Name}
 [[ ${CRTM_Output_Dir} != '' && ${CRTM_Output_Name} != '' ]] && echo ${CRTM_Output}




