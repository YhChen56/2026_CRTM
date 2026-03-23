
import numpy as np
from scipy.interpolate import interp1d
from PIL import Image
import os
import sys


#CRTM_File = '/data/NCEP_GFS025/CRTM_output_Data/2018-11-28_00/CRTM_NCEP025_18112800_000'

if len(sys.argv) != 2 :
 info = 'python ' + sys.argv[0] + '  CRTM_FullFile'
 print(info)
 exit()


CRTM_File = sys.argv[1]

CRTM_Base = os.path.basename(CRTM_File)



Channels = ['B07','B08','B09','B10','B11','B12','B13','B14','B15','B16']
CRTM_Dir = os.path.dirname(CRTM_File)
ImageDir = CRTM_Dir + '/Image/'
if not os.path.isdir(ImageDir) :
 os.makedirs(ImageDir)




CRTM_ImageFile = Channels
for ch in range(len(Channels)):
  CRTM_ImageFile[ch] =  ImageDir + CRTM_Base + '_' + Channels[ch] + '.bmp'


XS = 1440
YS = 721

Dat = np.fromfile(CRTM_File,dtype=np.float32,count=XS*YS*41)
bytscl = interp1d([330,190],[0,255],bounds_error=False,fill_value=(0,255))

cnts = bytscl(Dat)
cnts = cnts.reshape(41,YS,XS)
cnts = np.flip(cnts,1)


for ch in range(len(Channels)):
 idx = 1+ch*4
 cnt      = cnts[idx,:,:].astype(np.uint8)
 ImageOut = Image.fromarray(cnt,'L')
 ImageOut.save(CRTM_ImageFile[ch])
 print(CRTM_ImageFile[ch])

