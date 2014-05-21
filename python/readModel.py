import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt
from common import createFolder
import sys,re, os.path
from const import outFolderName, modelFolderName, numModels, fin1


outFolder = createFolder(outFolderName)

outMaxVelRadFile = "outMaxVelRad"
outMaxVelTanFile = "outMaxVelTan"
outCMDifFile = "outCMDif"
outAMFile = "outAM"

outMaxVelRad = open(os.path.join(outFolder,outMaxVelRadFile), "w")
outMaxVelTan = open(os.path.join(outFolder,outMaxVelTanFile), "w")
outCMDif = open(os.path.join(outFolder,outCMDifFile), "w")
outAM = open(os.path.join(outFolder,outAMFile), "w")


#change here and change in makemodels.py
REL_FOLDER_NAME = "outModel"
outmodelsuffix = "TREEBOD-"

for i in range(1,numModels+1):
	print("i=%d"%i)
	filename = os.path.join(modelFolderName, REL_FOLDER_NAME, "%s%d.txt" % (outmodelsuffix,i))
	with open(filename, 'r') as f:
		first_line = f.readline()
	rexpr = re.compile("\s*([\d.E\+-]+)(?:\s+[\d.E\+-]+){99}" + "(?:\s+([\d.E\+-]+))" * 10 + "(?:\s+[\d.E\+-]+)+\s*")
	g = rexpr.match(first_line)
	partTypes = []
	numpart = int(float((g.group(1))))
	numberTypes = int(float(g.group(2)))	
	for j in  range(numberTypes):
		partTypes.append([int(float(g.group(3 + j*2))), float(g.group(4 + j*2))])
	print("PART TYPES")
	print(partTypes)
	
	data = np.loadtxt(filename, skiprows=1+numpart)
	print(data.shape)
	radius = np.sqrt(data[0:numpart,0] ** 2 + data[0:numpart,1] ** 2 + data[0:numpart,2] ** 2)
	npAnt = 0
	cm1 = np.array([0.0,0.0,0.0])
	cm2 = np.array([0.0,0.0,0.0])
	am = np.array([0.0,0.0,0.0])
	for k in range(fin1):
		npr = partTypes[k][0]	
		mp = partTypes[k][1]
		cm1 += mp * np.sum(data[npAnt:npr,:], axis=0)
		am+=mp * np.sum(np.cross(data[npAnt:npr,:], data[numpart+npAnt:numpart+npr,:]), axis = 0)
		npAnt = npr
	for k in range(fin1, len(partTypes)):
		npr = partTypes[k][0]	
		mp = partTypes[k][1]
		cm2 += mp * np.sum(data[npAnt:npr,:], axis=0)
		am+=mp * np.sum(np.cross(data[npAnt:npr,:], data[numpart+npAnt:numpart+npr,:]), axis = 0)
		npAnt = npr

	outCMDif.write("%s\n" % " ".join(map(str,cm2-cm1)))
	outAM.write("%s\n" % " " .join(map(str,am)))
	vRad = np.sqrt(data[numpart:,0] ** 2 + data[numpart:,1] ** 2 + data[numpart:,2] ** 2)	
	vrMax = np.max(vRad)
	vTheta = np.arctan(data[numpart:,1]/ data[numpart:,0])
	#vPhi = np.arctan(np.sqrt(data[numpart:,0] ** 2 + data[numpart:,1] ** 2) /  data[numpart:,2])
	vPhi = np.arccos(data[numpart:,2] /  vRad)
		
	vt = np.sqrt(vTheta **2 + vPhi **2)
	#print(vt)
	vtMax = np.max(vt)

	outMaxVelRad.write("%E\n" % vrMax)
	outMaxVelTan.write("%E\n" % vtMax)
	




outMaxVelRad.close()
outMaxVelTan.close()
outCMDif.close()
outAM.close()



