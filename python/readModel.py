import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt
from common import createFolder
import sys,re, os.path
from const import outFolderName, modelFolderName, numModels, fin1


outFolder = createFolder(outFolderName)

outMaxVelRadFile = "outMaxVelRad"
outMeanVelRadFile = "outMeanVelRad"
outMinVelRadFile = "outMinVelRad"
outMeanVelFile = "outMeanVel" 
outMinVelFile = "outMinVel" 
outMaxVelFile = "outMaxVel" 
outMaxVelTanFile = "outMaxVelTan"
outMinVelTanFile = "outMinVelTan"
outMeanVelTanFile = "outMeanVelTan"
outCMDifFile = "outCMDif"
outAMFile = "outAM"
outEPFile = "outEP"
outEKFile = "outEK"

outMaxVelRad = open(os.path.join(outFolder,outMaxVelRadFile), "w")
outMinVelRad = open(os.path.join(outFolder,outMinVelRadFile), "w")
outMeanVelRad = open(os.path.join(outFolder,outMeanVelRadFile), "w")
outMeanVel = open(os.path.join(outFolder,outMeanVelFile), "w")
outMinVel = open(os.path.join(outFolder,outMinVelFile), "w")
outMaxVel = open(os.path.join(outFolder,outMaxVelFile), "w")
outMaxVelTan = open(os.path.join(outFolder,outMaxVelTanFile), "w")
outMinVelTan = open(os.path.join(outFolder,outMinVelTanFile), "w")
outMeanVelTan = open(os.path.join(outFolder,outMeanVelTanFile), "w")
outCMDif = open(os.path.join(outFolder,outCMDifFile), "w")
outAM = open(os.path.join(outFolder,outAMFile), "w")
outEP = open(os.path.join(outFolder,outEPFile), "w")
outEK = open(os.path.join(outFolder,outEKFile), "w")


#change here and change in makemodels.py
#REL_FOLDER_NAME = "outModel"
REL_FOLDER_NAME = "om"
outmodelsuffix = "TREEBOD-"


def mod2(v3d):
	return v3d[:,0] ** 2 + v3d[:,1] ** 2 + v3d[:,2] ** 2


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

	ep = 0
	with open(filename, 'r') as f:
		for k in range(numpart+1):
			line = f.readline()
			if(k>1):
				ep += float(line.strip())
	
	data = np.loadtxt(filename, skiprows=1+numpart)
	print(data.shape)
	radius = np.sqrt(data[0:numpart,0] ** 2 + data[0:numpart,1] ** 2 + data[0:numpart,2] ** 2)
	npAnt = 0
	cm1 = np.array([0.0,0.0,0.0])
	cm2 = np.array([0.0,0.0,0.0])
	am = np.array([0.0,0.0,0.0])
	ek=0
	for k in range(fin1):
		npr = partTypes[k][0]	
		mp = partTypes[k][1]
		cm1 += mp * np.sum(data[npAnt:npr,:], axis=0)
		am+=mp * np.sum(np.cross(data[npAnt:npr,:], data[numpart+npAnt:numpart+npr,:]), axis = 0)
		ek += mp *  np.sum(mod2(data[numpart+npAnt:numpart+npr,:]))
		npAnt = npr
	for k in range(fin1, len(partTypes)):
		npr = partTypes[k][0]	
		mp = partTypes[k][1]
		cm2 += mp * np.sum(data[npAnt:npr,:], axis=0)
		am+=mp * np.sum(np.cross(data[npAnt:npr,:], data[numpart+npAnt:numpart+npr,:]), axis = 0)
		ek += mp *  np.sum(mod2(data[numpart+npAnt:numpart+npr,:]))
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
	outMinVelRad.write("%E\n" % np.min(vRad))
	outMeanVelRad.write("%E\n" % np.mean(vRad))
	outMaxVelTan.write("%E\n" % vtMax)
	outMinVelTan.write("%E\n" %  np.min(vt))
	outMeanVelTan.write("%E\n" % np.mean(vt))
	mv = " ".join(map(str,np.mean(data[numpart:,:], axis=0)))
	outMeanVel.write("%s\n" % mv)
	outMaxVel.write("%s\n" % " ".join(map(str,np.max(data[numpart:,:], axis=0))))
	outMinVel.write("%s\n" % " ".join(map(str,np.min(data[numpart:,:], axis=0))))
	outEP.write("%s\n" % ep)
	outEK.write("%s\n" % ek)





outMaxVelRad.close()
outMinVelRad.close()
outMeanVelRad.close()
outMeanVel.close()
outMinVel.close()
outMaxVel.close()
outMaxVelTan.close()
outMinVelTan.close()
outMeanVelTan.close()
outCMDif.close()
outAM.close()
outEP.close()
outAM.close()
outEP.close()
outEK.close()



