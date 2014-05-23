import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt
import os.path

#startMinSearch = 3
startMinSearch = 0 #model fixed
startMaxSearch = 10 #model fixed
noutbod = 20

dt = 0.252 #conhalo
#dt = 0.1075#sinhalo

#folderName = "outCONHALO_0"
#folderName = "outCONSIN_0"
#folderName = "outSINCON_0"
#folderName = "outSINHALO_0"
#folderName = "outCONELLIPSE_0"
folderName = "outCONM1_2"
title = "chm1"

filename2=filename3=None
#use output from getDist.py
#filename = "distance"
#oytitle = "distance"
#for cm dif (see readModel.py)
filename = "outCMDif"
oytitle = "distance"
#for am (see readModel.py)
#filename = "outAM"
#oytitle = "am"
#mean vel
#filename = "outMeanVel"
#oytitle = "mean vel"
#filename2 = "outMinVel"
#filename3 = "outMaxVel"

oxtitle = "modelNumber"

data = np.loadtxt(os.path.join(folderName, filename))


vals = np.sqrt(data[:,0] ** 2 + data[:,1] ** 2 + data[:,2] ** 2)

argminv = np.argmin(vals[startMinSearch:])+ startMinSearch + 1
argmaxv = np.argmax(vals[startMaxSearch:])+ startMaxSearch + 1
minv = np.min(vals[startMinSearch:])
maxv = np.max(vals)

print(argminv)
print(minv)
print(argmaxv)
print(maxv)

plt.grid(True)
plt.plot(range(1, data.shape[0]+1), vals, color="b", label=filename[3:])

#plt.vlines(argmaxv, minv, maxv)
plt.xticks(list(plt.xticks()[0]) + [argminv])


plt.xlabel(oxtitle)
plt.ylabel(oytitle)
plt.title(title)

if filename2:
	data2 = np.loadtxt(os.path.join(folderName, filename2))
	vals2 = np.sqrt(data2[:,0] ** 2 + data2[:,1] ** 2 + data2[:,2] ** 2)
	plt.plot(range(1, data2.shape[0]+1), vals2, color="g", label=filename2[3:])
	plt.legend()
if filename3:
	data3 = np.loadtxt(os.path.join(folderName, filename3))
	vals3 = np.sqrt(data3[:,0] ** 2 + data3[:,1] ** 2 + data3[:,2] ** 2)
	plt.plot(range(1, data3.shape[0]+1), vals3, color="r", label=filename3[3:])
	plt.legend()


ax2 = plt.twiny()
ax2.set_xlabel("Time")
ax2.set_xlim(0,data.shape[0]*noutbod*dt)
plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.draw()
plt.show()


