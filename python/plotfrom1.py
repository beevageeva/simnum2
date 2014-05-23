import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt

import sys,os.path

#folderName = "outCONHALO_0"
#folderName = "outCONSIN_0"
#folderName = "outSINCON_0"
#folderName = "outSINHALO_0"
#folderName = "outCONELLIPSE_0"
folderName = "outCONM1_2"


filename2=filename3=None
#oytitle = "mean velRad"
#filename = "outMeanVelRad"
##filename2 = "outMinVelRad"
##filename3 = "outMaxVelRad"
#oytitle = "velTan"
#filename = "outMeanVelTan"
##filename2 = "outMinVelTan"
##filename3 = "outMaxVelTan"
#filename = "distances.txt"
#oytitle = "distance"
filename = "outEK"
filename2 = "outEP"
oytitle = "E"

#minstart = 3 #fixed
minstart = 0
maxstart = 10
noutbod = 20

dt = 0.252 #conhalo
#dt = 0.1075#sinhalo


def printDataStat(data):
	print(np.argmin(data[minstart:])+minstart+1)
	print(np.argmax(data[maxstart:])+maxstart+1)
	
	print(np.min(data[minstart:]))
	print(np.max(data[maxstart:]))

data = np.loadtxt(os.path.join(folderName, filename))

oxtitle = "modelNumber"

plt.grid(True)
plt.plot(range(1, data.shape[0]+1), data, color="b",label=filename[3:])
plt.xlabel(oxtitle)
plt.ylabel(oytitle)

printDataStat(data)
if filename2:
	data2 = np.loadtxt(os.path.join(folderName, filename2))
	printDataStat(data2)
	#for mean, min, max
	#plt.plot(range(1, data2.shape[0]+1), data2, color="g", label=filename2[3:])
	#for energy:
	p1 = data2/abs(np.min(data2))	
	plt.plot(range(1, data2.shape[0]+1), p1, color="r", label=filename2[3:])
	plt.plot(range(1, data2.shape[0]+1), data + p1, color="g", label="EK+EP")
	plt.plot(range(1, data2.shape[0]+1), 2*data + p1, color="k", label="2EK+EP")
	plt.legend()
if filename3:
	data3 = np.loadtxt(os.path.join(folderName, filename3))
	printDataStat(data3)
	plt.plot(range(1, data2.shape[0]+1), data3, color="r", label=filename3[3:])
	plt.legend()

plt.xticks(list(plt.xticks()[0]) + [63])
ax2 = plt.twiny()
ax2.set_xlabel("Time")
ax2.set_xlim(0,data.shape[0]*noutbod*dt)

plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.draw()
plt.show()


