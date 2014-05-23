import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt

from const import outFolderName, modelFolderName, numModels, fin1
import sys,re,os.path

#modelNumber = 1
#modelNumber = 265
modelNumber = 177


#change here and change in makemodels.py
REL_FOLDER_NAME = "outModel"
outmodelsuffix = "TREEBOD-"
filename = os.path.join(modelFolderName, REL_FOLDER_NAME, "%s%d.txt" % (outmodelsuffix,modelNumber))


RM = 100

with open(filename, 'r') as f:
	first_line = f.readline()
rexpr = re.compile("\s*([\d.E\+-]+)(?:\s+[\d.E\+-]+)+\s*")
g = rexpr.match(first_line)

numpart = int(float((g.group(1))))
data = np.loadtxt(filename, skiprows=1+numpart)
print(data.shape)
radius = np.sqrt(data[0:numpart,0] ** 2 + data[0:numpart,1] ** 2 + data[0:numpart,2] ** 2)

mask = radius <= RM

vr = np.sqrt(data[numpart:,0] ** 2 + data[numpart:,1] ** 2 + data[numpart:,2] ** 2)

#theta = arctan(y/x)
#phi = arctan(sqrt(x**2+y**2)/z)

vTheta = np.arctan(data[numpart:,1]/ data[numpart:,0])
#vPhi = np.arctan(np.sqrt(data[numpart:,0] ** 2 + data[numpart:,1] ** 2) /  data[numpart:,2])
vPhi = np.arccos(data[numpart:,2] /  vr)

vt = np.sqrt(vTheta **2 + vPhi **2)


print(radius.shape)
print(vr.shape)

plt.plot(radius[mask], vr[mask], 'bo', markersize=1)

#plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.figure(2)
plt.plot(radius[mask], vt[mask], 'bo', markersize=1)
plt.figure(3)
plt.plot(radius[mask], vTheta[mask], 'bo', markersize=1)
plt.figure(4)
plt.plot(radius[mask], vPhi[mask], 'bo', markersize=1)
plt.figure(5)
plt.plot(data[0:numpart,0], data[numpart:,0], 'bo', markersize=1)
plt.figure(6)
plt.plot(data[0:numpart,1], data[numpart:,1], 'bo', markersize=1)
plt.figure(7)
plt.plot(data[0:numpart,2], data[numpart:,2], 'bo', markersize=1)

#6phase space projection on 02
#calculate mean of reshaped array
def reshapeArray(ar):
	print("ARSHAPE")
	print(ar.shape)
	newSize = 1000
	axisn = len(ar.shape)
	newShape = [newSize]
	for i in range(1,len(ar.shape)):
		newShape.append(ar.shape[i])
	newShape.append(ar.shape[0]/newSize)
	print("NEWSHAPE")
	print(newShape)
	ar = ar.reshape(tuple(newShape))
	print(ar.shape)
	return ar.mean(axis=axisn)


posxresh = reshapeArray(data[0:numpart,0])
poszresh = reshapeArray(data[0:numpart,1])
velxresh = reshapeArray(data[numpart:,0])
velzresh = reshapeArray(data[numpart:,1])

#X, Y = np.meshgrid(posxresh, poszresh)
#U, V =  np.meshgrid(velxresh, velzresh) 

plt.figure(8)
#M = np.sqrt(pow(U, 2) + pow(V, 2))
#Q = plt.quiver( X, Y, U, V, M, units='x', pivot='tip', width=0.022, scale=1/0.15)
#qk = plt.quiverkey(Q, 0.9, 1.05, 1, r'$1 \frac{m}{s}$',   labelpos='E',fontproperties={'weight': 'bold'})
#plt.plot(X, Y, 'k.')
#plt.axis([-1, 7, -1, 7])

plt.plot(data[0:numpart,0], data[0:numpart,1], 'ko', markersize=1)
plt.plot(posxresh, poszresh, 'ro', markersize=3)



#plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.draw()
plt.show()


