import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt
import os.path

startMinSearch = 3

folderName = "outCONHALO_0"

#use output from getDist.py
#filename = "distance"
#oytitle = "distance"
#for cm dif (see readModel.py)
filename = "outCMDif"
oytitle = "distance"
#for am (see readModel.py)
#filename = "outAM"
#oytitle = "am"

oxtitle = "modelNumber"

data = np.loadtxt(os.path.join(folderName, filename))


vals = np.sqrt(data[:,0] ** 2 + data[:,1] ** 2 + data[:,2] ** 2)

print(np.argmin(vals[startMinSearch:])+ startMinSearch + 1 )
print(np.min(vals[startMinSearch:]))
print(np.argmax(vals)+ 1)

plt.plot(range(1, data.shape[0]+1), vals)
plt.xlabel(oxtitle)
plt.ylabel(oytitle)

plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.draw()
plt.show()


