import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt

import sys,os.path

folderName = "outCONHALO_0"
#folderName = "outCONHALO_1"

#filename = "outMaxVelRad"
#oytitle = "velRad"
filename = "outMaxVelTan"
oytitle = "velTan"
#filename = "distances.txt"
#oytitle = "distance"

minstart = 3

data = np.loadtxt(os.path.join(folderName, filename))

oxtitle = "modelNumber"

plt.plot(range(1, data.shape[0]+1), data)
plt.xlabel(oxtitle)
plt.ylabel(oytitle)
print(np.argmin(data[minstart:])+minstart+1)

print(np.min(data[minstart:]))

plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.draw()
plt.show()


