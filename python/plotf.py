import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt
import os.path
import sys

#outFolder = "outCONHALO_0" 
outFolder = "outCONHALO_1" 

files = ["0.01", "0.50", "0.80", "0.99"]


for f in files:
	data = np.loadtxt(os.path.join(outFolder,f))
	plt.plot(data[:,0], data[:,1], label=f)

plt.xlabel("modelNumber")
plt.ylabel("radius")


plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.draw()
plt.show()


