import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt

import sys

data = np.loadtxt("distances.txt")
plt.plot(range(1, data.shape[0]+1), data)
plt.xlabel("modelNumber")
plt.ylabel("distance")
print(np.argmin(data[3:])+4)

print(np.min(data[3:]))

plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.draw()
plt.show()


