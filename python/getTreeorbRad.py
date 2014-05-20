import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("TREEORB",  usecols=(0,1,2,3,4))
r = np.sqrt(data[:,1] ** 2 + data[:,2] ** 2 + data[:,3] ** 2)
plt.plot(data[:,0], r)
plt.draw()
plt.show()


