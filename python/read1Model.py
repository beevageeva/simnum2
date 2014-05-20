import matplotlib
matplotlib.use('TkAgg')
import numpy as np
import matplotlib.pyplot as plt

import sys,re

filename = "esinhalo1.txt"
RM = 60

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
vPhi = np.arccos(data[numpart:,2] /  radius)

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

#plt.get_current_fig_manager().window.wm_geometry("1000x900+50+50")
plt.draw()
plt.show()


