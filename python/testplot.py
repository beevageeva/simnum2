import numpy as np
import matplotlib.pyplot as plt

A = 1

def getPhi(r,B):
	return -A*B* np.exp(-r/B)*(-B - 2.0 * B**2/r)
	

def getVc(r,B):
	#exponential
	#return np.sqrt(np.absolute(-A*B*1.0/r * np.exp(-r/B)*(2*B**2 + 2.0 * B*r + r**2)))
	#hernquist
	#return np.sqrt(np.absolute(-A*B*r/(r+B) * np.log(B+r)))
	#hernquist2
	#return np.sqrt(np.absolute(-A*B*(B+2*r)/(2*r*(r+B)**2) ))
	return np.sqrt(A/B * r/B * np.log(B/r) )
	

#r = np.arange(0.1, 10., 0.1)
r = np.arange(0, 10., 0.01)

# red dashes, blue squares and green triangles
plt.plot(r, getVc(r,1.0))
plt.plot(r, getVc(r,2.0), "g")
plt.plot(r, getVc(r,3.0), "y")
plt.draw()
plt.show()
