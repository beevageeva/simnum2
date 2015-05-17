import numpy as np



"""
objNumber = 0(first object), 1(second object), 2(both objects)
"""
def getAngMom(objNumber):
	filename = "TREEAM"
	dt = 0.1146314
	noutbod = 100
	data = np.loadtxt(filename, usecols=(0, 1+3*objNumber, 2+3*objNumber, 3+3*objNumber))
	return [data[:,0]/(dt*float(noutbod)),  np.sqrt(data[:,1]**2 + data[:,2]**2 + data[:,3]**2)	]
