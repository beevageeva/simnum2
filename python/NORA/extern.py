import numpy as np



def getTreepar(keys):
	import re
	filename = "TREEPAR"
	res = {}
	with open(filename) as f:
		for line in f:
			for key in keys:
				m = re.search("(\S+)\s+%s" % key, line)
				if(m):
					res[key] = m.group(1)

	return res
	




"""
objNumber = 0(first object), 1(second object), 2(both objects)
"""
def getAngMom(objNumber):
	filename = "TREEAM"
	pars = getTreepar(["dtime", "noutbod"])
	dt = float(pars["dtime"])
	noutbod = float(pars["noutbod"])
	data = np.loadtxt(filename, usecols=(0, 1+3*objNumber, 2+3*objNumber, 3+3*objNumber))
	return [data[:,0]/(dt*noutbod),  np.sqrt(data[:,1]**2 + data[:,2]**2 + data[:,3]**2)	]

def getTreeorbDist():
	filename = "TREEORB"
	pars = getTreepar(["dtime", "noutbod"])
	dt = float(pars["dtime"])
	noutbod = float(pars["noutbod"])
	data = np.loadtxt(filename)
	return [data[:,0]/(dt*float(noutbod)),  np.sqrt( (data[:,1] - data[:,4])**2 + (data[:,2] - data[:,5])**2 + (data[:,3] - data[:,6])**2)	]

def getCenter(modelNumbers, indices1, indices2, str1, str2, str3):
	modelname = "TREEBOD"

	import pexpect,re

	#TODO this assumes no need for bodsreloc and modelNumbers in asc order
	firstInd1 = np.min(indices1)
	firstInd2 = np.min(indices2)
	lastInd1 = np.max(indices1)
	lastInd2 = np.max(indices2)

	print("***************firstInd1=%d,lastInd1=%d,firstInd2=%d,lastInd2=%d" % (firstInd1, lastInd1, firstInd2, lastInd2))
	res = np.zeros(len(modelNumbers))
	
	def getRad(s):
		#print("output start")
		#print(s)
		#print("output end")
		lines = s.split("\n")
		rexpr = re.compile("%s\s+:" % str1 + "\s+([\dE.\+-]+)" * 3)
		for l in lines:
			
			#print("Line is %s" %l)
			g = rexpr.search(l)
			if(g):
				#print("MATCHED")
				rx = float(g.group(1))
				ry = float(g.group(2))
				rz = float(g.group(3))
				return np.sqrt(rx**2+ry**2+rz**2)
	
	
	try:
		child = pexpect.spawn("./nora")
		#child.logfile = sys.stdout
		child.expect (['nora>>',pexpect.EOF])  
		#print("--------------------------------spawn")
		#print(child.before)
		#print("--------------------------------00")
		#print(child.after)
		#print("--------------------------------000")
		child.sendline("data %s %d %d 1" % (modelname, min(modelNumbers), max(modelNumbers)))
		#print("2")
		child.expect (['nora>>',pexpect.EOF])  
		#print("--------------------------------data")
		#print(child.before)
		#print("--------------------------------00")
		#print(child.after)
		#print("--------------------------------000")
		#print(child.before)
		for i in range(len(modelNumbers)):
			print("*****************i=%d" % modelNumbers[i])
			child.sendline("getmodel %d" % modelNumbers[i])
			child.expect(["getmodel>>.+nora>>", pexpect.EOF])
			#print("--------------------------------getmodel")
			#print(child.before)
			#print("--------------------------------00")
			#print(child.after)
			#print("--------------------------------000")
			child.sendline("bodsrange %d %d" % (firstInd1+1, lastInd1+1))
			child.expect (['nora>>',pexpect.EOF])  
			#print("--------------------------------bodsrange")
			#print(child.before)
			#print("--------------------------------00")
			#print(child.after)
			#print("--------------------------------000")
			
		
			child.sendline(str2)
			child.expect (["%s>>.+nora>>" % str3,pexpect.EOF])  
			#print("--------------------------------medcent")
			#print(child.before)
			#print("--------------------------------2")
			#print(child.after)
			#print("--------------------------------3")
			child.sendline("bodsrange  %d %d" % (firstInd2+1, lastInd2+1))
			child.expect (['nora>>',pexpect.EOF])  
			#print("--------------------------------0111")
			#print(child.before)
			#print("--------------------------------01222")
			child.sendline(str2)
			child.expect (["%s>>.+nora>>" % str3 ,pexpect.EOF])  
			#print("--------------------------------01133")
			#print(child.before)
			res[i] = getRad(child.after)
		child.close()
		return res
	except:
		print("Exception was thrown")
		print("debug information:")
		print(str(child))
		import sys, traceback
		traceback.print_exc(file=sys.stdout)

def getMedcent(modelNumbers, indices1, indices2):
	return getCenter(modelNumbers, indices1, indices2, "median position", "medcent", "medcntr")

def getCMcent(modelNumbers, indices1, indices2):
	return getCenter(modelNumbers, indices1, indices2, "cm position", "cmcent", "cmcntr")




def getTreelogE():
	import re
	filename = "TREELOG"
	#"e, ek, ep =  -3.244119287E-01  3.932141662E-01 -7.176260948E-01"
	ere = re.compile("e,\s+ek,\s+ep\s+=\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)")
	#time:   1.1463E+01
	tre = re.compile("time:\s+([0-9+-Ee.]+)")
	text=""
	with open(filename) as f:
		text = f.read()
	findRes = ere.findall(text)
	timeRes = tre.findall(text)
	
	print("FINDRES LEN %d TIMERES len %d" % (len(findRes), len(timeRes)))
	
	pars = getTreepar(["dtime", "noutbod"])
	dt = float(pars["dtime"])
	noutbod = float(pars["noutbod"])
	todiv = dt * noutbod

	res = np.zeros((len(findRes),3))
	modelRes = np.zeros(len(findRes))
	for i in range(len(findRes)):
		item = findRes[i]
		print("------------------")
		print(item)
		print(timeRes[i])
		print("------------------")
		res[i,0] = float(item[0])
		res[i,1] = float(item[1])
		res[i,2] = float(item[2])
		modelRes[i] = float(timeRes[i]) / todiv
	return modelRes, res







		
