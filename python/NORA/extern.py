import numpy as np



useCalcValues = True


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

def getAngMomAll():
	filename = "TREEAM"
	pars = getTreepar(["dtime", "noutbod"])
	dt = float(pars["dtime"])
	noutbod = float(pars["noutbod"])
	data = np.loadtxt(filename)
	return [data[:,0]/(dt*noutbod),  np.sqrt(data[:,1]**2 + data[:,2]**2 + data[:,3]**2), np.sqrt(data[:,4]**2 + data[:,5]**2 + data[:,6]**2), np.sqrt(data[:,7]**2 + data[:,8]**2 + data[:,9]**2) 	]

def getTreeorbDist():
	filename = "TREEORB"
	pars = getTreepar(["dtime", "noutbod"])
	dt = float(pars["dtime"])
	noutbod = float(pars["noutbod"])
	data = np.loadtxt(filename)
	return [data[:,0]/(dt*float(noutbod)),  np.sqrt( (data[:,1] - data[:,4])**2 + (data[:,2] - data[:,5])**2 + (data[:,3] - data[:,6])**2)	]




def getCMCenterAll(modelNumbers):
	from os.path import isfile
	if useCalcValues and isfile("ext_cmcentall.txt"):
		print("EXT CM center all: Using file ext_cmcentall.txt ")
		return np.loadtxt("ext_cmcentall.txt")

	modelname = "TREEBOD"

	import pexpect,re

	res = np.zeros(len(modelNumbers))
	
	def getRad(s):
		#print("output start")
		#print(s)
		#print("output end")
		lines = s.split("\n")
		rexpr = re.compile("cm position\s+:\s+([\dE.\+-]+)" * 3)
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
		child.sendline("data %s %d %d 1" % (modelname, min(modelNumbers), max(modelNumbers)))
		child.expect (['nora>>',pexpect.EOF])  
		for i in range(len(modelNumbers)):
			print("*****************i=%d" % modelNumbers[i])
			child.sendline("getmodel %d" % modelNumbers[i])
			child.expect(["getmodel>>.+nora>>", pexpect.EOF])
			child.sendline("cmcent")
			child.expect (["cmcntr>>.+nora>>" ,pexpect.EOF])  
			res[i] = getRad(child.after)
		child.close()
		if useCalcValues:
			print("EXT CM center all: Saving to file ext_cmcentall.txt ")
			np.savetxt("ext_cmcentall.txt", res)
		return res
	except:
		print("Exception was thrown")
		print("debug information:")
		print(str(child))
		import sys, traceback
		traceback.print_exc(file=sys.stdout)


#TODO this assumes no need for bodsreloc and modelNumbers in asc order
def getCenter(modelNumbers, firstInd1, lastInd1, firstInd2, lastInd2 , str1, str2, str3):


	if useCalcValues:
		savefilename = "ext_%s-%d-%d-%d-%d.txt" % (str2, firstInd1, firstInd2, lastInd1, lastInd2)	
		from os.path import isfile
		if isfile(savefilename):
			print("EXT getCenter: Using file %s " % savefilename)
			return np.loadtxt(savefilename)	

	modelname = "TREEBOD"

	import pexpect,re

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
			child.sendline("bodsrange %d %d" % (firstInd1, lastInd1))
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
			child.sendline("bodsrange  %d %d" % (firstInd2, lastInd2))
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
		if useCalcValues:
			print("EXT getCenter: Saving to file %s " % savefilename)
			np.savetxt(savefilename, res)
		return res
	except:
		print("Exception was thrown")
		print("debug information:")
		print(str(child))
		import sys, traceback
		traceback.print_exc(file=sys.stdout)

def getMedcent(modelNumbers, firstInd1, lastInd1, firstInd2, lastInd2):
	return getCenter(modelNumbers, firstInd1, lastInd1, firstInd2, lastInd2, "median position", "medcent", "medcntr")

def getCMcent(modelNumbers, firstInd1, lastInd1, firstInd2, lastInd2):
	return getCenter(modelNumbers, firstInd1, lastInd1, firstInd2, lastInd2, "cm position", "cmcent", "cmcntr")


def getTreelog3ValuesAndTimeToModel(regexp3Vals):
	import re
	filename = "TREELOG"
	ere = re.compile(regexp3Vals)
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








def getTreelogAM():
	#"amx, amy, amz =   1.962407678E-02 -6.682983786E-02  2.587036043E-02"
	return getTreelog3ValuesAndTimeToModel("amx,\s+amy,\s+amz\s+=\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)")


def getTreelogE():
	#"e, ek, ep =  -3.244119287E-01  3.932141662E-01 -7.176260948E-01"
	return getTreelog3ValuesAndTimeToModel("e,\s+ek,\s+ep\s+=\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)")


def getTreelogCMPos():
	#cmpos =  -8.104465485E+01  5.134593323E-02 -3.790937364E-01
	return getTreelog3ValuesAndTimeToModel("cmpos\s+=\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)")
	
def getTreelogCMVel():
	#cmvel =   1.913200617E-01  1.114703700E-04  5.081008421E-04
	return getTreelog3ValuesAndTimeToModel("cmvel\s+=\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)\s+([0-9+-Ee.]+)")

def getrm(modelNumbers, firstInd, lastInd):
	import pexpect,re,os
	import os

	modelname = "TREEBOD"	
	RM = 1000
	outFolder = "ext_rm-%d-%d" % (firstInd, lastInd)

	if os.path.exists(outFolder):
		print("Folder %s already exists , using files inside.." % outFolder)
		return outFolder
	os.mkdir(outFolder)	
	
	
	files = {}
	
	
	def writeToFile(name, string):
		if(not name in files):
			files[name] = open(os.path.join(outFolder,name), "w") 
		files[name].write(string)
	
	
	def closeAllFiles():
		for name in files:
			files[name].close()
	
	
	def getRad(s):
		print("output start")
		print(s)
		print("output end")
		lines = s.split("\n")
		mlexpr = re.compile("#FILE\s+%s\s+MODEL\s+(\d+)\s+\d+\s+\d+" % modelname)
		fexpr = re.compile("#FRM\s+:" + "\s+([\d.]+)" * 8)
		rexpr = re.compile("#Rm\s+:" + "\s+([\dE.\+-]+)" * 8)
		#FRM
		for l in lines:
			
			print("Line is %s" %l)
			g = mlexpr.match(l)
			if(g):
				numbermodel = g.group(1)
				print(numbermodel)
			else:
				g = fexpr.match(l)
				if(g):
					keys=g.groups()
					
				else:
					g = rexpr.match(l)
					if(g):
						vals=g.groups()
						for i in range(len(keys)):
							print("%s -> %s" % (keys[i], vals[i]))
							writeToFile(keys[i], "%s\t%s\n" % (numbermodel,vals[i]))
	
	child = pexpect.spawn("./nora")
	child.expect("nora>>")
	child.sendline("data %s %d %d 1" % (modelname, min(modelNumbers), max(modelNumbers)))
	child.expect("nora>>")
	print(child.before)
	for k in range(len(modelNumbers)):
		i = modelNumbers[k]
		print("*****************i=%d" %i)
		child.sendline("getmodel %d" % i)
		child.expect("getmodel>>.+nora>>")
		
		child.sendline("bodsrange %d %d" % (firstInd, lastInd))
		child.expect ('nora>>')
		child.sendline("Rm %d"%RM)
		child.expect ('nora>>')
		getRad(child.before)
	child.close()
	closeAllFiles()
	return outFolder



		
