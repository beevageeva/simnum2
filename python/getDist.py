import pexpect,re
import os.path
from math import sqrt

files = {}

outFolder = "out"
modelname = "TREEBOD"

numModels =  265

lastNumberFirst = 50000
lastNumberSecond = 100000

import time

wfile = open("distances.txt", "w")



def getRad(s):
	#print("output start")
	#print(s)
	#print("output end")
	lines = s.split("\n")
	rexpr = re.compile("median position\s+:" + "\s+([\dE.\+-]+)" * 3)
	for l in lines:
		
		#print("Line is %s" %l)
		g = rexpr.search(l)
		if(g):
			#print("MATCHED")
			rx = float(g.group(1))
			ry = float(g.group(2))
			rz = float(g.group(3))
			wfile.write("%E\n" % sqrt(rx**2+ry**2+rz**2))


try:
	#child = pexpect.spawn('../nora/nora', timeout=None)
	child = pexpect.spawn('../nora/nora')
	#child.logfile = sys.stdout
	child.expect (['nora>>',pexpect.EOF])  
	#print("--------------------------------spawn")
	#print(child.before)
	#print("--------------------------------00")
	#print(child.after)
	#print("--------------------------------000")
	child.sendline("data %s 1 %d 1" % (modelname, numModels))
	#print("2")
	child.expect (['nora>>',pexpect.EOF])  
	#print("--------------------------------data")
	#print(child.before)
	#print("--------------------------------00")
	#print(child.after)
	#print("--------------------------------000")
	#print(child.before)
	for i in range(1,numModels + 1):
		print("*****************i=%d" %i)
		child.sendline("getmodel %d" % i)
		child.expect(["getmodel>>.+nora>>", pexpect.EOF])
		#print("--------------------------------getmodel")
		#print(child.before)
		#print("--------------------------------00")
		#print(child.after)
		#print("--------------------------------000")
		child.sendline("bodsrange 1 %d" % lastNumberFirst)
		child.expect (['nora>>',pexpect.EOF])  
		#print("--------------------------------bodsrange")
		#print(child.before)
		#print("--------------------------------00")
		#print(child.after)
		#print("--------------------------------000")
		
	
		child.sendline("medcent")
		child.expect (['medcntr>>.+nora>>',pexpect.EOF])  
		#print("--------------------------------medcent")
		#print(child.before)
		#print("--------------------------------2")
		#print(child.after)
		#print("--------------------------------3")
		child.sendline("bodsrange  %d %d" % (lastNumberFirst+1, lastNumberSecond))
		child.expect (['nora>>',pexpect.EOF])  
		#print("--------------------------------0111")
		#print(child.before)
		#print("--------------------------------01222")
		child.sendline("medcent")
		child.expect (['medcntr>>.+nora>>',pexpect.EOF])  
		#print("--------------------------------01133")
		#print(child.before)
		if(i==86):
			print("--------------------------------01244")
			print(child.after)
			print("--------------------------------01244")
		getRad(child.after)
except:
	print("Exception was thrown")
	print("debug information:")
	print(str(child))


