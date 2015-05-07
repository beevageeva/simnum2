import pexpect,re,os
import os.path
from common import createFolder
from const2 import outFolderName, modelname, numModels, modelFolderName, noraFolderName


RM = 1000


files = {}
outFolder = createFolder(outFolderName)


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

cdir = os.getcwd()
#print("CWD=%s"%cdir)
os.chdir(modelFolderName)
#child = pexpect.spawn(os.path.join(noraFolderName,'nora'),  cwd=modelFolderName)
child = pexpect.spawn(os.path.join(noraFolderName,'nora'))
child.expect("nora>>")
child.sendline("data %s 1 %d 1" % (modelname, numModels))
child.expect("nora>>")
print(child.before)
for i in range(1,numModels + 1):
	print("*****************i=%d" %i)
	child.sendline("getmodel %d" % i)
	child.expect("getmodel>>.+nora>>")
	
	child.sendline("bodsrange 1 10000")
	child.expect ('nora>>')
	child.sendline("Rm %d"%RM)
	child.expect ('nora>>')
	getRad(child.before)
child.close()
os.chdir(cdir)
closeAllFiles()
