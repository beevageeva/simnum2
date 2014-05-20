import pexpect,re
import os.path
from const import outFolderName, modelname, numModels, modelFolderName, noraFolderName
import os

RM = 200



def getPoints(child):
	#child.sendline("bodsreloc 725001 990000 550000")
	#child.expect ('nora>>')
	#child.sendline("bodsrange 1 814999")
	#child.expect ('nora>>')
	child.sendline("points %d" % RM)
	child.expect ('nora>>')

cdir = os.getcwd()
#print("CWD=%s"%cdir)
os.chdir(modelFolderName)
#child = pexpect.spawn(os.path.join(noraFolderName,'nora'),  cwd=modelFolderName)
child = pexpect.spawn(os.path.join(noraFolderName,'nora'))
child.expect("nora>>")
child.sendline("data %s 1 %d 1" % (modelname,numModels))
child.expect("nora>>")
print(child.before)
for i in range(1,numModels+1):
	print("*****************i=%d" %i)
	child.sendline("getmodel %d" % i)
	child.expect("getmodel>>.+nora>>")
	getPoints(child)	

child.close()
os.chdir(cdir)
