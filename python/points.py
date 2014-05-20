import pexpect,re
import os.path
from const import outFolderName, modelname, numModels, modelFolderName, noraFolderName

RM = 200



def getPoints(child):
	#child.sendline("bodsreloc 725001 990000 550000")
	#child.expect ('nora>>')
	#child.sendline("bodsrange 1 814999")
	#child.expect ('nora>>')
	child.sendline("points %d" % RM)
	child.expect ('nora>>')



child = pexpect.spawn(os.path.join(noraFolderName,'nora'),  cwd=modelFolderName)
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
