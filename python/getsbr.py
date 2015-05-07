import pexpect,re,os
import os.path
from common import createFolder
from const2 import outFolderName, modelname, numModels, modelFolderName, noraFolderName


RM = 1000


files = {}
outFolder = createFolder(outFolderName)



def closeAllFiles():
	for name in files:
		files[name].close()



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
	
	child.sendline("sbr %d 1" % RM)
	child.expect ('nora>>')
	getRad(child.before)
child.close()
os.chdir(cdir)
closeAllFiles()
