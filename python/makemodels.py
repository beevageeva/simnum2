import pexpect,re
import os.path
from const import modelFolderName, modelname, numModels


files = {}

#change here and change in readModel.py
REL_FOLDER_NAME = "outModel"
outmodelsuffix = "TREEBOD-"


for i in range(1,numModels+1):
	print("i=%d"%i)
	try:	
		child = pexpect.spawn('../util/util/xvp-asc')
		child.expect("BINtoASC>> Name of INPUT File : ")
		child.sendline("%s" % modelname)
		child.expect("BINtoASC>> Model number : ")
		child.sendline("%d" % i)
		child.expect("BINtoASC>> Name of OUTPUT ASCII File : ")
		child.sendline(os.path.join(outFolderName, REL_FOLDER_NAME,"%s%d.txt" % (outmodelsuffix,i)))
		child.expect(pexpect.EOF)
		child.close()
	except:
		print("EXCEPTION")
		print(str(child))
