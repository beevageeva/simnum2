import pexpect,re
import os.path, os
from const import modelFolderName, modelname, numModels, MF



#change here and change in readModel.py
#REL_FOLDER_NAME = "outModel"
REL_FOLDER_NAME = "om"
outmodelsuffix = "TREEBOD-"

if not os.path.exists(os.path.join(modelFolderName, REL_FOLDER_NAME)):
	os.makedirs(os.path.join(modelFolderName, REL_FOLDER_NAME))

for i in range(1,numModels+1):
	print("i=%d"%i)
	try:	
		child = pexpect.spawn(MF + '/util/util/xvp-asc')
		child.expect("BINtoASC>> Name of INPUT File : ")
		child.sendline("%s" % os.path.join(modelFolderName,modelname))
		child.expect("BINtoASC>> Model number : ")
		child.sendline("%d" % i)
		child.expect("BINtoASC>> Name of OUTPUT ASCII File : ")
		child.sendline(os.path.join(modelFolderName, REL_FOLDER_NAME,"%s%d.txt" % (outmodelsuffix,i)))
		child.expect(pexpect.EOF)
		child.close()
	except Exception as e:
		print("EXCEPTION")
		print(e)
		print("CHILD")	
		print(str(child))
