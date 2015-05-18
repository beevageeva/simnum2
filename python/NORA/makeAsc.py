import pexpect,re
import os.path



binModelFilename = "TREEBOD"
numModels = 50
outPrefixPattern="ModelREPLACENUMBER.txt"



for modelNumber in range(numModels):
	try:
		modelNumber+=1
		child = pexpect.spawn("./xvp-asc")
		child.expect ('BINtoASC>> Name of INPUT File :')  
		child.sendline("%s" % (binModelFilename))
		child.expect ('BINtoASC>> Model number :')  
		child.sendline("%s" % (modelNumber))
		child.expect ('BINtoASC>> Name of OUTPUT ASCII File :') 
		 
		child.sendline("%s" % (outPrefixPattern.replace("REPLACENUMBER", str(modelNumber))))
		child.expect ('outbods>> Writing ') 
		child.close()
	
	except:
		print("Exception was thrown")
		print("debug information:")
		print(str(child))


