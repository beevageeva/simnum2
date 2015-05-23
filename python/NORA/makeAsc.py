import pexpect,re,sys
import os.path



binModelFilename = "TREEBOD"
outPrefixPattern="ModelREPLACENUMBER.txt"

numModels = -1
import getopt
try:
        opts, args = getopt.getopt(sys.argv[1:], "", ["numModels="])
except getopt.GetoptError as err:
        # print help information and exit:
        print(str(err)) # will print something like "option -a not recognized"
        sys.exit(2)
timeEnd = None
for o, a in opts:
	if o in("--numModels"):
		numModels = int(a)
	else:
		print("option %s not recognized " % o)
if numModels == -1:
	print("usage: python makeAsc.py --numModels=<NUMMODELS>" )
	sys.exit(2)
print("numModels = %d " % numModels)

for modelNumber in range(numModels):
	print("NumModel %d" % modelNumber)
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


