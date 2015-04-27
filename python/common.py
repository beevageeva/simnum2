#creates an output directory called out_0, out_1, ... the first that does not exists
def createFolder(dirname_base="out"):
  import os
  dirExists = True
  i = 0
  dirname = "%s_%i" % (dirname_base, i)
  while os.path.exists(dirname):
    i +=1
    dirname = "%s_%i" % (dirname_base, i)
  os.mkdir(dirname)
  return dirname

