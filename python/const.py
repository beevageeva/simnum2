#MF = "../cca/"
MF = "/scratch1/sn2cca/"


modeltypes = ["ch,sh,cs,sc,ce,cm"]

#folder paths can be absolute or relative, with or without trailing slash(using os.path.join)
modeltype = "cm"


if modeltype == "ch":
	#con halo:
	outFolderName = "outCONHALO"
	fin1 = 2
	#modelFolderName = "../PRAC1-conhalo"
	modelFolderName = MF + "PRAC1-conhalo"
	modelname = "TREEBOD_final" #el modelo final despues de poner en orbita y evolucionar
	#modelname = "TREEBOD_one" #para el modelo inicial relajado(solo 1 objeto)
	noraFolderName = "../nora/"  #if this is a relative path IT SHOULD BE RELATIVE TO modelFolderName NOT THIS
	numModels = 265 #modelo final
	#numModels = 1 #prueba
	#numModels = 11 #para el modelo incial relajado(solo 1 objeto)

elif modeltype == "sh":
	#sin halo:
	outFolderName = "outSINHALO"
	fin1 = 1
	modelFolderName = MF +  "PRACT1"
	modelname = "TREEBOD_final" #el modelo final despues de poner en orbita y evolucionar
	#modelname = "TREEBOD_one" #para el modelo inicial relajado(solo 1 objeto)
	noraFolderName = "../nora/"  #if this is a relative path IT SHOULD BE RELATIVE TO modelFolderName NOT THIS
	numModels = 292 #modelo final
	#numModels = 1 #prueba
	#numModels = 11 #para el modelo incial relajado(solo 1 objeto)

elif modeltype == "cs":
	#primero con halo, segundo sin halo:
	outFolderName = "outCONSIN"
	fin1 = 2
	modelFolderName = MF +  "PCS"
	modelname = "TREEBOD" #el modelo final despues de poner en orbita y evolucionar
	noraFolderName = "../nora/"  #if this is a relative path IT SHOULD BE RELATIVE TO modelFolderName NOT THIS
	numModels = 164 #modelo final
	#numModels = 1 #prueba

elif modeltype == "sc":
	#primero SIN halo, segundo sin halo:
	outFolderName = "outSINCON"
	fin1 = 1
	modelFolderName = MF + "PSC"
	modelname = "TREEBOD" #el modelo final despues de poner en orbita y evolucionar
	noraFolderName = "../nora/"  #if this is a relative path IT SHOULD BE RELATIVE TO modelFolderName NOT THIS
	numModels = 154 #modelo final
	#numModels = 1 #prueba

elif modeltype == "ce":
	#CON HALO los 2 elliptica
	outFolderName = "outCONELLIPSE"
	fin1 = 2
	modelFolderName = MF +  "PHE"
	modelname = "TREEBOD" #el modelo final despues de poner en orbita y evolucionar
	noraFolderName = "../nora/"  #if this is a relative path IT SHOULD BE RELATIVE TO modelFolderName NOT THIS
	numModels = 302 #modelo final
	#numModels = 1 #prueba

elif modeltype == "cm":
	outFolderName = "outCONM1"
	fin1 = 2
	modelFolderName = MF +  "PHM"
	modelname = "TREEBOD" #el modelo final despues de poner en orbita y evolucionar
	noraFolderName = "../nora/"  #if this is a relative path IT SHOULD BE RELATIVE TO modelFolderName NOT THIS
	numModels = 101 #modelo final
	#numModels = 1 #prueba
