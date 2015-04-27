import const

from subprocess import call
for t in const.modeltypes:
	const.modeltype = t
	call(["python", "readModel.py"])
