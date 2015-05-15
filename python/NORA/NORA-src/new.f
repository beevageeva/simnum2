C*********************************************************************
C
		CHARACTER*60 FUNCTION usage(command)
C
C*********************************************************************

	IMPLICIT NONE
	CHARACTER*(*) command

	IF (command .EQ. 'data') THEN
		usage = 'data filename modelnum [model2 modinc]'
	ELSE IF (command .EQ. 'prof4') THEN
		usage = 'prof4 rootfn coordmax radinc'
	ELSE IF (command .EQ. 'multiprof4') THEN
		usage = 'multiprof4 rootfn coordmax radinc'
	ELSE IF (command .EQ. 'setview') THEN
		usage = 'setview theta phi'
	ELSE IF (command .EQ. 'rotc') THEN
		usage = 'rotc PA Rmax swidth'
	ELSE IF (command .EQ. 'addrotc') THEN
		usage = 'addrotc PA Rmax swidth'
	ELSE IF (command .EQ. 'bodsrange') THEN
		usage = 'bodsrange istart iend'
	ELSE IF (command .EQ. 'levels') THEN
		usage = 'levels cmin cmax nlev [LIN/LOG]'
	ELSE IF (command .EQ. 'contour') THEN
		usage = 'contour [cmin cmax nlev [LIN/LOG]]'
	ELSE IF (command .EQ. 'space') THEN
		usage = 'space coordmax'
	ELSE IF (command .EQ. 'points') THEN
		usage = 'points coordmax'
	ELSE IF (command .EQ. 'addspace') THEN
		usage = 'addspace coordmax'
	ELSE IF (command .EQ. 'vel') THEN
		usage = 'vel coordmax'
	ELSE IF (command .EQ. 'translate') THEN
		usage = 'translate dX dY dZ'
        ELSE IF (command.eq.'getlist') THEN
                usage = 'getlist listfile'
	ELSE IF (command .EQ. 'addvel') THEN
		usage = 'addvel coordmax'
	ELSE IF (command .EQ. 'sbr') THEN
		usage = 'sbr coordmax'
	ELSE IF (command .EQ. 'dens') THEN
		usage = 'dens coordmin coordmax  (dmin dmax)'
	ELSE IF (command .EQ. 'densN') THEN
		usage = 'densN npbin (coordmax dmin dmax)'
	ELSE IF (command .EQ. 'ddens') THEN
		usage = 'ddens n_back coordmax (ddmin ddmax)'
	ELSE IF (command .EQ. 'velR') THEN
		usage = 'velR coordmax'
   	ELSE IF	(command.eq.'sphere') THEN
 		usage = 'sphere Rs'
	ELSE IF (command .EQ. 'velRJ') THEN
		usage = 'velRJ coordmax'
	ELSE IF (command .EQ. 'list') THEN
		usage = 'list pos/vel/mass/pot/header first last'
	ELSE IF (command .EQ. 'device') THEN
		usage = 'device outdev'
	ELSE IF (command .EQ. 'gray') THEN
		usage = 'gray min max'
	ELSE IF (command .EQ. 'velRT') THEN
		usage = 'velRT PA Rmax swidth'
	ELSE IF (command .EQ. 'innertia') THEN
		usage = 'innertia coordmax'
	ELSE IF (command .EQ. 'Rm') THEN
		usage = 'Rm coordmax'
	ELSE IF (command .EQ. 'outmodel') THEN
		usage = 'outmodel filename modelnumber'
	ELSE IF (command .EQ. 'edhead') THEN
		usage = 'edhead headfield headvalue'
	ELSE IF (command .EQ. '!') THEN
		usage = '! system_command'
	ELSE IF (command .EQ. 'macro') THEN
		usage = 'macro macrofile macroname'
	ELSE IF (command .EQ. 'bodsreloc') THEN
		usage = 'bodsreloc first last after'
	ELSE IF (command .EQ. 'medcent') THEN
		usage = 'medcent istart iend'
	ELSE IF (command .EQ. 'potcent') THEN
ccc		usage = 'potcent istart iend potfrac'
		usage = 'potcent potfrac'
	ELSE IF (command .EQ. 'Ebind') THEN
		usage = 'Ebind coordmin coordmax'
        ELSE IF (command.EQ.'EvsE') THEN
                usage=' EvsE model1 model2'
	ELSE
		PRINT '(A)', ' Unknown usage of ', command
	END IF

	usage = '.......usage:    '//usage

	RETURN
	END
