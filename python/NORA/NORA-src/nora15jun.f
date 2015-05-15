C************************************************************************
C
C
			PROGRAM nora
C
C
C************************************************************************
C	Program to produce diagnostics on a PJQ-format N-body model.
C------------------------------------------------------------------------

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbods, istart, iend, ng1, n1, after, i, p1, p2

C--------Contour levels
	INTEGER maxlev, nlev
	PARAMETER (maxlev=25)
	REAL level(maxlev), cmin, cmax
	CHARACTER*3 ctype
C--------coordinate buffers
	REAL xbuff, ybuff, vbuff
	DIMENSION xbuff(nmax),ybuff(nmax),vbuff(nmax)

        CHARACTER*40 filename, outfile, macrofile, macroname
	INTEGER modelnum, model, model1, model2, modinc, outmodel,ierr
	INTEGER mclines, mcrem
	CHARACTER*4 keyw, coord*3, range*8
	CHARACTER*80 infostr(6)
	CHARACTER*10 gdev

C--------map arrays
	INTEGER imsize, newimsize
	PARAMETER (IMSIZE = 100)
	PARAMETER (newimsize=104)
	REAL map(imsize,imsize), map1(imsize,imsize)
	REAL newmap(newimsize,newimsize), newmap1(newimsize,newimsize)
	INTEGER inbin(imsize,imsize)
	INTEGER newinbin(newimsize,newimsize)

	REAL theta, phi, majfac, smooth
	REAL GRAVC, coordmax, PA, Rmax, swidth, headvalue, potfrac
	INTEGER m, nviews, maxviews, headfield
	INTEGER ushow, status, SYSTEM
	PARAMETER (maxviews=50)
	REAL views(2,maxviews)

	LOGICAL add

C----Definitions for the command parser
        INTEGER maxcmmds, maxpar
        INTEGER npar, lcom 
        INTEGER strgeti, irest
	INTEGER ier, erpar
        PARAMETER (maxcmmds=16, maxpar=12)
	DIMENSION erpar(maxpar)
        REAL strgetr
        CHARACTER*80 inline
        CHARACTER*80 command, prevcom,  par(maxpar)
        CHARACTER*80 strgets
C-----Definitions for the manual facility
	CHARACTER*80 usage


C---------------------------------------------------------------------
C---------
C   Start PGDISP graph server
C---------
	status = SYSTEM
     &	('/her1/users/balcells/pgsun/pgdisp -geometry 512x630+130+4 &')
	IF (status .NE. 0) PRINT *, 'Error starting PGDISP'
C----------
C  Initialize parameters
C----------
	GRAVC = 1.
	nbods = nmax
	istart = 1
	iend   = nbods
	smooth = 1.
	theta = 0.
	phi = 0.
	model1 = 1
	model2 = 1
	modinc = 999
	gdev = '/XDISP'


	PRINT *,' '
	PRINT *,'	     >>     NORA analysis tool for MODELS    <<'
	PRINT *,' '

110	PRINT *,' '	

	PRINT *,'nora>> MENU : '
	PRINT *,' '
	PRINT *,'		!		macro'
	PRINT *,'		?		menu		quit'
	PRINT *,'		show		list		device'
	PRINT *,'		data		getmodel'
	PRINT *,'		datar8		getmodelr8'
	PRINT *,'		cmcent		medcent		potcent'
	PRINT *,'		setview		levels'
	PRINT *,'		bodsrange	bodsreloc'
	PRINT *,'		space		addspace'
	PRINT *,'		vel		addvel		velRT'
	PRINT *,'		dens		sbr		Rm'
	PRINT *,'		innertia'
	PRINT *,'		rotc		addrotc'
        PRINT *,'		vdist		addvdist'
	PRINT *,'		prof4		multiprof4'
	PRINT *,'		contour		gray'
	PRINT *,'		points		manypts'
	PRINT *,'		outFITS		outBDF' 
	PRINT *,'		edhead		outmodel' 
	PRINT *,' '

C-------------
C  Reset, read in command line
C--------------
120	prevcom = command
	DO 122 i = 1, 80
		inline(i:i) = ' '
122	CONTINUE
	ier = 0
	mcrem = 0
	DO 125 i = 1, maxpar
	    erpar(maxpar) = 0
125	CONTINUE

	PRINT '((A)$)', ' nora>> '
        READ(5,'(A)') inline

C-------------
C  Parse inline, get command, parameters, no parameters
C-------------
126     CALL getsubstr (inline, command, lcom, irest, ier)
	IF (lcom .EQ. 0) GO TO 120
	CALL getpars(inline, irest, par, maxpar, npar, ier)

C------------
C  Start main command IF-block
C------------

C------------
C  Quit
C------------
	IF(command.eq.'q'.or.command.eq.'quit') THEN
		GO TO 990

C-----------
C  Escape to operating system
C-----------

	ELSE IF (command(1:1) .EQ. '!') THEN
	status = SYSTEM(inline(2:))
	IF (status .NE. 0) PRINT *, 'Error executing '//inline(1:65)
	
C-----------
C  Macro execution
C-----------

	ELSE IF (command .EQ. 'macro') THEN
	    macrofile = strgets(par(1), erpar(1))
	    macroname = strgets(par(1), erpar(1))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
		GO TO 120
            END IF
	    CALL domacro (macrofile, macroname, maxpar, erpar, 
	1		mclines, mcrem, 
	1		inline, command, prevcom)
	    GO TO 126
	    
C-----------
C  Menu; more manual facility
C-----------

	ELSE IF (command .EQ. '?') THEN
	    IF (npar .GT. 0) THEN
		PRINT '(A)', usage(par(1))
	    ELSE
		GO TO 110
	    END IF 


	ELSE IF (command .EQ. 'menu' .OR. command .EQ. 'MENU') THEN
		GO TO 110

C------------
C  Show current parameters
C------------
	ELSE IF (command .EQ. 'show') THEN
	    ushow = 6
	    CALL info(ushow, prevcom, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

C------------
C  List coordinates
C------------
	ELSE IF (command .EQ. 'list') THEN
	    coord = strgets(par(1), erpar(1))
	    p1    = strgeti(par(2), erpar(2))
	    p2    = strgeti(par(3), erpar(3))
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
		GO TO 120
            END IF
	    CALL list (coord, p1, p2)

C-----------
C  Select PGPLOT graphics device
C-----------
	ELSE IF (command .EQ. 'device') THEN
	    gdev = strgets(par(1), erpar(1))
	    IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
		GO TO 120
            END IF
	    IF (gdev(1:1) .EQ. '/' .OR. gdev(1:1) .EQ. '?') 
     &		CALL PGBEGIN(0, gdev, 1, 1)

C------------
C  Get filename, model(s)
C	If 2 parameters: filename + model
C	If 4 parameters: file + model1 + modelend + modinc (coadd models)
C------------
	ELSE IF (command .EQ. 'data') THEN
            filename = strgets(par(1), erpar(1))
            modelnum = strgeti(par(2),erpar(2))
	    model1    = modelnum
	    IF (npar .GE. 4) THEN
		model1 = modelnum
		model2 = strgeti(par(3), erpar(3))
		modinc = strgeti(par(4), erpar(4))
	    END IF
	    IF (erpar(1)+erpar(2)+erpar(3)+erpar(4) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
            END IF
	    IF (npar .EQ. 2) THEN
     	    CALL getmodel(modelnum,filename,nbods)
              istart = 1
              iend   = nbods
	      IF (ABS(header(8)) .GT. 1.e-15) GRAVC = header(8)
	      IF (ABS(header(21)) .GT. 1.E-5) ng1 = header(21) - 0.9
	    END IF

C------------
C  Get filename, model(s) double precision files
C	If 2 parameters: filename + model
C	If 4 parameters: file + model1 + modelend + modinc (coadd models)
C------------
	ELSE IF (command .EQ. 'datar8') THEN
            filename = strgets(par(1), erpar(1))
            modelnum = strgeti(par(2),erpar(2))
	    model1    = modelnum
	    IF (npar .GE. 4) THEN
		model1 = modelnum
		model2 = strgeti(par(3), erpar(3))
		modinc = strgeti(par(4), erpar(4))
	    END IF
	    IF (erpar(1)+erpar(2)+erpar(3)+erpar(4) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
            END IF
	    IF (npar .EQ. 2) THEN
     	    CALL getmodr8(modelnum,filename,nbods)
              istart = 1
              iend   = nbods
	      IF (ABS(header(8)) .GT. 1.e-15) GRAVC = header(8)
	      IF (ABS(header(21)) .GT. 1.E-5) ng1 = header(21) - 0.9
	    END IF

C------------
C  Read in model 
C------------
	ELSE IF (command .EQ. 'getmodel') THEN
	    IF (npar .GT. 0) THEN 
	      modelnum = strgeti(par(1),erpar(1))
	      IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
              END IF
	    END IF
	    CALL getmodel(modelnum,filename,nbods)
	    istart = 1
	    iend   = nbods
	    IF (ABS(header(8)) .GT. 1.e-15) GRAVC = header(8)
	    IF (ABS(header(21)) .GT. 1.E-5) ng1 = header(21) - 0.9

C------------
C  Read in model double precision
C------------

	ELSE IF (command .EQ. 'getmodelr8') THEN
	    IF (npar .GT. 0) THEN 
	      modelnum = strgeti(par(1),erpar(1))
	      IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
              END IF
	    END IF
	    CALL getmodr8(modelnum,filename,nbods)
	    istart = 1
	    iend   = nbods
	    IF (ABS(header(8)) .GT. 1.e-15) GRAVC = header(8)
	    IF (ABS(header(21)) .GT. 1.E-5) ng1 = header(21) - 0.9

C------------
C  CM center a range of bodies
C------------

	ELSE IF (command .EQ. 'cmcent') THEN
	  IF (npar .EQ. 2) THEN
            istart = strgeti(par(1), erpar(1))
            iend   = strgeti(par(2),erpar(2))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	  ELSE IF (npar .NE. 0) THEN
		PRINT '(A)', usage(command)
                GO TO 120
          END IF
	  CALL cmcntr(istart, iend)

C------------
C  Median center a range of bodies
C------------

	ELSE IF (command .EQ. 'medcent') THEN
	  IF (npar .EQ. 2) THEN
            istart = strgeti(par(1), erpar(1))
            iend   = strgeti(par(2),erpar(2))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	  ELSE IF (npar .NE. 0) THEN
		PRINT '(A)', usage(command)
                GO TO 120
          END IF
	  CALL medcntr(istart, iend)

C------------
C  Median center a range of bodies of low potential
C------------

	ELSE IF (command .EQ. 'potcent') THEN
	  IF (npar .EQ. 3) THEN
            istart = strgeti(par(1), erpar(1))
            iend   = strgeti(par(2),erpar(2))
            potfrac = strgetr(par(3),erpar(3))
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	  ELSE IF (npar .EQ. 1) THEN
	    potfrac = strgetr(par(1),erpar(1))
            IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	  ELSE
	    PRINT '(A)', usage(command)
	    GO TO 120
	  END IF
	  CALL potcntr(istart, iend, potfrac)

C------------
C  Output model 
C------------

	ELSE IF (command .EQ. 'outmodel') THEN
	    outfile	= strgets(par(1), erpar(1))
	    outmodel	= strgeti(par(2), erpar(2))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF

	    CALL m2head(pmass, header)
	    CALL xvpwrite
     1         (outmodel, outfile, header,
     1          x, y, z, vx, vy, vz, pmass, ppot,
     1          ierr)

C------------
C  Manual input header values
C------------

	ELSE IF (command .EQ. 'edhead') THEN
	    headfield	= strgeti(par(1), erpar(1))
	    headvalue	= strgetr(par(2), erpar(2))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    header(headfield) = headvalue

C------------
C  Renumber particles
C------------
 
        ELSE IF (command .EQ. 'bodsreloc') THEN
            istart      = strgeti(par(1), erpar(1))
            iend        = strgeti(par(2), erpar(2))
            after       = strgeti(par(3), erpar(3))
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
            CALL bodsreloc (istart, iend, after)
 
C-------------
C  Create disk FITS file from NEWMAP
C-------------
	ELSE IF (command .EQ. 'outFITS') THEN
	    CALL outFITS(newmap, newimsize, newimsize)

C---------------
C  Isophotal contour analysis of NEWMAP with PROF4
C---------------	
	ELSE IF(command .EQ. 'prof4') THEN
	    keyw =     strgets(par(1), erpar(1))
	    coordmax = strgetr(par(2), erpar(2))
	    majfac =   strgetr(par(3), erpar(3))
	    IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
		GO TO 120
	    END IF
	    CALL prof4(newmap,newimsize,newimsize,2,0.,majfac,
     &                 FLOAT(imsize/2),FLOAT(imsize/2),
     &                 0.2,0.,100,keyw)
	
C---------------
C  Isophotal contour analysis with PROF4 for a number of projections 
C  of the system.
C---------------	
	ELSE IF(command .EQ. 'multiprof4') THEN
            keyw =     strgets(par(1), erpar(1))
            coordmax = strgetr(par(2), erpar(2))
            majfac =   strgetr(par(3), erpar(3))
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
		GO TO 120
            END IF
	    CALL pickview(nviews, views)
	    DO 130 i = 1, nviews
		theta = views(1,i)
		phi   = views(2,i)
		CALL setmapzero(imsize, newimsize, map, newmap)
		CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	        CALL spacemap(istart,iend,xbuff,ybuff,imsize,coordmax,
     &                   smooth,newimsize,newmap,map,pmass)

		CALL prof4(newmap,newimsize,newimsize,2,0.,majfac,
     &                 FLOAT(imsize/2),FLOAT(imsize/2),
     &                 0.2,0.,100,keyw)
 130	    CONTINUE
	
C---------------
C  Select viewing angles
C---------------	
	ELSE IF(command .EQ. 'setview') THEN
	    theta = strgetr(par(1), erpar(1))
	    phi   = strgetr(par(2), erpar(2))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
		PRINT '(A)', usage(command)
                GO TO 120
            END IF
	   IF (theta .LT. 0. .OR. theta .GT. 180.e0) THEN
	    WRITE(6,*)
	    WRITE(6,*) '>> Polar angle  0. < theta < 180. '
	    WRITE(6,*)
	    GO TO 120
	   ELSE IF (phi .LT. -180. .OR. phi .GT. 180.e0) THEN
	    WRITE(6,*)
	    WRITE(6,*) '>> Azimuth angle -180. < phi < 180.'
	    WRITE(6,*)
	    GO TO 120
	   END IF	

C--------------
C  Rotation curve, velocity distribution
C--------------
        ELSE IF (command .EQ. 'rotc') THEN
	    PA =      strgetr(par(1), erpar(1))
	    Rmax =    strgetr(par(2), erpar(2))
	    swidth  = strgetr(par(3), erpar(3))
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    add = .FALSE.
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

            CALL rotc
     &           (gdev, nbods, istart, iend, theta, phi, 
     &		 PA, Rmax, swidth, 
     &		 filename, model1, model2, modinc, add, 0,
     &           GRAVC, infostr)
        ELSE IF (command .EQ. 'addrotc') THEN
	    add = .TRUE.
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

            CALL rotc
     &           (gdev, nbods, istart, iend,  theta, phi, 
     &		 filename, model1, model2, modinc, add, 0,
     &           GRAVC, infostr)
        ELSE IF (command .EQ. 'vdist') THEN
	    add = .FALSE.
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

            CALL rotc
     &           (gdev, nbods, istart, iend,  theta, phi, 
     & 		 filename, model1, model2, modinc, add, 1,
     &           GRAVC, infostr)
        ELSE IF (command .EQ. 'addvdist') THEN
	    add = .TRUE.
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

            CALL rotc
     &           (gdev, nbods, istart, iend,  theta, phi, 
     & 		 filename, model1, model2, modinc, add, 1,
     &           GRAVC, infostr)

C--------------
C  Profile of tangential velocity
C--------------
        ELSE IF (command .EQ. 'velRT') THEN
	    PA =      strgetr(par(1), erpar(1))
	    Rmax =    strgetr(par(2), erpar(2))
	    swidth  = strgetr(par(3), erpar(3))
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    add = .FALSE.
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

            CALL velrt
     &           (gdev, nbods, istart, iend, theta, phi, 
     &		 PA, Rmax, swidth, 
     &		 filename, model1, model2, modinc, add, 0,
     &           GRAVC, infostr)
C-----------
C  Select range of particles
C-----------
	ELSE IF(command.eq.'bodsrange')THEN
	  range	= strgets(par(1), erpar(1))
	  IF (erpar(1) .EQ. 0) THEN
	    IF (npar .GE. 2) THEN
		n1 = strgeti(par(2), erpar(2))
		IF (erpar(2) .NE. 0) THEN
		   PRINT '(A)', usage(command)
		   GO TO 120
		END IF
	    ELSE 
		n1 = ng1
	    END IF
	    IF (par(1)(1:3) .EQ. 'all') THEN
		istart = 1
		iend   = nbods
	    ELSE IF (par(1)(1:2) .EQ. 'up') THEN
		istart = n1 + 1
		iend   = nbods
	    ELSE IF (par(1)(1:3) .EQ. 'low') THEN
		istart = 1
		iend   = n1
	    ELSE
		PRINT '(A)', usage(command)
		GO TO 120
	    END IF
	  ELSE
	    istart= strgeti(par(1), erpar(1))
	    iend  = strgeti(par(2), erpar(2))
	    IF (erpar(1) + erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
            END IF
	  END IF

C--------------
C  Select contour levels, default linear spacing
C--------------
	ELSE IF(command.eq.'levels')THEN
	   cmin	= strgetr(par(1), erpar(1))
	   cmax = strgetr(par(2), erpar(2))
	   nlev = strgeti(par(3), erpar(3))
	   IF (npar .GT. 3) THEN
	        ctype = strgets(par(4), erpar(4))
	   ELSE 
		ctype = 'LIN'
	   END IF
	   IF (erpar(1)+erpar(2)+erpar(3)+erpar(4) .NE. 0) then
                PRINT '(A)', usage(command)
                GO TO 120
	   END IF
	   CALL setlevels(cmin, cmax, nlev, ctype, level, maxlev)

C----------------
C  Contour plot, try to use predefined levels
C----------------
	ELSE IF (command .EQ. 'contour') THEN
	    IF (npar .GT. 0) THEN
		cmin = strgetr(par(1), erpar(1))
		cmax = strgetr(par(2), erpar(2))
                nlev = strgeti(par(3), erpar(3))
                IF (npar .GT. 3) THEN
                     ctype = strgets(par(4), erpar(4))
                ELSE 
                     ctype = 'LIN'
                END IF
                IF (erpar(1)+erpar(2)+erpar(3)+erpar(4) .NE. 0) then
                     PRINT '(A)', usage(command)
                     GO TO 120
                END IF
		CALL setlevels(cmin, cmax, nlev, ctype, level, maxlev)
	    END IF
	    IF (nlev .EQ. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	    END IF

	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    CALL contplot(gdev, map, imsize, coordmax, level, nlev,
     &			  infostr)

C---------
C  Grayscale plot
C---------
	ELSE IF (command .EQ. 'gray') THEN
	    IF (npar .GT. 0) THEN
		cmin = strgetr(par(1), erpar(1))
                cmax = strgetr(par(2), erpar(2))
                IF (erpar(1)+erpar(2) .NE. 0) then
                     PRINT '(A)', usage(command)
                     GO TO 120
                END IF
            END IF
            IF (nlev .EQ. 0 .AND. npar .EQ. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    CALL grayplot(gdev, map, imsize, coordmax, cmin, cmax,
     &			  infostr)

C-------------
C  particle distribution points plot
C-------------
	ELSE IF(command.eq.'points')THEN
	    coordmax = strgetr(par(1), erpar(1))
	    IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
            CALL info(0, command, gdev, filename,
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    CALL ptplot(gdev, istart, iend, theta, phi, coordmax,
     &			infostr)

C-------------
C  particle distribution points plot, many on a page
C-------------
	ELSE IF(command.eq.'manypts')THEN
	    coordmax = strgetr(par(1), erpar(1))
	    IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
            CALL info(0, command, gdev, filename,
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

            CALL manypts(model1, model2, modinc,
     &                  filename,
     &                  gdev, istart, iend, theta, phi, coordmax,
     &                  infostr)

C-------------
C  Surface density map
C-------------
	ELSE IF(command.eq.'space')THEN
	   coordmax = strgetr(par(1), erpar(1))
	   IF (erpar(1) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	   END IF
	   CALL setmapzero(imsize, newimsize, map, newmap)

           CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	   CALL spacemap(istart,iend,xbuff,ybuff,imsize,coordmax,
     &                   smooth,newimsize,newmap,map,pmass)

	ELSE IF(command.eq.'addspace')THEN
	   coordmax = strgetr(par(1), erpar(1))
	   IF (erpar(1) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	   END IF
	   CALL setmapzero(imsize, newimsize, map, newmap)
	   DO 70 m = model1, model2, modinc
	      model = m
	      CALL getmodel(model,filename,header,nbods,
     &                      x,y,z,vx,vy,vz,pmass)

              CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	      CALL spacemap(istart,iend,xbuff,ybuff,imsize,coordmax,
     &                   smooth,newimsize,newmap1,map1,pmass)

	      CALL coadd(imsize, newimsize, map1, newmap1, map, newmap)
 70        CONTINUE

C------------
C  Velocity map
C------------
	ELSE IF(command.eq.'vel')THEN
	   coordmax = strgetr(par(1), erpar(1))
	   IF (erpar(1) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	   END IF
	   CALL setmapzero(imsize, newimsize, map, newmap)

	   CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	   CALL velmap(istart,iend,xbuff,ybuff,vbuff,
     &                        imsize,coordmax,smooth,newimsize,
     &                        inbin,newinbin,newmap,map,pmass)


	ELSE IF(command.eq.'addvel')THEN
	   coordmax = strgetr(par(1), erpar(1))
	   IF (erpar(1) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	   END IF
	   CALL setmapzero(imsize, newimsize, map, newmap)
	   DO 80 m = model1, model2, modinc
	      model = m
	      CALL getmodel(model,filename,header,nbods,
     &                      x,y,z,vx,vy,vz,pmass)

	      CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	      CALL velmap(istart,iend,xbuff,ybuff,vbuff,
     &                        imsize,coordmax,smooth,newimsize,
     &                        inbin,newinbin,newmap1,map1,pmass)

	      CALL coadd(imsize, newimsize, map1, newmap1, map, newmap)
 80        CONTINUE

C-----------
C  Surface density profile
C-----------
	ELSE IF(command.eq.'sbr') THEN
	    coordmax = strgetr(par(1), erpar(1))
            IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    CALL sbr(gdev, coordmax, istart, iend,
     &                                  theta, phi, GRAVC, infostr)

C-----------
C  Rm and total mass
C-----------
	ELSE IF(command.eq.'Rm') THEN
	    coordmax = strgetr(par(1), erpar(1))
            IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    IF (gdev(1:4) .EQ. 'file' .OR. gdev(3:6) .EQ. 'file') THEN
		gdev = 'Rmfile'
	    ELSE
		gdev = 'Rmterm'
	    END IF
	    CALL dens(gdev, coordmax, istart, iend, GRAVC, infostr)

C-----------
C  Volume density profile
C-----------
	ELSE IF(command.eq.'dens') THEN
	    coordmax = strgetr(par(1), erpar(1))
            IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    CALL dens(gdev, coordmax, istart, iend, GRAVC, infostr)

	ELSE IF(command.eq.'innertia') THEN
	    coordmax = strgetr(par(1), erpar(1))
            IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)
	    CALL intens(istart, iend, coordmax)

	ELSE
		print *,'nora>> Unknown Option <<'
	END IF

C---- If running macro, execute next line; otherwise, go to prompt

	IF (mcrem .GT. 0) THEN
	    CALL domacro (macrofile, macroname, maxpar, erpar, 
     1               mclines, mcrem,
     1               inline, command, prevcom)
	    GO TO 126
	ELSE
	    GOTO 120
	END IF

 990	CLOSE(unit=99)
CCC	status = SYSTEM ('kill -9 %pgdisp')

 999	END
