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

	INTEGER nbods, istart, iend, after, i, p1, p2

C--------Contour levels
	INTEGER maxlev, nlev
	PARAMETER (maxlev=25)
	REAL level(maxlev), cmin, cmax
	CHARACTER*3 ctype
C--------coordinate buffers
	REAL xbuff, ybuff, vbuff
	DIMENSION xbuff(nmax),ybuff(nmax),vbuff(nmax)

        CHARACTER*40 filename, outfile, macrofile, macroname, listfile
	INTEGER modelnum, model, model1, model2, modinc, outmodel,ierr
	INTEGER mclines, mcrem
	CHARACTER*4 keyw, coord*3
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

	REAL theta, phi, majfac, smooth, Rs
	REAL coordmin, coordmax, PA, Rmax, swidth, headvalue, potfrac
        REAL dmin(2), dmax(2), elow, eupp, emin, emax, d0
	INTEGER m, nviews, maxviews, headfield
	INTEGER ushow, status, SYSTEM
	PARAMETER (maxviews=50)
	REAL views(2,maxviews)

        
        REAL dx, dy, dz

	LOGICAL add

C----Definitions for the command parser
        INTEGER maxcmmds, maxpar
        INTEGER npar, lcom, npbin
        INTEGER strgeti, irest
	INTEGER ier, erpar
        PARAMETER (maxcmmds=16, maxpar=12)
	DIMENSION erpar(maxpar)
        REAL strgetr
        CHARACTER*80 inline
        CHARACTER*80 command, prevcom,  par(maxpar)
        CHARACTER*80 strgets
C-----Definitions for the manual facility
	CHARACTER*60 usage


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
	PRINT *,'		medcent		potcent		velR'
	PRINT *,'		setview		levels'
	PRINT *,'		bodsrange	bodsreloc    	velRJ'
	PRINT *,'		space		addspace'
	PRINT *,'		vel		addvel		translate'
	PRINT *,'		dens		sbr		Rm'
	PRINT *,'		innertia			Ebind'
	PRINT *,'		rotc		addrotc		EvsE'
        PRINT *,'		vdist		addvdist	ddens'
	PRINT *,'		prof4		multiprof4	densN'
	PRINT *,'		contour		gray		points'
	PRINT *,'		outFITS		outBDF		getlist' 
	PRINT *,'		edhead		outmodel	sphere' 
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
ccc	    IF (npar .GE. 4) THEN
ccc		model1 = modelnum
		model2 = strgeti(par(3), erpar(3))
		modinc = strgeti(par(4), erpar(4))
ccc	    END IF
	    IF (erpar(1)+erpar(2).NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
            END IF
ccc	    IF (npar .EQ. 2) THEN
     	    CALL getmodel(modelnum,filename,nbods)
              istart = 1
              iend   = nbods
ccc	    END IF

C--------------
C  Get sublist index array from file listfile
C--------------
         ELSE IF (command.eq.'getlist') THEN
             listfile = strgets(par(1), erpar(1))    
	    IF (erpar(1) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
            END IF
            call getlist(listfile,istart,iend)

C--------------
C  Cut sphere around origin of size Rs
C--------------
         ELSE IF (command.eq.'sphere') THEN
             Rs = strgetr(par(1), erpar(1))    
	    IF (erpar(1) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
            END IF
            call sphere(Rs,istart,iend)

C--------------
C  translate along vector  (dx, dy, dz)
C--------------
         ELSE IF (command.eq.'translate') THEN
             dx = strgetr(par(1), erpar(1))    
             dy = strgetr(par(2), erpar(2))    
             dz = strgetr(par(3), erpar(3))    
	    IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
            END IF
            call translate(dx,dy,dz,istart,iend)



C------------
C  Read in model 
C------------

	ELSE IF (command .EQ. 'getmodel') THEN
              modelnum=strgeti(par(1),erpar(1))
	      CALL getmodel(modelnum,filename,nbods)
	      istart = 1
	      iend   = nbods
        

C------------
C  Plot Ebind vs Ebind for two models
C------------
	ELSE IF (command .EQ. 'EvsE') THEN
              model1=strgeti(par(1),erpar(1))
              model2=strgeti(par(2),erpar(2))
              emin=strgetr(par(3),erpar(3))
             e max=strgetr(par(4),erpar(4))
              if(erpar(3)+erpar(4).ne.0)then
                emax=-1.
                emin=1.
              endif
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	      CALL EvsE(gdev,istart,iend,
     +    model1,model2,filename,emin,emax,infostr)
	      istart = 1
	      iend   = nbods



C------------
C  Median center a range of bodies
C------------

	ELSE IF (command .EQ. 'medcent') THEN
            istart = strgeti(par(1), erpar(1))
            iend   = strgeti(par(2),erpar(2))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                istart=1
                iend=nbods
cc                PRINT '(A)', usage(command)
cc                GO TO 120
            END IF
	    CALL medcntr(istart, iend)

C------------
C  Median center a range of bodies of low potential
C------------

	ELSE IF (command .EQ. 'potcent') THEN
ccc            istart = strgeti(par(2), erpar(2))
ccc            iend   = strgeti(par(3),erpar(3))
ccc            erpar(2)=0
ccc            erpar(3)=0
            potfrac = strgetr(par(1),erpar(1))
            IF (erpar(1) .NE. 0) THEN
               potfrac=-1.
ccc                PRINT '(A)', usage(command)
ccc                GO TO 120
            END IF
ccccccccccc  potcntr calculates median of all points within potlow and 
ccccccccccc  potlow(1-potfrac)
ccc	    CALL potcntr(istart, iend, potfrac)
ccccccccccc  potcenter calculates CoM of the potfrac*N points 
ccccccccccc  with lowest potential energy
            CALL POTCENTER(ISTART,IEND,POTFRAC)

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
     &           infostr)
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
     &           infostr)
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
     &           infostr)
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
     &           infostr)

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
     &           infostr)
C-----------
C  Select range of particles
C-----------
	ELSE IF(command.eq.'bodsrange')THEN
	istart	= strgeti(par(1), erpar(1))
	iend	= strgeti(par(2), erpar(2))
	IF (erpar(1) + erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
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
     &                                  theta, phi, infostr)

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
	    CALL dens(gdev, coordmax, istart, iend, infostr)

C-----------
C  Volume density profile, bins logarithmic in R
C  input maximum radius : COORDMAX
C  optional input min/max (log)density for plotting : DMIN DMAX
C-----------
	ELSE IF(command.eq.'dens') THEN
	    coordmin = strgetr(par(1), erpar(1))
	    coordmax = strgetr(par(2), erpar(2))
            dmin(1) = strgetr(par(3), erpar(3))
            dmin(2) = erpar(3)
            dmax(1) = strgetr(par(4), erpar(4))
            dmax(2) = erpar(4)
            IF (erpar(1)+erpar(2) .NE. 0) THEN
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

	    CALL dens(gdev, coordmin,coordmax, istart, iend, infostr, dmin, 
     1		dmax)


C------------
C  Volume density profile, fixed # particles per bin 
C    input # particles per bin : NPBIN
C    optional input maximum radius for plotting :  COORDMAX
C    optional input min/max (log)density : DMIN DMAX
C-----------
	ELSE IF(command.eq.'densN') THEN
	      npbin = strgetr(par(1), erpar(1))
           coordmax = strgetr(par(2), erpar(2))
            if(erpar(2).ne.0)coordmax=-1
            dmin(1) = strgetr(par(3), erpar(3))
            dmin(2) = erpar(3)
            dmax(1) = strgetr(par(4), erpar(4))
            dmax(2) = erpar(4)
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
	    CALL densn(gdev, npbin, coordmax, istart, iend, infostr, 
     1                dmin, dmax)


C-----------------
C   Volume density perturbation above background density 
C      input background density : D0
C      input maximum radius : COORDMAX
C optional input min/max density perturbation : DMIN DMAX
C----------------
	ELSE IF(command.eq.'ddens') THEN
		d0 = strgetr(par(1), erpar(1))
	    coordmax = strgetr(par(2), erpar(2))
            dmin(1) = strgetr(par(3), erpar(3))
            dmin(2) = erpar(3)
            dmax(1) = strgetr(par(4), erpar(4))
            dmax(2) = erpar(4)
            IF (erpar(1)+erpar(2) .NE. 0) THEN
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

	    CALL ddens(gdev, coordmax, istart, iend, infostr, dmin, dmax,
     1		d0)

C-----------
C  Volume radial velocity profile
C-----------
	ELSE IF(command.eq.'velR') THEN
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

	    CALL velr(gdev, coordmax, istart, iend, infostr)

C-----------
C  Radial velocity and angular momentum profiles
C-----------
	ELSE IF(command.eq.'velRJ') THEN
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

	    CALL velRJ(gdev, coordmax, istart, iend, infostr)

C-----------
C  Binding energy spectrum
C-----------
	ELSE IF(command.eq.'Ebind') THEN
	    coordmin = strgetr(par(1), erpar(1))
	    coordmax = strgetr(par(2), erpar(2))
            elow=strgetr(par(3),erpar(3))
            eupp=strgetr(par(4),erpar(4))
            IF (erpar(1) .NE. 0.or.erpar(2).ne.0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
            if(erpar(3)+erpar(4).ne.0)then
              eupp=-1.
              elow=1.
            endif
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    CALL ebind(gdev,coordmin, coordmax, elow, eupp, 
     1           istart, iend, infostr)


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
