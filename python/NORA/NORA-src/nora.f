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

	INTEGER nbods, istart, iend, ng1, n1, after, i, p1, p2, ibod
	INTEGER istart2,iend2,iempieza,ifin,imed,iuno,imid
	INTEGER imed1,imed2
C--------Contour levels
	INTEGER maxlev, nlev
	PARAMETER (maxlev=25)
	REAL level(maxlev), cmin, cmax
	CHARACTER*3 ctype
C--------coordinate buffers
	REAL xbuff, ybuff, vbuff, Vmax, sigma
	REAL xbuff1, ybuff1, vbuff1
	REAL xbuff2, ybuff2, vbuff2
	REAL xbuff3, ybuff3, vbuff3
	DIMENSION xbuff(nmax),ybuff(nmax),vbuff(nmax)
	DIMENSION xbuff1(nmax),ybuff1(nmax),vbuff1(nmax)
	DIMENSION xbuff2(nmax),ybuff2(nmax),vbuff2(nmax)
	DIMENSION xbuff3(nmax),ybuff3(nmax),vbuff3(nmax)
	REAL xnew, ynew, znew, vxnew, vynew, vznew, mnew,ang

        CHARACTER*40 filename, outfile, macrofile, macroname,listfile
	INTEGER modelnum, model, model1, model2, modinc, outmodel,ierr
	INTEGER mclines, mcrem, PGCURS, nx, ny,proy, modeli,modelf
	REAL worldx, worldy
	CHARACTER*4 keyw, coord*3, range*8, char*1
	CHARACTER*80 infostr(6)
	CHARACTER*10 gdev

	REAL sigma3,meanvt2

C--------map arrays
	INTEGER imsize, newimsize
	PARAMETER (IMSIZE = 100)
	PARAMETER (newimsize=104)
	REAL map(imsize,imsize), map1(imsize,imsize)
	REAL newmap(newimsize,newimsize), newmap1(newimsize,newimsize)
	INTEGER inbin(imsize,imsize)
	INTEGER newinbin(newimsize,newimsize)

	REAL theta, phi, majfac, smooth,eulert,euleri
	REAL GRAVC, coordmin, coordmax, scalel,PA, Rmax,xmax
	REAL swidth, headvalue, potfrac
        REAL dmin(2), dmax(2), elow, eupp, emin, emax
	INTEGER m, nviews, maxviews, headfield
	INTEGER ushow, status, SYSTEM
	PARAMETER (maxviews=100)
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
	CHARACTER*80 usage
	

C---------------------------------------------------------------------
C---------
C   Start PGDISP graph server
C---------
	status = SYSTEM
     &	('~/.noralinux/pgdisp -geometry 512x630+130+4 &')
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
	PRINT *,'		!		macro		cursor'
	PRINT *,'		?		menu		quit'
	PRINT *,'		show		list		device'
	PRINT *,'		data		getmodel        parti'
	PRINT *,'		datar8		getmodelr8'
	PRINT *,'		cmcent		medcent		potcent'
	PRINT *,'		setview		levels'
	PRINT *,'		bodsrange	bodsreloc'
	PRINT *,'		space		addspace        ring'
	PRINT *,'		vel		addvel		velRT'
	PRINT *,'		dens		densN		velR'
	PRINT *,'		sbr		Rm              zscale'
	PRINT *,'		innertia        ellipt          cambioeje'
	PRINT *,'		rotc		addrotc'
        PRINT *,'		vdist		addvdist'
	PRINT *,'		prof4		multiprof4'
	PRINT *,'		contour		gray            points4'
	PRINT *,'		points		manypts         pcolors'
	PRINT *,'		outFITS		outBDF          manypts2' 
	PRINT *,'		edhead		outmodel        ebneg' 
	PRINT *,'		Ebind		EvsE		getlist'
	PRINT *,'		translate	sphere          ebneg2'
	PRINT *,'		set'
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
	    erpar(i) = 0
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

C----------
C  Cursor read
C----------
	ELSE IF (command .EQ. 'c') THEN
		i = PGCURS (worldx, worldy, char)
		WRITE(6,*) worldx, worldy
	    
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
	ELSE IF (command .EQ. 'set') THEN
	    ibod = strgeti(par(1), erpar(1))
	    xnew = strgetr(par(2), erpar(2))
	    ynew    = strgetr(par(3), erpar(3))
	    znew    = strgetr(par(4), erpar(4))
	    vxnew = strgetr(par(5), erpar(5))
	    vynew    = strgetr(par(6), erpar(6))
	    vznew    = strgetr(par(7), erpar(7))
	    mnew     = strgetr(par(8), erpar(8))
            IF (erpar(1)+erpar(2)+erpar(3)+erpar(4)+erpar(5)+
     1          erpar(6)+erpar(7)+erpar(8) .NE. 0) THEN
                PRINT '(A)', usage(command)
		GO TO 120
            END IF
	    x(ibod)	= xnew
	    y(ibod)	= ynew
	    z(ibod)	= znew
	    vx(ibod)	= vxnew
	    vy(ibod)	= vynew
	    vz(ibod)	= vznew
	    pmass(ibod)	= mnew
	    ppot(ibod)	= -10

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
     &		CALL PGBEG(0, gdev, 1, 1)

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
C  Read in several models adding them up 
C------------
	ELSE IF (command .EQ. 'addmodel') THEN
	    IF (npar .GT. 0) THEN 
	      modeli = strgeti(par(1),erpar(1))
	      modelf = strgeti(par(2),erpar(2))
	      IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
              END IF
	    END IF
c	    CALL getmodel(modelnum,filename,nbods)
       PRINT*, 'getmodel>> model : ', model

        CALL xvpread(model,filename,header,
     1               x, y, z, vx, vy, vz, pmass, ppot, ierr)

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
C  In POTCENTER, POTFRAC=fraction of most bound (PPOT) particles 
C	taken for CofM calculation
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
CCCC	  CALL potcntr(istart, iend, potfrac)
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



c------------
c  Output model with desired particles
c------------

	ELSE IF (command .EQ. 'outmodel2') THEN
	    outfile	= strgets(par(1), erpar(1))
	    outmodel	= strgeti(par(2), erpar(2))
	    istart	= strgeti(par(3), erpar(3))
	    iend	= strgeti(par(4), erpar(4))
	    
        IF (erpar(1)+erpar(2)+erpar(3)+erpar(4) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF

	    CALL m2head(pmass, header)
	    CALL xvpwrite2
     1         (outmodel, outfile, header, istart, iend,
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
	    IF (iend .LT. istart) erpar(2) = 1
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
            CALL bodsreloc (istart, iend, after)
 
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
             coordmax = strgetr(par(1), erpar(1))
            IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
            call sphere(coordmax,istart,iend)

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
C-----------
C  Binding energy spectrum
C-----------
        ELSE IF(command.eq.'ex') THEN
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

            CALL ex(gdev,coordmin, coordmax, elow, eupp,
     1           istart, iend, infostr)
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
C-----------
C  Binding energy spectrum
C-----------
        ELSE IF(command.eq.'Ebindcol') THEN
            coordmin = strgetr(par(1), erpar(1))
            coordmax = strgetr(par(2), erpar(2))
	    iuno= strgetr(par(3), erpar(3))
	    imid= strgetr(par(4), erpar(4))
            elow=strgetr(par(5),erpar(5))
            eupp=strgetr(par(6),erpar(6))
            IF (erpar(1) .NE. 0.or.erpar(2).ne.0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
            if(erpar(5)+erpar(6).ne.0)then
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

            CALL ebindcol(gdev,coordmin, coordmax, elow, eupp,
     1           istart, iuno,imid,iend, infostr)
C-----------
C Only particles with negative binding energy
C-----------
        ELSE IF(command.eq.'ebneg') THEN
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

            CALL ebneg(gdev,coordmin, coordmax, elow, eupp,
     1           istart, iend, infostr)

C-----------
C Only particles with negative binding energy
C-----------
        ELSE IF(command.eq.'ebneg2') THEN
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

            CALL ebneg2(gdev,coordmin, coordmax, elow, eupp,
     1           istart, iend, infostr)

C--------------
C       Gives particles with masses ...
C--------------
	    ELSE IF(command.eq.'parti') THEN
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

            CALL parti(gdev,coordmin, coordmax, 
     1           istart, iend, infostr)
C------------
C  Plot Ebind vs Ebind for two models
C------------
        ELSE IF (command .EQ. 'EvsE') THEN
              model1=strgeti(par(1),erpar(1))
              model2=strgeti(par(2),erpar(2))
              emin=strgetr(par(3),erpar(3))
              emax=strgetr(par(4),erpar(4))
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
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
     1    model1,model2,filename,emin,emax,infostr)
              istart = 1
              iend   = nbods

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
     &                 0.2,0.,100,keyw,ang)
	
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
	    CALL pickview2(nviews, views)
	    open(17,file='Vmax.out',status='unknown')
	    DO 130 i = 1, nviews
		theta = views(1,i)
		phi   = views(2,i)
		CALL setmapzero(imsize, newimsize, map, newmap)
		print*,istart,iend,theta,phi
		CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	        CALL spacemap(istart,iend,xbuff,ybuff,imsize,coordmax,
     &                   smooth,newimsize,newmap,map,pmass)

		CALL prof42(newmap,newimsize,newimsize,2,0.,majfac,
     &                 FLOAT(imsize/2),FLOAT(imsize/2),
     &                 0.2,0.,100,keyw,ang)
		PA=ang
		Rmax=coordmax
		swidth=0.3
            CALL rotc4
     &           (gdev, nbods, istart, iend,  theta, phi, 
     &           PA, Rmax, swidth,
     &		 filename, model1, model2, modinc, add, 0,
     &           GRAVC, Vmax,sigma,infostr)
	    write(17,*) Vmax,sigma
 130	    CONTINUE
	    close(17)
	
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
	    PA =      strgetr(par(1), erpar(1))
	    Rmax =    strgetr(par(2), erpar(2))
	    swidth  = strgetr(par(3), erpar(3))
            IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
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
     &		 PA, Rmax, swidth, 
     &		 filename, model1, model2, modinc, add, 0,
     &           GRAVC, infostr)

        ELSE IF (command .EQ. 'vdist') THEN
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
     &           (gdev, nbods, istart, iend,  theta, phi, 
     &           PA, Rmax, swidth,
     & 		 filename, model1, model2, modinc, add, 1,
     &           GRAVC, infostr)

        ELSE IF (command .EQ. 'addvdist') THEN
	    PA =      strgetr(par(1), erpar(1))
	    Rmax =    strgetr(par(2), erpar(2))
	    swidth  = strgetr(par(3), erpar(3))
	    keyw =     strgets(par(4), erpar(4))
            IF (erpar(1)+erpar(2)+erpar(3)+erpar(4) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF
	    add = .TRUE.
	    CALL info(0, command, gdev, filename, 
     1                modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    xmax=Rmax

c	    open(17,file='Vmax.out',status='unknown')
	    CALL pickview2(nviews, views)

c	    DO 132 i = 1, nviews
	    open(17,file='ang.out',status='unknown')
	    DO 132 i = 1, 90
	       		print*,'hola2',xmax
       	        theta = views(1,i)
		phi   = views(2,i)
		print*,theta,phi
		write (17,*) theta,phi
		majfac=1.4
		coordmax=Rmax

		CALL setmapzero(imsize, newimsize, map, newmap)

		CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	        CALL spacemap(istart,iend,xbuff,ybuff,imsize,coordmax,
     &                   smooth,newimsize,newmap,map,pmass)

	    CALL prof4(newmap,newimsize,newimsize,2,0.,majfac,
     &                 FLOAT(imsize/2),FLOAT(imsize/2),
     &                 0.2,0.,100,keyw,ang)
	    PA=ang
	    Rmax=xmax
c	    print*,'PA, Rmax=',ang,Rmax,xmax
            CALL rotcv
     &           (gdev, nbods, istart, iend,  theta, phi, 
     &           PA, Rmax, swidth,
     & 		 filename, model1, model2, modinc, add, 1,
     &           GRAVC, infostr, keyw)

 132	    CONTINUE
	    close(17)
C----
C       Calculate the rotc for different views
C----
        ELSE IF (command .EQ. 'multirotc') THEN
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
	    open(17,file='Vmax.out',status='unknown')
	    CALL pickview2(nviews, views)
	    DO 133 i = 1, nviews
		theta = views(1,i)
		phi   = views(2,i)
C		CALL setmapzero(imsize, newimsize, map, newmap)
		CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)
            CALL rotc4
     &           (gdev, nbods, istart, iend,  theta, phi, 
     &           PA, Rmax, swidth,
     &		 filename, model1, model2, modinc, add, 0,
     &           GRAVC, Vmax,sigma,infostr)
	    write(17,*) Vmax,sigma
 133	     CONTINUE
	     close(17)

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
C  Select range of particles
C-----------
	ELSE IF(command.eq.'bodsrange')THEN
	  istart= strgeti(par(1), erpar(1))
	  IF (erpar(1) .EQ. 0) THEN
	     iend  = strgeti(par(2), erpar(2))
	     IF (erpar(2) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	     END IF
	  ELSE			! case par(1)  not a number
	    range = strgets(par(1), erpar(1))
	    IF (npar .GE. 2) THEN
		n1 = strgeti(par(2), erpar(2))
		IF (erpar(1)+erpar(2) .NE. 0) THEN
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

C	    CALL info(0, command, gdev, filename, 
C     1                modelnum, model1, model2, modinc,
C     2                istart, iend,
C     3                theta, phi,
C     4                coordmax, PA, Rmax, swidth,
C     5                level, nlev,
C     6                infostr)

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
C  particle distribution points plot
C-------------
	ELSE IF(command.eq.'points2')THEN
	    coordmax = strgetr(par(1), erpar(1))
c	    imed = strgetr(par(2), erpar(2))
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

	    CALL ptplot2(gdev, istart, iend, theta, phi, 
     &			coordmax, infostr)

C-------------
C  particle distribution points plot with a scale lenght
C-------------
	ELSE IF(command.eq.'points3')THEN
	    coordmax = strgetr(par(1), erpar(1))
	    scalel = strgetr(par(2), erpar(2))
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

	    CALL ptplot3(gdev, istart, iend, theta, phi, coordmax,
     &			scalel,infostr)

C-------------
C  particle distribution points plot with a scale lenght
C-------------
	ELSE IF(command.eq.'points4')THEN
	    coordmax = strgetr(par(1), erpar(1))
	    iuno = strgetr(par(2), erpar(2))
	    imid = strgetr(par(3), erpar(3))
	    IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
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

	    CALL ptplot4(gdev, istart, iuno, imid, iend, 
     &			theta, phi, coordmax,infostr)
C-------------
C  particle distribution points plot with colors
C-------------
	ELSE IF(command.eq.'pcolors')THEN
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

	    CALL pcolors(gdev, istart, iend, theta, phi, coordmax,
     &			infostr)


C-------------
C  particle distribution points plot with colors
C-------------
	ELSE IF(command.eq.'pcolors2')THEN
	    coordmax = strgetr(par(1), erpar(1))
	    istart= strgetr(par(2), erpar(2))
	    imed1 = strgetr(par(3), erpar(3))
C            imed2 = strgetr(par(4), erpar(4))
	    iend  = strgetr(par(5), erpar(5))
	    IF (erpar(1) .NE. 0 ) THEN
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

	    CALL pcolors2(gdev, istart, imed1,iend,
     &		          theta, phi, coordmax,infostr)

C-------------
C  particle distribution points plot, many on a page
C-------------
	ELSE IF(command.eq.'manypts')THEN
	    coordmax = strgetr(par(1), erpar(1))
	    IF (npar .EQ. 3) THEN
		nx = strgeti(par(2), erpar(2))
		ny = strgeti(par(3), erpar(3))
	    ELSE
		nx = 4
		ny = 5
	    END IF
	    IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
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
     &			nx, ny,
     &                  infostr)
C-------------
C  particle distribution points plot, many on a page, ordered.
C-------------
	ELSE IF(command.eq.'manypts2')THEN
	    coordmax = strgetr(par(1), erpar(1))
	    IF (npar .EQ. 8) THEN
		nx = strgeti(par(2), erpar(2))
		ny = strgeti(par(3), erpar(3))
		istart2 = strgeti(par(4), erpar(4))
		iend2 = strgeti(par(5), erpar(5))
		after = strgeti(par(6), erpar(6))
		iempieza = strgeti(par(7), erpar(7))
		ifin = strgeti(par(8), erpar(8))
	    ELSE
		nx = 4
		ny = 5
	    END IF
	    IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
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

            CALL manypts2(model1, model2, modinc,
     &                  istart2,iend2,after,iempieza,ifin,filename,
     &                  gdev, istart, iend, theta, phi, coordmax,
     &			nx, ny,
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

C-------------
C  Surface density map, coadding models
C-------------
	ELSE IF(command.eq.'addspace')THEN
	   coordmax = strgetr(par(1), erpar(1))
	   IF (npar .GE. 2) THEN
		istart   = strgeti(par(2), erpar(2))
		iend     = strgeti(par(3), erpar(3))
	   ELSE
		istart 	= 1
		iend	= 1
	   END IF
	   IF (erpar(1)+erpar(2)+erpar(3) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	   END IF
	   CALL setmapzero(imsize, newimsize, map, newmap)
	   DO 70 m = model1, model2, modinc
	      model = m
	      CALL getmodel(model,filename,nbods)
	      IF (istart .LT. 1 .OR. iend .GT. nbods) THEN
      			PRINT*, ' nora>> body range 1 - ', nbods
			GO TO 120
	      END IF
	      IF (iend .EQ. 1) iend = nbods

	      CALL medcntr(istart, iend)

              CALL prject
     &             (istart, iend, theta, phi, xbuff, ybuff, vbuff)

	      CALL spacemap(istart,iend,xbuff,ybuff,imsize,coordmax,
     &                   smooth,newimsize,newmap1,map1,pmass)

	      CALL coadd(imsize, newimsize, map1, newmap1, map, newmap)
 70        CONTINUE

C------------
C sigma3 and <vphi>
C------------

	ELSE IF(command.eq.'sigma3')THEN
	   coordmax = strgetr(par(1), erpar(1))
	   IF (erpar(1) .NE. 0) THEN
		PRINT '(A)', usage(command)
		GO TO 120
	   END IF

	   CALL VSFIND (istart, iend, coordmax, sigma3, meanvt2)
	   print*,'sigma3, <V_phi>:',sigma3,meanvt2


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
c	      CALL getmodel(model,filename,header,nbods,
c     &                      x,y,z,vx,vy,vz,pmass)
	      CALL getmodel(model,filename,nbods)

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
	    proy= strgetr(par(2), erpar(2))
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
     &                             theta, phi, GRAVC, proy,infostr)

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
C  Gives the parameters to put the model in a Fundamental Plane
C-----------
C	ELSE IF(command.eq.'autofp') THEN
c	    coordmax = strgetr(par(1), erpar(1))
c            IF (erpar(1) .NE. 0) THEN
c                PRINT '(A)', usage(command)
c                GO TO 120
c            END IF
C	    CALL info(0, command, gdev, filename, 
C     1                modelnum, model1, model2, modinc,
C     2                istart, iend,
C     3                theta, phi,
C     4                coordmax, PA, Rmax, swidth,
C     5                level, nlev,
C     6                infostr)

C	    IF (gdev(1:4) .EQ. 'file' .OR. gdev(3:6) .EQ. 'file') THEN
C		gdev = 'autofpfile'
C	    ELSE
C		gdev = 'autoterm'
C	    END IF
C	    CALL autofp1
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
C-----------
C  Volume density profile
C-----------
	ELSE IF(command.eq.'ring') THEN
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

	    CALL ring(gdev, coordmax, istart, iend, GRAVC, infostr)
C------------

C------------
C Scale heigth
C------------
	 ELSE IF(command.eq.'zscale') THEN
	    coordmax = strgetr(par(1), erpar(1))
            IF (erpar(1) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF

	    CALL info(0, command, gdev, filename, 
     1               modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    call zscale(gdev,coordmax,istart,iend)
C------------
C Scale heigth
C------------
	 ELSE IF(command.eq.'cambioeje') THEN
	    eulert = strgetr(par(1), erpar(1))
	    euleri = strgetr(par(2), erpar(2))	    
            IF (erpar(1)+erpar(2) .NE. 0) THEN
                PRINT '(A)', usage(command)
                GO TO 120
            END IF

	    CALL info(0, command, gdev, filename, 
     1               modelnum, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev,
     6                infostr)

	    call cambioeje(gdev,eulert,euleri,nbods)

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

C-----------
C  Moment of innertia
C-----------
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

	ELSE IF(command.eq.'innertia2') THEN
	    coordmin = strgetr(par(1), erpar(1))
	    coordmax = strgetr(par(2), erpar(2))

            IF (erpar(2) .NE. 0) THEN
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
	    CALL intens2(istart, iend, coordmin, coordmax)
C	ELSE
C		print *,'nora>> Unknown Option <<'
C	END IF


C-----------
C  Ellipticity
C-----------
	ELSE IF(command.eq.'ellipt') THEN
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
	    CALL ellipt(istart, iend, coordmax)

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
