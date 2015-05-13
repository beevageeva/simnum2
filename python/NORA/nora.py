#!/usr/bin/env python
"""
An example of how to use wx or wxagg in an application with the new
toolbar - comment out the setA_toolbar line for no toolbar
"""

# Used to guarantee to use at least Wx2.8
import wxversion
wxversion.ensureMinimal('2.8')

import numpy as np

import matplotlib

from mpl_toolkits.mplot3d import Axes3D


# uncomment the following to use wx rather than wxagg
#matplotlib.use('WX')
#from matplotlib.backends.backend_wx import FigureCanvasWx as FigureCanvas

# comment out the following to use wx rather than wxagg
matplotlib.use('WXAgg')
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas

from matplotlib.backends.backend_wx import NavigationToolbar2Wx

from matplotlib.figure import Figure

import wx, wx.html, sys,getopt, os.path
from math import fabs

#				<p>SFR string: %(sfr)s </p><p>
#				<p>CEL1 string: %(cel1)s </p>
#				<p>CEL2 string: %(cel2)s </p>


class AboutBox(wx.Dialog):
		def __init__(self, isochrones):
				wx.Dialog.__init__(self, None, -1, "About <<project>>",
						style=wx.DEFAULT_DIALOG_STYLE|wx.THICK_FRAME|wx.RESIZE_BORDER|
								wx.TAB_TRAVERSAL)
				hwin = wx.html.HtmlWindow(self, -1, size=(400,200))
				vers = {}
				aboutText = """<p>Total age(Gy): %(totalAge)s </p>
				<p>isochrones: %(isos)s </p>

				<p>SFR(gy): %(sfr)s </p>
				<p>Metalicity(gy): %(isochrones)s </p>
				It is running on version %(wxpy)s of <b>wxPython</b> and %(python)s of <b>Python</b>.
				See <a href="http://wiki.wxpython.org">wxPython Wiki</a></p>"""
				vers["python"] = sys.version.split()[0]
				vers["wxpy"] = wx.VERSION_STRING
				vers["isochrones"] = vers["isos"] = "<ul>"
				totalage = 0

				for isoname in isochrones.keys():
					if isochrones[isoname]["visible"] and  len(isochrones[isoname]["toPlot"])>0:
						vers["isos"] += "<li>" + isoname + ":{"  + ",".join(["%5.2f" % i for i in  sorted(isochrones[isoname]["toPlot"])] ) + "}</li>"
						isomax = max(isochrones[isoname]["toPlot"])
						if isomax > totalage:
							totalage = isomax
				vers["isos"]+="</ul>"
				totalage = 10 ** (totalage - 9)
				vers["totalAge"] = "%5.2f" % totalage		
				vers["sfr"] = ""
				#TODO not hardcode the values
				startx=endx=None
				for x in np.arange(10.25,7.75,-0.05):
					x=round(float(x),2)
					found = False
					for isoname in isochrones.keys():
						if isochrones[isoname]["visible"] and  (x in isochrones[isoname]["toPlot"]):
							found = True
							startx=x
							if endx is None:
								endx = x
					if not found:
						if not endx is None :
							vers["sfr"] +=" [" + "%5.2f" % (totalage - 10 ** (endx-9))   + ", " + "%5.2f" % (totalage - 10 ** (startx-9))  + "] "
							endx = None
				if not endx is None :
					vers["sfr"] +=" [" + "%5.2f" % (totalage - 10 ** (endx-9))   + ", " + "%5.2f" % (totalage - 10 ** (startx-9))  + "] "
					

						
				for isoname in isochrones.keys():
					if isochrones[isoname]["visible"] and  len(isochrones[isoname]["toPlot"])>0:
						vers["isochrones"] += "<li>" + isoname + ":{" 
						startx=oldx=None
						for x in sorted(isochrones[isoname]["toPlot"], reverse = True ):
							if startx is None:
								startx = x
							else:
								if (not oldx is None) and  (x<oldx-0.05):
									vers["isochrones"] +=" [" + "%5.2f" % (totalage - 10 ** (startx-9))   + ", " + "%5.2f" % (totalage - 10 ** (oldx-9))  + "] "
									startx = x		
						
							oldx=x
							
						vers["isochrones"] +=" [" + "%5.2f" % (totalage - 10 ** (startx-9))   + ", " + "%5.2f" % (totalage - 10 ** (oldx-9))  + "] "
						vers["isochrones"] += "}</li>"
						#vers["isochrones"] += "<li>" + isoname + ":{" +  ",".join([ "%5.2f" % (totalage - 10 ** (x-9)) for x in sorted(isochrones[isoname]["toPlot"], reverse = True )]) + "}" + "</li>"
				vers["isochrones"] += "</ul>"

				hwin.SetPage(aboutText % vers)
				btn = hwin.FindWindowById(wx.ID_OK)
				irep = hwin.GetInternalRepresentation()
				hwin.SetSize((irep.GetWidth()+25, irep.GetHeight()+10))
				self.SetClientSize(hwin.GetSize())
				self.CentreOnParent(wx.BOTH)
				self.SetFocus()

def floatRgb(mag, cmin, cmax):
	"""
	Return a tuple of floats between 0 and 1 for the red, green and
	blue amplitudes.
	"""
	try:
		# normalize to [0,1]
		x = float(mag-cmin)/float(cmax-cmin)
	except:
		# cmax = cmin
		x = 0.5
	blue = min((max((4*(0.75-x), 0.)), 1.))
	red  = min((max((4*(x-0.25), 0.)), 1.))
	green= min((max((4*fabs(x-0.5)-1., 0.)), 1.))
	return "#%02x%02x%02x" % (red*255, green*255, blue*255)

def line_picker(line, mouseevent):
    """
    find the points within a certain distance from the mouseclick in
    data coords and attach some extra attributes, pickx and picky
    which are the data points that were picked
    """
    if mouseevent.xdata is None: return False, dict()
    xdata = line.get_xdata()
    ydata = line.get_ydata()
		#TODO hardcoded value
    maxd = 0.05
    d = np.sqrt((xdata-mouseevent.xdata)**2. + (ydata-mouseevent.ydata)**2.)

    ind = np.nonzero(np.less_equal(d, maxd))
    if len(ind):
        pickx = np.take(xdata, ind)
        picky = np.take(ydata, ind)
        props = dict(line=line, ind=ind, pickx=pickx, picky=picky)
        return True, props
    else:
        return False, dict()


class IsoageCheckbox(wx.CheckBox):
	def __init__(self, parent, age, isoname, parentFrame):
		isohash = parentFrame.isochrones[isoname]
		wx.CheckBox.__init__(self, parent, -1, str(age))
		self.SetValue(age in isohash['toPlot'])
		self.Bind(wx.EVT_CHECKBOX, self.OnCheck)
		self.age = age
		self.isoname = isoname
		self.parentFrame = parentFrame
	
	def OnCheck(self,event):
		#print "value " + str(self.GetValue())
		self.parentFrame.checkIsoAge(self.isoname, self.age, self.GetValue())

class ListIsoAges(wx.Dialog):

	def __init__(self, isoname, parentFrame):
		#wx.Dialog.__init__(self, None, -1, "Ages for " + isoname,(600,600),style=wx.DEFAULT_DIALOG_STYLE|wx.THICK_FRAME|wx.RESIZE_BORDER|wx.TAB_TRAVERSAL)
		wx.Dialog.__init__(self, None, -1, "Ages for " + isoname,style=wx.DEFAULT_DIALOG_STYLE)
		self.SetSize((600,600))
		#self.sizer = wx.BoxSizer(wx.VERTICAL)
		self.sizer = wx.GridSizer(rows=4, cols=4, hgap=5, vgap=5)
		isohash = parentFrame.isochrones[isoname]
		dataiso = np.genfromtxt(isohash['filename'], comments="#", usecols=(0), converters={0: lambda x: round(float(x),2)})
		for age in np.unique(dataiso):
			#print "age = " + str(age)
			isopanel = wx.Panel(self)
			hbox1 = wx.BoxSizer(wx.HORIZONTAL)        
			hbox1.Add(IsoageCheckbox(isopanel, age ,isoname, parentFrame))
			isopanel.SetSizer(hbox1)
			self.sizer.Add(isopanel, 0, wx.ALL, 10)
		okbtn = wx.Button(self, 1, 'Ok', (90, 185), (60, -1))
		self.sizer.Add(okbtn)
		self.Bind(wx.EVT_BUTTON, self.OnClose, okbtn)
		self.SetSizer(self.sizer)

	def OnClose(self,event):
		self.Destroy()
		



class ListIsochronesBox(wx.Dialog):

	class IsoButton(wx.Button):
		def __init__(self, parent, isoname, parentFrame):
			wx.Button.__init__(self, parent, -1,"Ages", (500,500))
			self.Bind(wx.EVT_BUTTON, self.listAges)
			self.isoname = isoname
			self.parentFrame = parentFrame
	
		def listAges(self,event):
			#print "list ages for " + str(self.isoname)
			dlg = ListIsoAges(self.isoname, self.parentFrame)
			dlg.ShowModal()
			dlg.Destroy()
	
	#TODO I do not know HOW to get the label of the checkbox -- THIS is why I make a new class to be able to reference isohash
	class IsoCheckbox(wx.CheckBox):
		def __init__(self, parent, isoname, parentFrame):
			wx.CheckBox.__init__(self, parent, -1, isoname)
			self.Bind(wx.EVT_CHECKBOX, self.OnCheck)
			self.isoname = isoname
			self.parentFrame = parentFrame
			self.SetValue(parentFrame.isochrones[isoname]['visible'])
	
		def OnCheck(self,event):
				self.parentFrame.setIsoVisible(self.isoname, self.GetValue())


	def __init__(self, parentFrame):
		wx.Dialog.__init__(self, None, -1, "Isocronas",style=wx.DEFAULT_DIALOG_STYLE|wx.THICK_FRAME|wx.RESIZE_BORDER|wx.TAB_TRAVERSAL|wx.CLOSE_BOX)
		self.CentreOnParent(wx.BOTH)
		self.SetFocus()
		self.sizer = wx.GridSizer(rows=4, cols=2, hgap=5, vgap=5)
		#self.sizer = wx.BoxSizer(wx.VERTICAL)
		self.parentFrame = parentFrame
		for isoname in parentFrame.isochrones.keys():	
			isopanel = wx.Panel(self)
			hbox1 = wx.BoxSizer(wx.HORIZONTAL)        
			hbox1.Add(ListIsochronesBox.IsoCheckbox(isopanel, isoname, parentFrame))
			hbox1.Add(ListIsochronesBox.IsoButton(isopanel,isoname , parentFrame), flag=wx.LEFT, border=5)
			isopanel.SetSizer(hbox1)
			self.sizer.Add(isopanel, 0, wx.ALL, 10)
		okbtn = wx.Button(self, 1, 'Ok', (90, 185), (60, -1))
		self.sizer.Add(okbtn)
		self.Bind(wx.EVT_BUTTON, self.OnClose, okbtn)
		self.SetSizer(self.sizer)

	def OnClose(self,event):
		self.Destroy()



class PickDialog(wx.Panel):

	def __init__(self,parentPanel,  parentFrame):
		wx.Panel.__init__(self, parentPanel)
		self.SetSize((600,600))
		self.parentFrame = parentFrame
		self.sizer = wx.BoxSizer(wx.VERTICAL)
		self.sizer.Add(wx.StaticText(self, -1, "Picked lines"))
		okbtn = wx.Button(self, 1, 'Clear')
		self.sizer.Add(okbtn)
		self.Bind(wx.EVT_BUTTON, self.OnClear, okbtn)
		self.SetSizer(self.sizer)
		self.parentPanel = parentPanel
		print("  ---- end " +  str(self.GetSize()))

	def addIsoage(self, isoname, age):
		isohash = self.parentFrame.isochrones[isoname]

		isopanel = wx.Panel(self)
		hbox = wx.BoxSizer(wx.HORIZONTAL)        
		hbox.Add(wx.StaticText(isopanel,-1, label=isoname))
		hbox.Add(IsoageCheckbox(isopanel, age ,isoname, self.parentFrame))
		isopanel.SetSizer(hbox)
		self.sizer.Add(isopanel, 0, wx.ALL, 10)


		#repaint
		self.parentPanel.Layout()
		self.parentPanel.Fit()
		self.sizer.Layout()
		self.Fit()
		self.Show(True)	

	def OnClear(self,event):
		for child in self.GetChildren(): 
			if(type(child) is wx.Panel):
				child.Destroy() 



class CanvasFrame(wx.Frame):

		def __init__(self, mydatafile):
			
				self.isochrones = {}
				self.isolinestyle="-"
				self.minIniMass=0.7		

				self.mydatafile = mydatafile 	
				wx.Frame.__init__(self,None,-1,
												 'CanvasFrame')
				self.SetBackgroundColour(wx.NamedColour("WHITE"))
				self.Bind(wx.EVT_CLOSE, self.OnClose)

				menuBar = wx.MenuBar()
				menu = wx.Menu()
				m_exit = menu.Append(wx.ID_EXIT, "E&xit\tAlt-X", "Close window and exit program.")
				self.Bind(wx.EVT_MENU, self.OnClose, m_exit)
				menuBar.Append(menu, "&File")

				menu = wx.Menu()
				m_iso = menu.Append(-1, "New", "New Isochrone File")
				self.Bind(wx.EVT_MENU, self.newIsochroneFile, m_iso)
				m_liso = menu.Append(-1, "List", "List Isochrone Files")
				self.Bind(wx.EVT_MENU, self.listIsochroneFiles, m_liso)
				self.m_line = menu.Append(-1, "Line style -", "Line style", kind=wx.ITEM_CHECK)
				self.m_line.Check()
				self.Bind(wx.EVT_MENU, self.changeIsolinestyle, self.m_line)
				self.m_legend = menu.Append(-1, "Show Legend", "Legend", kind=wx.ITEM_CHECK)
				self.Bind(wx.EVT_MENU, self.changeLegendVisible, self.m_legend)
				m_inimass = menu.Append(-1, "Masa inicial minima", "Masa inicial minima")
				self.Bind(wx.EVT_MENU, self.changeMinIniMass, m_inimass)
				menuBar.Append(menu, "&Isocrone files")
			
				menu = wx.Menu()
				m_about = menu.Append(wx.ID_ABOUT, "&About", "Information about this program")
				self.Bind(wx.EVT_MENU, self.OnAbout, m_about)
				m_clear = menu.Append(-1, "&Clear plot", "Clear")
				self.Bind(wx.EVT_MENU, self.OnClear, m_clear)
				m_replot = menu.Append(-1, "&Replot", "Replot")
				self.Bind(wx.EVT_MENU, lambda event: self.replotIsosVisible(), m_replot)
				menuBar.Append(menu, "&Help")

				self.SetMenuBar(menuBar)
				self.statusbar = self.CreateStatusBar()

				self.figure = Figure()
				self.axes = self.figure.add_subplot(111, projection='3d')
			
				#SCROLLING
				#self.scrolling = wx.ScrolledWindow( self )
				#self.scrolling.SetSize((800,600))
				#self.scrolling.SetScrollRate(1,1)
				#self.scrolling.EnableScrolling(True,True)
				#self.scrolling.SetScrollbars(1, 1, 600, 400)
				#self.canvas = FigureCanvas(self.scrolling, -1, self.figure)
				hpanel = wx.Panel(self)
				hpanel.SetSize(self.GetSize())
				vpanel = wx.Panel(self)
				self.canvas = FigureCanvas(vpanel, -1, self.figure)
				#self.canvas.SetSize((800,600))
				#TODOCANV	
				#self.canvas.mpl_connect('pick_event', self.onpick2)

				self.sizer = wx.BoxSizer(wx.VERTICAL)
		
				hsizer = wx.BoxSizer(wx.HORIZONTAL)
				vsizer = wx.BoxSizer(wx.VERTICAL)
				hpanel.SetSizer(hsizer)	
				vpanel.SetSizer(vsizer)	
				vsizer.Add(self.canvas, 0, wx.ALL, 10)
				hsizer.Add(vpanel)
				self.sizer.Add(vpanel, 0, wx.ALL, 10)
		

				#PICK items	
				self.pickDialog = PickDialog(hpanel, self)
				hsizer.Add(self.pickDialog, 0, wx.ALL, 10)
									


				# TOOLBAR comment this out for no toolbar
				self.toolbar = NavigationToolbar2Wx(self.canvas)
				self.toolbar.Realize()
				if wx.Platform == '__WXMAC__':
						# Mac platform (OSX 10.3, MacPython) does not seem to cope with
						# having a toolbar in a sizer. This work-around gets the buttons
						# back, but at the expense of having the toolbar at the top
						self.SetToolBar(self.toolbar)
				else:
						# On Windows platform, default window size is incorrect, so set
						# toolbar width to figure width.
						tw, th = self.toolbar.GetSizeTuple()
						fw, fh = self.canvas.GetSizeTuple()
						# By adding toolbar in sizer, we are able to put it at the bottom
						# of the frame - so appearance is closer to GTK version.
						# As noted above, doesn't work for Mac.
						self.toolbar.SetSize(wx.Size(fw, th))
						vsizer.Add(self.toolbar, 0, wx.LEFT | wx.EXPAND)
				# update the axes menu on the toolbar
				self.toolbar.update()

				self.SetSizer(self.sizer)
				hsizer.Layout()
				hpanel.Fit()
				self.sizer.Layout()
				self.Fit()
				#TODO not hardcode this
				self.SetSize((1000,800))
				#TODO why this is needed in python/wx CCA otherwise y axis will show in 0..1
				self.axes.cla()	
				self.plotMyData()
				#TODO why legend does not show first line plotted
				self.repaint()	
	
		
		def onpick2(self,event):
			thisline = event.artist
			for isoname in self.isochrones.keys():
				isohash = self.isochrones[isoname]
				if isohash["visible"]:
					for age in isohash["toPlot"]:
						if thisline==isohash["plotted"][age] and  event.pickx.shape[1] > 0:
							self.pickDialog.addIsoage(isoname, age)
							print("onpick2 line: x=" + str(event.pickx[0])+",y="+ str(event.picky[0]) + ", isoname="+isoname +",age="+str(age) )
		

		##SCROLLING
		#def OnSize(self, event):
		#	self.scrolling.SetSize(self.GetClientSize())

		def changeMinIniMass(self, event):
			ma = ['0.0','0.7', '0.75', '0.8', '0.85', '0.9', '1', '1.05', '1.1', '1.15', '1.2', '1.25', '1.3']
			dlg = wx.SingleChoiceDialog(self, 'Masa inicial minima', 'Which one?', ma, wx.CHOICEDLG_STYLE)
			#print "INI MASS =>" + str(self.minIniMass)	+ "<"
			#print "INI MAASS INDEX = " + str(ma.index(str(self.minIniMass)))	
			dlg.SetSelection(ma.index(str(self.minIniMass)))
			if dlg.ShowModal() == wx.ID_OK:
				self.minIniMass = float(dlg.GetStringSelection())
				self.replotIsosVisible()
			dlg.Destroy()	


		def newIsochroneFile(self,event):
			openFileDialog = wx.FileDialog(self, "Open iso  file", "", "","DAT files (*.dat)|*.dat", wx.FD_OPEN | wx.FD_FILE_MUST_EXIST)
			if openFileDialog.ShowModal() == wx.ID_CANCEL:
				#cancelled
				return     
			# proceed loading the file chosen by the user
			# this can be done with e.g. wxPython input streams:
			addIsochroneFile(openFileDialog.GetPath())

		def addIsochroneFile(self,filename):
			isoname = os.path.basename(filename)[:-4]	
			newiso = {'filename':filename, 'plotted':{}, 'visible' : True, 'toPlot':[]}
			self.isochrones[isoname] = newiso
			print("OPENED'%s'."%filename)


		#dataiso is supposed to have 4 cols
		def plotDataiso(self,dataiso,age):
			dataiso=dataiso[dataiso[:,2]==age]
 		  #in dhr7.dat seems that there are no values for mass < 0.7
			if(self.minIniMass > 0):
				dataiso=dataiso[dataiso[:,3]>self.minIniMass]
			#delete age and masainicial columns
			dataiso = np.delete(dataiso,(2,3),axis=1)
			dataiso[:,0] = dataiso[:,0] - dataiso[:,1]
			colorstring =  floatRgb(age, 7.8, 10.25)
			line, = self.axes.plot(dataiso[:,0],dataiso[:,1], "o" , markersize=1, color=colorstring ,  markeredgecolor='none' , label= "%5.2f" %  (10.0 ** (age - 9)) + " GY", picker=line_picker)
			line.set_linestyle(self.isolinestyle)
			return line

		def changeIsolinestyle(self, event):
			if(self.m_line.IsChecked()):
				self.isolinestyle = "-"
			else:
				self.isolinestyle = ""
			#change existing ISOCHRONE lines linestyle 
			for isoname in self.isochrones.keys():	
				isohash = self.isochrones[isoname]
				for age in  isohash["plotted"]:
					isohash["plotted"][age].set_linestyle(self.isolinestyle)
			#REFRESH
			self.repaint()

		def changeLegendVisible(self, event):
			if(not self.m_legend.IsChecked()):
				self.axes.legend_ = None
			else:
				self.axes.legend().draggable()
			#NO REPAINT only needs canvas.draw, not to redraw legend
			self.canvas.draw()
		

		def replotIsosVisible(self):
			for isoname in self.isochrones.keys():
				isohash = self.isochrones[isoname]
				dataiso = np.genfromtxt(isohash['filename'], comments="#", usecols=(9,11,0,1), converters={0: lambda x: round(float(x),2)})
				if(isohash["visible"]):
					for age in isohash['toPlot']:
						self.axes.lines.remove(isohash["plotted"][age])
						isohash['plotted'][age] = self.plotDataiso(dataiso,age)
			#repaint
			self.repaint()
					
	


		def setIsoVisible(self, isoname, value):
			self.isochrones[isoname]['visible'] = value
			isohash = self.isochrones[isoname]
			if(value):
				dataiso = np.genfromtxt(isohash['filename'], comments="#", usecols=(9,11,0,1), converters={0: lambda x: round(float(x),2)})
				for age in isohash['toPlot']:
					isohash['plotted'][age] = self.plotDataiso(dataiso,age)
			else:
				for age in isohash['toPlot']:
					self.axes.lines.remove(isohash["plotted"][age])
				isohash["plotted"].clear()
			#REFRESH
			self.repaint()
		
		def checkIsoAge(self, isoname, age, value):
			isohash = self.isochrones[isoname]
			if(value):
				if not age in isohash['toPlot']:
					isohash['toPlot'].append(age)
					if(isohash['visible']):
						dataiso = np.genfromtxt(isohash['filename'], comments="#", usecols=(9,11,0,1), converters={0: lambda x: round(float(x),2)})
						isohash['plotted'][age] = self.plotDataiso(dataiso, age)
						#REFRESH
						self.repaint()
			else:
				if age in isohash['toPlot']:
					isohash["toPlot"].remove(age)
					if(isohash["visible"]):
						self.axes.lines.remove(isohash["plotted"][age])
						del isohash["plotted"][age]
						#REFRESH	
						self.repaint()
	

		def listIsochroneFiles(self,event):
				dlg = ListIsochronesBox(self)
				dlg.ShowModal()
				dlg.Destroy()

		def OnPaint(self, event):
				print "ON paint EVENT  repaint"
				self.repaint()

		def repaint(self):
				self.canvas.draw()
				#if(self.m_legend.IsChecked()):
				#makes more sense to check m_legend in bind function changeLegendVisible
				if(not (self.axes.legend_ is None)):
					self.axes.legend().draggable()

		def OnClose(self, event):
				dlg = wx.MessageDialog(self,
						"Do you really want to close this application?",
						"Confirm Exit", wx.OK|wx.CANCEL|wx.ICON_QUESTION)
				result = dlg.ShowModal()
				dlg.Destroy()
				if result == wx.ID_OK:
						self.Destroy()

		def OnAbout(self, event):
				dlg = AboutBox(self.isochrones)
				dlg.ShowModal()
				dlg.Destroy()

		def OnClear(self, event):
			for isoname in self.isochrones.keys():
				self.isochrones[isoname]["plotted"].clear()
				del self.isochrones[isoname]["toPlot"][:]
			self.axes.cla()
			self.plotMyData()
			#repaint
			self.repaint()
			
		def plotMyData(self):
			import re
			with open(self.mydatafile, 'r') as f:
				first_line = f.readline()
			rexpr = re.compile("\s*([\d.E\+-]+)(?:\s+[\d.E\+-]+)+\s*")
			g = rexpr.match(first_line)
			
			numpart = int(float((g.group(1))))
			data = np.loadtxt(self.mydatafile, skiprows=1+numpart)
			print(data.shape)
			

			self.axes.plot(data[numpart:,0] ,  data[numpart:,1], data[numpart:,2], "ko")
			#self.axes.plot(mydata[:,0],mydata[:,1], "ro", markeredgecolor='none', color="#000000" , markersize=1)
			self.axes.set_xlabel('x')
			self.axes.set_ylabel('y')
			self.axes.set_ylabel('z')
	

class App(wx.App):

		def OnInit(self):
				'Create the main window and insert the custom frame'
				argv = sys.argv[1:]
				mydatafile="two.xvp.txt"
				frame = CanvasFrame(mydatafile)
				frame.Show(True)

				return True





app = App(0)
app.MainLoop()
