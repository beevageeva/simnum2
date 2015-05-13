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
		def __init__(self):
				wx.Dialog.__init__(self, None, -1, "About <<project>>",
						style=wx.DEFAULT_DIALOG_STYLE|wx.THICK_FRAME|wx.RESIZE_BORDER|
								wx.TAB_TRAVERSAL)
				hwin = wx.html.HtmlWindow(self, -1, size=(400,200))
				vers = {}
				aboutText = """<p>Total age(Gy): %(totalAge)s </p>

				<p>SFR(gy): %(sfr)s </p>
				<p>Metalicity(gy): %(isochrones)s </p>
				It is running on version %(wxpy)s of <b>wxPython</b> and %(python)s of <b>Python</b>.
				See <a href="http://wiki.wxpython.org">wxPython Wiki</a></p>"""
				vers["python"] = sys.version.split()[0]
				vers["wxpy"] = wx.VERSION_STRING
				vers["isochrones"] = vers["isos"] = "<ul>"

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


def line_picker2(line, mouseevent):
	print(line)
	print(dir(line))
	return False
	

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

		





class PickDialog(wx.Panel):
	class RangeCheckbox(wx.CheckBox):
		def __init__(self, parent, rangeValue, parentFrame):
			wx.CheckBox.__init__(self, parent, -1, str(rangeValue))
			self.Bind(wx.EVT_CHECKBOX, self.OnCheck)
			self.rangeValue = rangeValue
			self.parentFrame = parentFrame
			self.SetValue(parentFrame.ranges[rangeValue])
	
		def OnCheck(self,event):
				self.parentFrame.ranges[self.rangeValue] = not self.parentFrame.ranges[self.rangeValue]
				self.parentFrame.plotMyData()

	def __init__(self,parentPanel,  parentFrame):
		wx.Panel.__init__(self, parentPanel)
		self.SetSize((600,600))
			 
		self.sizer = wx.BoxSizer(wx.VERTICAL)
		self.sizer.Add(wx.StaticText(self, -1, "Groups of particles"))
		for item in sorted(parentFrame.ranges.items()):
			self.sizer.Add(PickDialog.RangeCheckbox(self, item[0], parentFrame ))
		self.SetSizer(self.sizer)
		print("  ---- end " +  str(self.GetSize()))



		#repaint
		parentPanel.Layout()
		parentPanel.Fit()
		self.sizer.Layout()
		self.Fit()
		self.Show(True)	

	def OnClear(self,event):
		for child in self.GetChildren(): 
			if(type(child) is wx.Panel):
				child.Destroy() 


from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d import proj3d

class Arrow3D(FancyArrowPatch):
	def __init__(self, xs, ys, zs, *args, **kwargs):
		FancyArrowPatch.__init__(self, (0,0), (0,0), *args, **kwargs)
		self._verts3d = xs, ys, zs

	def draw(self, renderer):
		xs3d, ys3d, zs3d = self._verts3d
		xs, ys, zs = proj3d.proj_transform(xs3d, ys3d, zs3d, renderer.M)
		self.set_positions((xs[0],ys[0]),(xs[1],ys[1]))
		FancyArrowPatch.draw(self, renderer)

class CanvasFrame(wx.Frame):




		def __init__(self, data, numpart, ranges):
			
				self.vector = None
				self.data = data 	
				self.numpart = numpart
				self.ranges = ranges 	
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
				#self.Bind(wx.EVT_MENU, self.newIsochroneFile, m_iso)
				m_liso = menu.Append(-1, "List", "List Isochrone Files")
				#self.Bind(wx.EVT_MENU, self.listIsochroneFiles, m_liso)
				self.m_line = menu.Append(-1, "Line style -", "Line style", kind=wx.ITEM_CHECK)
				self.m_line.Check()
				#self.Bind(wx.EVT_MENU, self.changeIsolinestyle, self.m_line)
				self.m_legend = menu.Append(-1, "Show Legend", "Legend", kind=wx.ITEM_CHECK)
				self.Bind(wx.EVT_MENU, self.changeLegendVisible, self.m_legend)
				m_inimass = menu.Append(-1, "Masa inicial minima", "Masa inicial minima")
				#self.Bind(wx.EVT_MENU, self.changeMinIniMass, m_inimass)
				menuBar.Append(menu, "&Isocrone files")
			
				menu = wx.Menu()
				m_about = menu.Append(wx.ID_ABOUT, "&About", "Information about this program")
				self.Bind(wx.EVT_MENU, self.OnAbout, m_about)
				m_clear = menu.Append(-1, "&Clear plot", "Clear")
				self.Bind(wx.EVT_MENU, self.OnClear, m_clear)
				m_replot = menu.Append(-1, "&Replot", "Replot")
				#self.Bind(wx.EVT_MENU, lambda event: self.replotIsosVisible(), m_replot)
				menuBar.Append(menu, "&Help")

				self.SetMenuBar(menuBar)
				self.statusbar = self.CreateStatusBar()

				self.figure = Figure()
			
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
				self.axes = self.figure.add_subplot(111, projection='3d')
				#self.canvas.SetSize((800,600))
				self.canvas.mpl_connect('pick_event', self.onpick2)

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
			print("PICK")
			ind = event.ind[0]
			print("vertss3d")
			x,y,z=thisline._verts3d
			print("%2.3f,%2.3f,%2.3f" % (x[ind],y[ind],z[ind]))
			for i in range(self.numpart):
				if self.data[i,0] == x[ind] and self.data[i,1] == y[ind] and self.data[i,2] == z[ind]:
					print("Found index in data %d" % i)
					print("velocity is vx=%e,vy=%e,vz=%e" % (self.data[self.numpart+i,0], self.data[self.numpart+i,1], self.data[self.numpart+i,2]))
					if not self.vector is None:
						self.vector.remove()
					self.vector = Arrow3D([x[ind],x[ind] +10* self.data[self.numpart+i,0]],[y[ind],y[ind]+10* self.data[self.numpart+i,1]],[z[ind],z[ind]+10*self.data[self.numpart+i,2]], mutation_scale=20, lw=1, arrowstyle="-|>", color="r")
					self.axes.add_artist(self.vector)
					self.repaint()
					break	
			#z = thisline.get_zdata()[ind]
			#print x, y, z
		

		##SCROLLING
		#def OnSize(self, event):
		#	self.scrolling.SetSize(self.GetClientSize())


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
				dlg = AboutBox()
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
			print(self.data.shape)
			self.axes.cla()
			self.vector = None
			lastindex = 0
			indices = []
			for item in sorted(self.ranges.items()):
				if(item[1]):
					indices+=range(lastindex, item[0])
				lastindex = item[0]
					
			velInd = np.array(indices) + self.numpart

			self.axes.plot(self.data[indices,0] ,  self.data[indices,1], self.data[indices,2], "ko", markersize=1, picker=1)
			#self.axes.quiver3d(self.data[indices,0] ,  self.data[indices,1], self.data[indices,2], self.data[velInd,0] , self.data[velInd,1], self.data[velInd,2])
			#self.axes.plot(mydata[:,0],mydata[:,1], "ro", markeredgecolor='none', color="#000000" , markersize=1)
			self.axes.set_xlabel('x')
			self.axes.set_ylabel('y')
			self.axes.set_zlabel('z')
			self.repaint()	

class App(wx.App):

		def OnInit(self):
				'Create the main window and insert the custom frame'
				argv = sys.argv[1:]
				mydatafile="two.xvp.txt"
				import re
				with open(mydatafile, 'r') as f:
					first_line = f.readline()
				
				header = re.split('\s+', first_line)
				print("header")
				print(len(header))	
				print(header[1])
				print(header[101])
				print(header[102])
				numpart = int(float(header[1]))
				numRanges = int(float(header[101]))
				ranges = {}
				for i in range(numRanges):
					ranges[int(float(header[102+2*i]))] =  True

				print(numpart)
				print(ranges)
				data = np.loadtxt(mydatafile, skiprows=1+numpart)
				frame = CanvasFrame(data, numpart, ranges)
				frame.Show(True)

				return True





app = App(0)
app.MainLoop()
