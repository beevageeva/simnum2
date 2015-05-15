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
						style=wx.DEFAULT_DIALOG_STYLE|wx.THICK_FRAME|wx.RESIZE_BORDER| wx.CLOSE_BOX|
								wx.TAB_TRAVERSAL)
				hwin = wx.html.HtmlWindow(self, -1, size=(400,200))
				vers = {}
				aboutText = """
				It is running on version %(wxpy)s of <b>wxPython</b> and %(python)s of <b>Python</b>.
				See <a href="http://wiki.wxpython.org">wxPython Wiki</a></p>"""
				vers["python"] = sys.version.split()[0]
				vers["wxpy"] = wx.VERSION_STRING

				hwin.SetPage(aboutText % vers)
				btn = hwin.FindWindowById(wx.ID_OK)
				irep = hwin.GetInternalRepresentation()
				hwin.SetSize((irep.GetWidth()+25, irep.GetHeight()+10))
				self.SetClientSize(hwin.GetSize())
				self.CentreOnParent(wx.BOTH)
				self.SetFocus()







class PickDialog(wx.Panel):
	class RangeCheckbox(wx.Panel):
	
		def __init__(self, parent, rangeValue, parentFrame):

			wx.Panel.__init__(self, parent, -1)

			sizer = wx.BoxSizer(wx.HORIZONTAL)
			check = wx.CheckBox(self, -1, str(rangeValue))
			check.Bind(wx.EVT_CHECKBOX, self.OnCheck)
			self.cb = wx.ComboBox(self,size=(100,20), choices=["1","2"])
			self.cb.SetValue("1")
			#self.cb.Append("Undefined", 0)
			#self.cb.Append("Object 1", 1)
			#self.cb.Append("Object 2", 2)
			self.cb.Bind(wx.EVT_COMBOBOX, self.onSelect)
			sizer.Add(check)
			sizer.Add(self.cb)
			self.SetSizer(sizer)
			self.rangeValue = rangeValue
			self.parentFrame = parentFrame
			check.SetValue(parentFrame.ranges[rangeValue][0])
	
		def OnCheck(self,event):
				self.parentFrame.ranges[self.rangeValue][0] = not self.parentFrame.ranges[self.rangeValue][0]
				self.parentFrame.plotMyData()
				self.parentFrame.repaint()	

		def onSelect(self, event):
			print(self.cb.GetClientData(self.cb.GetSelection()))
			print("GET VALUE %s" % self.cb.GetValue())
			self.parentFrame.ranges[self.rangeValue][1] = int(self.cb.GetValue())


	def __init__(self,parentPanel,  parentFrame):
		wx.Panel.__init__(self, parentPanel)
		self.SetSize((600,600))
			 
		sizer = wx.BoxSizer(wx.VERTICAL)
		sizer.Add(wx.StaticText(self, -1, "Groups of particles"))
		for item in sorted(parentFrame.ranges.items()):
			sizer.Add(PickDialog.RangeCheckbox(self, item[0], parentFrame ))

		panelButtons = wx.Panel(self)
		sizer2 = wx.BoxSizer(wx.HORIZONTAL)
		button1 = wx.Button(panelButtons, label="<<", size=(30, 30))	
		button1.Bind(wx.EVT_BUTTON, parentFrame.readFirstModel)
		sizer2.Add(button1)
		button2 = wx.Button(panelButtons, label="<", size=(30, 30) )
		button2.Bind(wx.EVT_BUTTON, parentFrame.readPrevModel)
		sizer2.Add(button2)
		button3 = wx.Button(panelButtons, label=">" , size=(30, 30) )
		button3.Bind(wx.EVT_BUTTON, parentFrame.readNextModel)
		sizer2.Add(button3)
		button4 = wx.Button(panelButtons, label=">>"  ,size=(30, 30))
		button4.Bind(wx.EVT_BUTTON, parentFrame.readLastModel)
		sizer2.Add(button4)

		panelButtons.SetSizer(sizer2)
		sizer.Add(panelButtons)

		self.SetSizer(sizer)
		print("  ---- end " +  str(self.GetSize()))



		#repaint
		parentPanel.Layout()
		parentPanel.Fit()
		sizer.Layout()
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


		def reloadModel(self):
				filename = self.globPattern.replace("*", str(self.modelNumbers[self.currentModelNumberIndex]) )
				print("FILENAME %s" % filename)
				self.data = np.loadtxt(filename, skiprows=1+self.numpart)
				self.plotMyData()
				self.repaint()

		def __init__(self, globPattern):
				self.globPattern = globPattern
				#read model
				self.modelNumbers = []
				import glob,re
				for name in glob.glob(globPattern):
					m = re.search("(\d+)", name)
					if(m):
						#print(m.group(1))
						self.modelNumbers.append(int(m.group(1)))
				self.modelNumbers=sorted(self.modelNumbers)
				self.currentModelNumberIndex =  0

				filename = globPattern.replace("*", str(self.modelNumbers[self.currentModelNumberIndex]))
				print("FILENAME %s" % filename)
				import re
				with open(filename, 'r') as f:
					first_line = f.readline()
				
				header = re.split('\s+', first_line)
				#print("header")
				#print(len(header))	
				#print(header[1])
				#print(header[101])
				#print(header[102])
				self.numpart = int(float(header[1]))
				numRanges = int(float(header[101]))
				self.ranges = {}
				for i in range(numRanges):
					self.ranges[int(float(header[102+2*i]))] =  [True, 1,float(header[102+2*i+1]) ]

				print(self.numpart)
				print(self.ranges)
				self.data = np.loadtxt(filename, skiprows=1+self.numpart)

				self.vector = None
				wx.Frame.__init__(self,None,-1,
												 'CanvasFrame')
				self.SetBackgroundColour(wx.NamedColour("WHITE"))
				self.Bind(wx.EVT_CLOSE, self.OnClose)

				menuBar = wx.MenuBar()
				menu = wx.Menu()

				m_showObjects = menu.Append(-1, "Center distance", "Center distance")
				self.Bind(wx.EVT_MENU, self.OnShowObjects, m_showObjects)

				m_exit = menu.Append(wx.ID_EXIT, "E&xit\tAlt-X", "Close window and exit program.")
				self.Bind(wx.EVT_MENU, self.OnClose, m_exit)
				menuBar.Append(menu, "&File")

			
				menu = wx.Menu()
				m_about = menu.Append(wx.ID_ABOUT, "&About", "Information about this program")
				self.Bind(wx.EVT_MENU, self.OnAbout, m_about)
				menuBar.Append(menu, "&Help")

				self.SetMenuBar(menuBar)
				self.statusbar = self.CreateStatusBar()

				self.figure = Figure(figsize=(800 / 80.0, 600 / 80.0))
			
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
		
				#group ranges panel	
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
				self.plotMyData()
				#TODO why legend does not show first line plotted
				self.repaint()	


		def readFirstModel(self, event):
			print("goto first")
			self.currentModelNumberIndex = 0
			self.reloadModel()

		def readPrevModel(self, event):
			print("goto prev")
			if  self.currentModelNumberIndex >0:
				self.currentModelNumberIndex -=1
				self.reloadModel()

		def readNextModel(self, event):
			print("goto next")
			if  self.currentModelNumberIndex < len(self.modelNumbers)-1:
				self.currentModelNumberIndex +=1
				self.reloadModel()

		def readLastModel(self, event):
			print("goto last")
			self.currentModelNumberIndex = len(self.modelNumbers)-1
			self.reloadModel()

		def OnShowObjects(self, event):
			lastindex = 0	
			indices1 = []
			indices2 = []
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][1] == 1 and item[1][0] ):
					indices1+=range(lastindex, item[0])
				elif(item[1][1] == 2 and item[1][0]):
					indices2+=range(lastindex, item[0])
				lastindex = item[0]	
			if len(indices1)==0 or len(indices2)==0:
				return
			xcenter1 = np.mean(self.data[indices1,0])
			ycenter1 = np.mean(self.data[indices1,1])
			zcenter1 = np.mean(self.data[indices1,2])
			xcenter2 = np.mean(self.data[indices2,0])
			ycenter2 = np.mean(self.data[indices2,1])
			zcenter2 = np.mean(self.data[indices2,2])
			print("Center distance is %e" % ((xcenter1-xcenter2)**2 + (ycenter1-ycenter2)**2 + (zcenter1-zcenter2)**2)**0.5  )
			if not self.vector is None:
				self.vector.remove()
			self.vector = Arrow3D([xcenter1,xcenter2],[ycenter1,ycenter2],[zcenter1, zcenter2], mutation_scale=20, lw=2, arrowstyle="-|>", color="r")
			self.axes.add_artist(self.vector)
			self.repaint()
	
			
		#TODO center of mass distance
		def OnShowObjects2(self, event):
			lastindex = 0	
			indices1 = []
			indices2 = []
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][1] == 1 and item[1][0] ):
					indices1+=range(lastindex, item[0])
				elif(item[1][1] == 2 and item[1][0]):
					indices2+=range(lastindex, item[0])
				lastindex = item[0]	
			if len(indices1)==0 or len(indices2)==0:
				return
			xcenter1 = np.mean(self.data[indices1,0])
			ycenter1 = np.mean(self.data[indices1,1])
			zcenter1 = np.mean(self.data[indices1,2])
			xcenter2 = np.mean(self.data[indices2,0])
			ycenter2 = np.mean(self.data[indices2,1])
			zcenter2 = np.mean(self.data[indices2,2])
			print("Center distance is %e" % ((xcenter1-xcenter2)**2 + (ycenter1-ycenter2)**2 + (zcenter1-zcenter2)**2)**0.5  )
			if not self.vector is None:
				self.vector.remove()
			self.vector = Arrow3D([xcenter1,xcenter2],[ycenter1,ycenter2],[zcenter1, zcenter2], mutation_scale=20, lw=2, arrowstyle="-|>", color="r")
			self.axes.add_artist(self.vector)
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
			#print(self.data.shape)
			self.axes.cla()
			self.vector = None
			lastindex = 0
			indices = []
			for item in sorted(self.ranges.items()):
				if(item[1][0]):
					indices+=range(lastindex, item[0])
				lastindex = item[0]
					

			self.axes.plot(self.data[indices,0] ,  self.data[indices,1], self.data[indices,2], "ko", markersize=1, picker=1)
			#use mayavi package?
			#velInd = np.array(indices) + self.numpart
			#self.axes.quiver3d(self.data[indices,0] ,  self.data[indices,1], self.data[indices,2], self.data[velInd,0] , self.data[velInd,1], self.data[velInd,2])
			self.axes.set_xlabel('x')
			self.axes.set_ylabel('y')
			self.axes.set_zlabel('z')

class App(wx.App):

		def OnInit(self):
				'Create the main window and insert the custom frame'
				argv = sys.argv[1:]
				globPattern = "Model*.txt"
				frame = CanvasFrame(globPattern)
				frame.Show(True)

				return True





app = App(0)
app.MainLoop()
