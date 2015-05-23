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

import wx, wx.html, sys,getopt


useCalcValues = True





def plot2d(values, title):
	import matplotlib.pyplot as plt
	plotLegend = False
	f = plt.figure(1)
	f.suptitle(title)
	plt.cla()
	#plt.title(title)
	#TODO this is a crap
	isMultArr = hasattr(values[0][0], "__len__")
	if(isMultArr):
		for i in range(len(values)):
			if(len(values[i])==3):	
				plt.plot(values[i][0], values[i][1], label=values[i][2])
				plotLegend = True
			else:
				plt.plot(values[i][0], values[i][1])
	else:	
		plt.plot(values[0], values[1])
	plt.draw()
	plt.show(block=False)
	if plotLegend:
		#plt.legend(loc='upper center', bbox_to_anchor=(1, 0.5))
		#plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc=3,ncol=2, mode="expand", borderaxespad=0.).draggable()
		plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc=3).draggable()



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
			self.parentFrame.ranges[self.rangeValue][1] = int(self.cb.GetValue())
			self.parentFrame.plotMyData()
			self.parentFrame.repaint()	


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
		#sliders for mult am and vel
		sizer.Add(wx.StaticText(self, -1, "Mult vel and am:"))
		slider1 = wx.Slider(self, -1, 10, 0, 100, size=(150,50),style = wx.SL_HORIZONTAL | wx.SL_AUTOTICKS | wx.SL_LABELS)
		slider1.Bind(wx.EVT_SLIDER, parentFrame.sliderUpdateVel)
		slider2 = wx.Slider(self, -1, 100, 0, 1000, size=(150,50), style = wx.SL_HORIZONTAL | wx.SL_AUTOTICKS | wx.SL_LABELS)
		slider2.Bind(wx.EVT_SLIDER, parentFrame.sliderUpdateAM)
		sizer.Add(slider1)
		sizer.Add(slider2)

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
				self.multAM = 100
				self.multVel = 10
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
				self.Bind(wx.EVT_MENU, self.OnCenterDistance, m_showObjects)
				m_showObjects = menu.Append(-1, "Center of mass distance", "Center of mass distance")
				self.Bind(wx.EVT_MENU, self.OnCenterMassDistance, m_showObjects)

				m_showObjects = menu.Append(-1, "Angular moment 1", "Angular moment 1")
				self.Bind(wx.EVT_MENU, self.OnAngMom1, m_showObjects)
				m_showObjects = menu.Append(-1, "Angular moment 2", "Angular moment 2")
				self.Bind(wx.EVT_MENU, self.OnAngMom2, m_showObjects)
				m_showObjects = menu.Append(-1, "Total Angular moment", "Total Angular moment ")
				self.Bind(wx.EVT_MENU, self.OnAngMomTotal, m_showObjects)

				m_exit = menu.Append(wx.ID_EXIT, "E&xit\tAlt-X", "Close window and exit program.")
				self.Bind(wx.EVT_MENU, self.OnClose, m_exit)
				menuBar.Append(menu, "&Each")

				menu = wx.Menu()
				m_showObjects = menu.Append(-1, "Angular moment 1", "Angular moment 1")
				self.Bind(wx.EVT_MENU, self.OnAngMomCalc1, m_showObjects)
				m_showObjects = menu.Append(-1, "Angular moment 2", "Angular moment 2")
				self.Bind(wx.EVT_MENU, self.OnAngMomCalc2, m_showObjects)
				m_showObjects = menu.Append(-1, "Angular moment both", "Angular moment both")
				self.Bind(wx.EVT_MENU, self.OnAngMomCalcTotal, m_showObjects)
				m_showObjects = menu.Append(-1, "Center distance", "Center distance")
				self.Bind(wx.EVT_MENU, self.OnCenterDistanceCalc, m_showObjects)
				m_showObjects = menu.Append(-1, "Center of mass distance", "Center of mass distance")
				self.Bind(wx.EVT_MENU, self.OnCenterOfMassDistanceCalc, m_showObjects)
				m_showObjects = menu.Append(-1, "Center distance compare", "Center distance compare")
				self.Bind(wx.EVT_MENU, self.OnCenterDistanceCalcCompare, m_showObjects)
				m_showObjects = menu.Append(-1, "Ek1", "Ek1")
				self.Bind(wx.EVT_MENU, self.OnEkCalc1, m_showObjects)
				m_showObjects = menu.Append(-1, "Ek2", "Ek2")
				self.Bind(wx.EVT_MENU, self.OnEkCalc2, m_showObjects)
				m_showObjects = menu.Append(-1, "Ek total", "Ek total")
				self.Bind(wx.EVT_MENU, self.OnEkCalcTotal, m_showObjects)
				m_showObjects = menu.Append(-1, "Make images", "Make images")
				self.Bind(wx.EVT_MENU, self.OnMakeImages, m_showObjects)
				menuBar.Append(menu, "&All (calc)")
		
				menu = wx.Menu()
				m_showObjects = menu.Append(-1, "Angular moment 1", "Angular moment 1")
				self.Bind(wx.EVT_MENU, self.OnAngMomExt1, m_showObjects)
				m_showObjects = menu.Append(-1, "Angular moment 2", "Angular moment 2")
				self.Bind(wx.EVT_MENU, self.OnAngMomExt2, m_showObjects)
				m_showObjects = menu.Append(-1, "Angular moment both", "Angular moment both")
				self.Bind(wx.EVT_MENU, self.OnAngMomExtAll, m_showObjects)
				m_showObjects = menu.Append(-1, "Center distance", "Center distance")
				self.Bind(wx.EVT_MENU, self.OnCenterDistanceExt, m_showObjects)
				m_showObjects = menu.Append(-1, "Center of mass distance", "Center of mass distance")
				self.Bind(wx.EVT_MENU, self.OnCenterOfMassDistanceExt, m_showObjects)
				m_showObjects = menu.Append(-1, "Center distance compare", "Center distance compare")
				self.Bind(wx.EVT_MENU, self.OnCenterDistanceExtCompare, m_showObjects)
				m_showObjects = menu.Append(-1, "Energy", "Energy")
				self.Bind(wx.EVT_MENU, self.OnExtEnergy, m_showObjects)
				m_showObjects = menu.Append(-1, "Virial Th.", "Virial Th.")
				self.Bind(wx.EVT_MENU, self.OnExtVirialTh, m_showObjects)
				m_showObjects = menu.Append(-1, "Treelog AM", "Treelog AM")
				self.Bind(wx.EVT_MENU, self.OnExtTreelogAM, m_showObjects)
				m_showObjects = menu.Append(-1, "Treelog cmpos", "Treelog cmpos")
				self.Bind(wx.EVT_MENU, self.OnExtTreelogCMPos, m_showObjects)
				m_showObjects = menu.Append(-1, "Treelog cmvel", "Treelog cmvel")
				self.Bind(wx.EVT_MENU, self.OnExtTreelogCMVel, m_showObjects)
				m_showObjects = menu.Append(-1, "RM1", "RM1")
				self.Bind(wx.EVT_MENU, self.OnExtRm1, m_showObjects)
				m_showObjects = menu.Append(-1, "RM2", "RM2")
				self.Bind(wx.EVT_MENU, self.OnExtRm2, m_showObjects)

				menuBar.Append(menu, "&All (ext)")


				menu = wx.Menu()
				m_showObjects = menu.Append(-1, "Angular moment 1", "Angular moment 1")
				self.Bind(wx.EVT_MENU, self.OnAngMomCompare1, m_showObjects)
				m_showObjects = menu.Append(-1, "Angular moment 2", "Angular moment 2")
				self.Bind(wx.EVT_MENU, self.OnAngMomCompare2, m_showObjects)
				m_showObjects = menu.Append(-1, "Angular moment both", "Angular moment both")
				self.Bind(wx.EVT_MENU, self.OnAngMomCompareAll, m_showObjects)
				m_showObjects = menu.Append(-1, "Center distance", "Center distance")
				self.Bind(wx.EVT_MENU, self.OnCenterDistanceCompare, m_showObjects)
				m_showObjects = menu.Append(-1, "Center of mass distance", "Center of mass distance")
				self.Bind(wx.EVT_MENU, self.OnCenterOfMassDistanceCompare, m_showObjects)
				m_showObjects = menu.Append(-1, "Ek", "Ek")
				self.Bind(wx.EVT_MENU, self.OnEkCompare, m_showObjects)

				menuBar.Append(menu, "&All (comp)")


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
#				# On Windows platform, default window size is incorrect, so set
#				# toolbar width to figure width.
#				tw, th = self.toolbar.GetSizeTuple()
#				fw, fh = self.canvas.GetSizeTuple()
#				# By adding toolbar in sizer, we are able to put it at the bottom
#				# of the frame - so appearance is closer to GTK version.
#				# As noted above, doesn't work for Mac.
#				self.toolbar.SetSize(wx.Size(fw, th))
				vsizer.Add(self.toolbar, 0, wx.LEFT | wx.EXPAND)
				self.SetToolBar(self.toolbar)
				# update the axes menu on the toolbar
				self.toolbar.update()

				self.SetSizer(self.sizer)
				hsizer.Layout()
				hpanel.Fit()
				self.sizer.Layout()
				self.Fit()
				#TODO not hardcode this
				self.SetSize((1000,800))
				#changed toolbar?
				#self.SetSize((1000,853))
				self.plotMyData()
				#TODO why legend does not show first line plotted
				self.repaint()	

		def sliderUpdateVel(self, event):
			print("Update vel slider")
			self.multVel = float(event.GetEventObject().GetValue())

		def sliderUpdateAM(self, event):
			print("update am slider")
			self.multAM = float(event.GetEventObject().GetValue())




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


		#select indices begin

		def  selectObject(self, objNum, testVis = True):
			lastindex = 0	
			indices1 = []
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][1] == objNum and ((testVis and item[1][0]) or not testVis) ):
					indices1+=range(lastindex, item[0])
				lastindex = item[0]
			return indices1

		def  selectObjects(self, testVis = True, returnStr = False):
			lastindex = 0	
			indices1 = []
			indices2 = []
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][1] == 1 and ((testVis and item[1][0]) or not testVis) ):
					indices1+=range(lastindex, item[0])
				elif(item[1][1] == 2 and ((testVis and item[1][0]) or not testVis) ):
					indices2+=range(lastindex, item[0])
				lastindex = item[0]
			return indices1, indices2

		def  selectObjectsAll(self, testVis = True, returnStr = False):
			lastindex = 0	
			indices1 = []
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if( (item[1][0] and testVis) or not testVis):
					indices1+=range(lastindex, item[0])
				lastindex = item[0]
			return indices1

		#select indices end


		#select indices as str begin

		def  selectObjectStr(self, objNum, testVis = True):
			indStr = ""
			for item in sorted(self.ranges.items()):
				if(item[1][1] == objNum and ((testVis and item[1][0]) or not testVis) ):
					indStr+="-%s" % str(item[0])
			return indStr

		def  selectObjectsStr(self, testVis = True):
			indStr = ""
			for item in sorted(self.ranges.items()):
				if((testVis and item[1][0]) or not testVis) :
					indStr+="-%s" % str(item[0])
			return indStr


		#select indices as str end

		#center begin
		def getCenter(self, indices1):
			#TODO needed?
			if len(indices1)==0 :
				return [0,0,0]
			xcenter = np.mean(self.data[indices1,0])
			ycenter = np.mean(self.data[indices1,1])
			zcenter = np.mean(self.data[indices1,2])
			return np.array([xcenter,ycenter,zcenter])	

		#center END

		#center of mass
		def  centerOfMass(self, objNum):
			lastindex = 0	
			m1 = 0
			mtot = 0
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][1] == objNum and item[1][0] ):
					m1+=self.data[lastindex:item[0],:].sum(axis=0) * item[1][2]
					mtot+=(item[0] - lastindex) * item[1][2]
				lastindex = item[0]
			return m1/mtot

		def  centerOfMassBoth(self):
			lastindex = 0	
			m1 = 0
			m2 = 0
			mtot1 = 0
			mtot2 = 0
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][1] == 1 and item[1][0] ):
					m1+=self.data[lastindex:item[0],:].sum(axis=0) * item[1][2]
					mtot1+=(item[0] - lastindex) * item[1][2]
				elif(item[1][1] == 2 and item[1][0]):
					m2+=self.data[lastindex:item[0],:].sum(axis=0) * item[1][2]
					mtot2+=(item[0] - lastindex) * item[1][2]
				lastindex = item[0]
			return m1/mtot1, m2/mtot2

		def  centerOfMassAll(self):
			lastindex = 0	
			m1 = 0
			mtot = 0
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][0]):
					m1+=self.data[lastindex:item[0],:].sum(axis=0) * item[1][2]
					mtot+=(item[0] - lastindex) * item[1][2]
				lastindex = item[0]
			return m1/mtot

		#center of mass END


		#angular mom
		def  angMom(self, objNumber, testVis = True):
			lastindex = 0
			am = np.zeros(3)		
			cm = self.centerOfMass(objNumber)	
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if(item[1][1] == objNumber and ( (testVis and item[1][0]) or not testVis) ):
					massPart = item[1][2]	
					indices =np.arange(lastindex, item[0])
					#print("AM")
					#print(am.shape)
					am+=massPart * np.cross(self.data[indices] - cm , self.data[indices + self.numpart]).sum(axis=0)
				lastindex = item[0]
			return am
		
		def  angMomTotal(self, testVis = True):
			lastindex = 0
			am = np.zeros(3)		
			cm = self.centerOfMassAll()
			for item in sorted(self.ranges.items()):
				#print("Object is %d" % item[1][1])
				if((item[1][0] and testVis) or not testVis):
					massPart = item[1][2]	
					indices =np.arange(lastindex, item[0])
					am+=massPart * np.cross(self.data[indices] - cm, self.data[indices + self.numpart]).sum(axis=0)
				lastindex = item[0]
			return am

		#angular mom end


		#Ek start

		def  ek(self, objNumber):
			lastindex = 0
			ek = 0
			for item in sorted(self.ranges.items()):
				if(item[1][1] == objNumber and item[1][0] ):
					massPart = item[1][2]	
					indices =np.arange(lastindex, item[0])
					ek+=massPart * np.sum(self.data[indices + self.numpart,0]**2 + self.data[indices + self.numpart,1]**2 + self.data[indices + self.numpart,2]**2  )
				lastindex = item[0]
			return ek * 0.5

		def  ekAll(self, testVis=True):
			lastindex = 0
			ek = 0
			for item in sorted(self.ranges.items()):
				if( (item[1][0] and testVis) or not testVis):
					massPart = item[1][2]	
					indices =np.arange(lastindex, item[0])
					ek+=massPart * np.sum(self.data[indices + self.numpart,0]**2 + self.data[indices + self.numpart,1]**2 + self.data[indices + self.numpart,2]**2  )
				lastindex = item[0]
			return ek * 0.5


		#Ek end



		def performForAllModels(self, f, txtfn):
			print("PERF MODELS useCalcValues = %s" % str(useCalcValues))
			from os.path import isfile
			if useCalcValues and isfile(txtfn):
				print("Using file %s" % txtfn)
				return np.loadtxt(txtfn)
			mydata = self.data
			norapyam = np.zeros(len(self.modelNumbers))
			i=0
			for i in range(len(self.modelNumbers)):
				filename = self.globPattern.replace("*", str(self.modelNumbers[i]) )
				print("FILENAME %s" % filename)
				self.data = np.loadtxt(filename, skiprows=1+self.numpart)
				norapyam[i] = f()

			self.data = mydata
			if useCalcValues:
				print("Saving file %s" % txtfn)
				np.savetxt(txtfn, norapyam)
			return norapyam

		def calcAngMom1(self, testVis=True):
			def func():
				#print("CALLING FUNC")
				am = self.angMom(1, testVis)
				return np.sqrt(am[0]**2+am[1]**2+am[2]**2)
			return self.performForAllModels(func, "am1%s.txt" % (self.selectObjectStr(1, testVis)))


		def calcAngMom2(self, testVis = True):
			def func():
				#print("CALLING FUNC")
				am = self.angMom(2, testVis)
				return np.sqrt(am[0]**2+am[1]**2+am[2]**2)
			return self.performForAllModels(func, "am2%s.txt" % (self.selectObjectStr(2, testVis)))


		def calcAngMomTotal(self, testVis = True):
			def func():
				#print("CALLING FUNC")
				am = self.angMomTotal(testVis)
				return np.sqrt(am[0]**2+am[1]**2+am[2]**2)
			return self.performForAllModels(func, "am3%s.txt" % (self.selectObjectsStr( testVis)))


		def calcCenterDistance(self): 
			indices1, indices2 = self.selectObjects()
			def func():	
				center1 = self.getCenter(indices1)	
				center2 = self.getCenter(indices2)	
				return  np.sqrt((center1[0] - center2[0] )**2 + (center1[1] - center2[1] )**2 + (center1[2] - center2[2])**2)
			return self.performForAllModels(func, "medcent%s.txt" % ( self.selectObjectsStr()  ))

		def calcCenterOfMassDistance(self): 
			def func():	
				center1, center2  = self.centerOfMassBoth()
				return  np.sqrt((center1[0] - center2[0] )**2 + (center1[1] - center2[1] )**2 + (center1[2] - center2[2])**2)
			return self.performForAllModels(func, "cmcent%s.txt" % (  self.selectObjectsStr() ))

		def calcEk1(self):
			return self.performForAllModels(lambda: self.ek(1), "ek1%s.txt" % (self.selectObjectStr(1) ))

		def calcEk2(self):
			return self.performForAllModels(lambda: self.ek(2), "ek2%s.txt" % ( self.selectObjectStr(2) ))

		def calcEkTotal(self, testVis = True):
			return self.performForAllModels(lambda: self.ekAll(testVis), "ek3%s.txt" % ( self.selectObjectsStr( testVis)  ))

		def plotRm(self, objNumber):	
			files = ["0.20" , "0.50", "0.80", "0.90", "0.99"]
			indices = self.selectObject(objNumber)
			from extern import getrm
			outFolder = getrm(self.modelNumbers, min(indices)+1, max(indices)+1)
			plotData = []
			import os.path
			for f in files:
				data = np.loadtxt(os.path.join(outFolder,f))
				plotData.append([data[:,0], data[:,1], f])
			plot2d(plotData,"RM%d" % objNumber)

		def OnExtRm1(self, event):
			self.plotRm(1)
	
		def OnExtRm2(self, event):
			self.plotRm(2)

		def OnMakeImages(self, event):
			import os
			#TODO do not display  images : no repaint would work?
			#matplotlib.use('Agg')
			def createFolder(dirname_base="out"):
				dirExists = True
				i = 0
				dirname = "%s_%i" % (dirname_base, i)
				while os.path.exists(dirname):
					i +=1
					dirname = "%s_%i" % (dirname_base, i)
				os.mkdir(dirname)
				return dirname
			dirname = createFolder("outImages")
			oldCNI = self.currentModelNumberIndex
			for i in range(len(self.modelNumbers)):
				self.currentModelNumberIndex = i
				self.reloadModel()
				#something has changed(toolbar?) and imgs will be 1000x697!
				#self.figure.savefig(os.path.join(dirname, "Fig%03d" % self.currentModelNumberIndex)) 
				self.figure.savefig(os.path.join(dirname, "Fig%03d" % self.currentModelNumberIndex), figsize=(12.5,9.375), dpi=80) 
			self.currentModelIndex = oldCNI
			self.reloadModel()
			#matplotlib.use('WXAgg')


		def OnAngMom1(self, event):
			center = self.getCenter(self.selectObject(1))
			am = self.angMom(1)
			print("AM1 ")
			print(am)
			self.drawVector(center, center + self.multAM * am)

		def OnAngMomCalc1(self, event):
			import matplotlib.pyplot as plt
			plt.figure(1)
			plt.title("AM1")
			plt.plot(range(len(self.modelNumbers)), self.calcAngMom1(), 'k')
			plt.draw()
			plt.show(block=False)
			
		def OnAngMomCalc2(self, event):
			import matplotlib.pyplot as plt
			plt.figure(1)
			plt.cla()
			plt.title("AM2")
			plt.plot(range(len(self.modelNumbers)), self.calcAngMom2(), 'k')
			plt.draw()
			plt.show(block=False)

		def OnAngMomCalcTotal(self, event):
			import matplotlib.pyplot as plt
			plt.figure(1)
			plt.cla()
			plt.title("AM Total")
			plt.plot(range(len(self.modelNumbers)), self.calcAngMomTotal(), 'k')
			plt.draw()
			plt.show(block=False)


		def OnEkCalc1(self, event):
			plot2d([range(len(self.modelNumbers)), self.calcEk1()], "Ek1" )

		def OnEkCalc2(self, event):
			plot2d([range(len(self.modelNumbers)), self.calcEk2()], "Ek2")

		def OnEkCalcTotal(self,event):
			plot2d([range(len(self.modelNumbers)), self.calcEkTotal()], "Ek total")


		def OnEkCompare(self, event):
			from extern import getTreelogE
			ee = getTreelogE()	
			plot2d([[range(len(self.modelNumbers)), self.calcEkTotal(False), "calc"], [ee[0], ee[1][:,1], "treelog"]], "Ek total compare")




		def OnExtEnergy(self, event):
			from extern import getTreelogE
			res = getTreelogE()	
			plot2d([ [res[0], res[1][:,0] , "Et"],  [res[0], res[1][:,1] , "Ek"], [res[0], res[1][:,2] , "Ep"]], "Energies from TREELOG")

		def OnExtVirialTh(self, event):
			from extern import getTreelogE
			res = getTreelogE()	
			plot2d([res[0], 2 * res[1][:,1] + res[1][:,2]], "Virial Th from TREELOG(2Ek+Ep)")


		def OnExtTreelogAM(self, event):
			from extern import getTreelogAM
			res = getTreelogAM()
			am = np.sqrt(res[1][:,0]**2 + res[1][:,1]**2 + res[1][:,2]**2)	
			#plot without AM **2
			plot2d([res[0], am], "Treelog am")
			#plot WITH AM **2
#			from extern import getAngMomAll
#			restam = getAngMomAll()
#			amtam = np.sqrt(restam[1]**2 + restam[2]**2 + restam[3]**2)
#			plot2d([[res[0], am, "TREELOG"],[restam[0],amtam, "TREEAM"]], "Ext AM")
			

		def OnExtTreelogCMPos(self, event):
			from extern import getTreelogCMPos
			res = getTreelogCMPos()
			am = np.sqrt(res[1][:,0]**2 + res[1][:,1]**2 + res[1][:,2]**2)	
			#plot WITHOUT treelog	
			#plot2d([res[0], am], "Distance CM from origin")
			#plot WITH treelog	
			from extern import getCMCenterAll
			plot2d([[range(len(self.modelNumbers)), getCMCenterAll(self.modelNumbers), "NORA"], [res[0], am, "TREELOG"]], "Ext CMPos")
			
		def OnExtTreelogCMVel(self, event):
			from extern import getTreelogCMVel
			res = getTreelogCMVel()
			am = np.sqrt(res[1][:,0]**2 + res[1][:,1]**2 + res[1][:,2]**2)	
			plot2d([res[0], am], "CM velocity from treelog")



		#TODO repeated code in the following 3 functions			
		def OnAngMomCompare1(self, event):
			print("AM COMPARE")
			from extern import getAngMom
			noraam = getAngMom(0)	
			plot2d([[range(len(self.modelNumbers)), self.calcAngMom1(False), "calc"],[noraam[0], noraam[1], "TREEAM"]], 'AM1 Compare')

		def OnAngMomExt1(self, event):
			print("AM EXT")
			from extern import getAngMom
			noraam = getAngMom(0)	
			plot2d([noraam[0], noraam[1]], 'AM1 from TREEAM')

		def OnAngMomCompare2(self, event):
			print("AM COMPARE 2")
			from extern import getAngMom
			noraam = getAngMom(1)	
			plot2d([[range(len(self.modelNumbers)), self.calcAngMom2(False), "calc"],[noraam[0], noraam[1], "TREEAM"]], 'AM2 Compare')

		def OnAngMomExt2(self, event):
			print("AM EXT 2")
			from extern import getAngMom
			noraam = getAngMom(1)	
			plot2d([noraam[0], noraam[1]], 'AM2 from TREEAM')


		def OnAngMomCompareAll(self, event):
			print("AM COMPARE TOTAL")

			#COMPARE with treeam
			from extern import getAngMom
			noraam = getAngMom(2)	
			plot2d([[range(len(self.modelNumbers)), self.calcAngMomTotal(False), "calc"],[noraam[0], noraam[1], "TREEAM"] ], "AM Total Comp")

#			#compare with treelog
#			from extern import getTreelogAM
#			res = getTreelogAM()
#			am = np.sqrt(res[1][:,0]**2 + res[1][:,1]**2 + res[1][:,2]**2)
#			plot2d([[range(len(self.modelNumbers)), self.calcAngMomTotal(False), "calc"],[res[0], am, "TREELOG"] ], "AM comp")
#			#selfam = np.sqrt(self.calcAngMomTotal(False)**2 + self.calcAngMom1(False)**2 + self.calcAngMom2(False)**2 )	
#			#plot2d([[range(len(self.modelNumbers)), selfam, "calc"],[res[0], am, "TREELOG"] ], "AM comp")


		def OnAngMomExtAll(self, event):
			print("AM EXT")
			from extern import getAngMom
			noraam = getAngMom(2)	
			plot2d([noraam[0], noraam[1]], 'AM Total from TREEAM')

		def  OnCenterDistanceCalc(self, event):
			print("MEDCENT CALC")
			plot2d([range(len(self.modelNumbers)), self.calcCenterDistance()], "Medcent")

		def  OnCenterOfMassDistanceCalc(self, event):
			print("CMENT CALC")
			plot2d([range(len(self.modelNumbers)), self.calcCenterOfMassDistance()], "CM")

		def  OnCenterDistanceCalcCompare(self, event):
			print("Center Distance CALC COMPARE")
			plot2d([[range(len(self.modelNumbers)), self.calcCenterDistance(), "Medcent"], [range(len(self.modelNumbers)), self.calcCenterOfMassDistance(), "CM"]], "Center distance compare")


		def  OnCenterDistanceExt(self, event):
			print("MEDCENT EXT")
			indices1, indices2 = self.selectObjects()
			from extern import getMedcent
			plot2d([range(len(self.modelNumbers)), getMedcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1)], "medcent Nora" )

		def  OnCenterDistanceCompare(self, event):
			print("MEDCENT COMPARE")
			indices1, indices2 = self.selectObjects()
			from extern import getMedcent
			#plot WITHOUT treeorb dist
			plot2d([[range(len(self.modelNumbers)), self.calcCenterDistance(), "calc" ], [range(len(self.modelNumbers)),getMedcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1), "nora medcent"] ], "medcent compare")
			#plot WITH treeorb dist
			#from extern import getTreeorbDist
			#d3 = getTreeorbDist()
			#plot2d([[range(len(self.modelNumbers)), self.calcCenterDistance(), "calc" ], [range(len(self.modelNumbers)),getMedcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1), "nora medcent"] , [d3[0], d3[1], "Treeorb"] ], "medcent compare")



		def  OnCenterOfMassDistanceExt(self, event):
			print("CMCENT EXT")
			indices1, indices2 = self.selectObjects()
			from extern import getCMcent
			plot2d([range(len(self.modelNumbers)), getCMcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1)], "CM Nora" )


		def  OnCenterOfMassDistanceCompare(self, event):
			print("CMCENT COMPARE")
			from extern import getCMcent
			indices1, indices2 = self.selectObjects()
			#plot WITHOUT treeorb dist
			#plot2d([[range(len(self.modelNumbers)), self.calcCenterOfMassDistance(), "calc"],[range(len(self.modelNumbers)), getCMcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1), "nora"]], "CM compare" )
			#plot WITH treeorb dist
			from extern import getTreeorbDist
			d3 = getTreeorbDist()
			plot2d([[range(len(self.modelNumbers)), self.calcCenterOfMassDistance(), "calc" ], [range(len(self.modelNumbers)),getCMcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1), "nora"] , [d3[0], d3[1], "Treeorb"] ], "CM compare")

		def OnCenterDistanceExtCompare(self, event):
			print("EXT center distance COMPARE")
			indices1, indices2 = self.selectObjects()
			from extern import getMedcent, getCMcent
			plot2d([[range(len(self.modelNumbers)), getMedcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1), "medcent"],[range(len(self.modelNumbers)), getCMcent(self.modelNumbers, min(indices1)+1, max(indices1)+1, min(indices2)+1, max(indices2)+1), "cmcent"]], "NORA center distance" )


		#END	






		def OnAngMom2(self, event):
			center = self.getCenter(self.selectObject(2))
			am = self.angMom(2)
			print("AM ")
			print(am)
			self.drawVector(center, center + self.multAM * am)

		def OnAngMomTotal(self, event):
			center = self.getCenter(self.selectObjectsAll())
			am = self.angMomTotal()
			print("AM ")
			print(am)
			self.drawVector(center, center + slef.multAM * am)


		def drawVector(self, v1, v2):
			if not self.vector is None:
				self.vector.remove()
			self.vector = Arrow3D([v1[0],v2[0]],[v1[1], v2[1]],[v1[2], v2[2]], mutation_scale=20, lw=2, arrowstyle="-|>", color="r")
			self.axes.add_artist(self.vector)
			self.repaint()



		def OnCenterDistance(self, event):
			indices1, indices2 = self.selectObjects()
			center1 = self.getCenter(indices1)	
			center2 = self.getCenter(indices2)	
			print("Center distance is %e" % ((center1[0] - center2[0] )**2 + (center1[1] - center2[1] )**2 + (center1[2] - center2[2])**2)**0.5  )
			self.drawVector(center1, center2)
	
			
		def OnCenterMassDistance(self, event):
			center1, center2 = self.centerOfMassBoth()
			print("Center of mass distance is %e" % ((center1[0] - center2[0] )**2 + (center1[1] - center2[1] )**2 + (center1[2] - center2[2])**2)**0.5  )
			self.drawVector(center1, center2)


	
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
					self.drawVector([x[ind],y[ind], z[ind]],[x[ind] +self.multVel* self.data[self.numpart+i,0], y[ind]+self.multVel* self.data[self.numpart+i,1], z[ind]+self.multVel*self.data[self.numpart+i,2] ] )
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

			
		def plotMyData(self):
			#print(self.data.shape)
			self.axes.cla()
			#TODO grid	
			self.axes.grid(False)
			self.vector = None
					
			indices1, indices2 = self.selectObjects()
			#print("indices 1")
			#print(indices1)	
			#print("indices 2")
			#print(indices2)	

			x1 = self.data[indices1,0]
			x2 = self.data[indices2,0]
			y1 = self.data[indices1,1]
			y2 = self.data[indices2,1]
			z1 = self.data[indices1,2]
			z2 = self.data[indices2,2]

			#plot points			
			self.axes.plot(x1, y1, z1,  "go", markersize=1, picker=1)
			self.axes.plot(x2 , y2, z2, "ro", markersize=1, picker=1)


			#use mayavi package?
			#velInd = np.array(indices) + self.numpart
			#self.axes.quiver3d(self.data[indices,0] ,  self.data[indices,1], self.data[indices,2], self.data[velInd,0] , self.data[velInd,1], self.data[velInd,2])
#			#matplotlib 1.4
#			N = 100
#			indices = np.array(indices1 + indices2)
#			#USE MEAN N VALUES
##			velInd = indices + self.numpart
##			c1 = self.data[indices,:].reshape(-1, N, 3)
##			b1 = np.mean(c1, axis=1)
##			c2 = self.data[velInd,:].reshape(-1, N, 3)
##			b2 = np.mean(c2, axis=1)
##			self.axes.quiver(b1[:,0] ,  b1[:,1], b1[:,2], b2[:,0] , b2[:,1], b2[:,2])
#
#			#USE EVERY N value
#			indices = indices[::N]
#			velInd = indices + self.numpart
#			self.axes.quiver(self.data[indices,0] ,  self.data[indices,1], self.data[indices,2], self.data[velInd,0] , self.data[velInd,1], self.data[velInd,2])
#			#end matplotlib 1.4


			self.axes.set_xlabel('x')
			self.axes.set_ylabel('y')
			self.axes.set_zlabel('z')
			#TODO aurtoscale
			#self.figure.tight_layout()	
			#self.axes.autoscale(True)
			

			def maxa(a1,a2):
				if(a1.size == 0 and a2.size == 0):
					return 0
				elif(a1.size == 0):
					return a2.max()
				elif(a2.size==0):
					return a1.max()
				else:
					return max(a1.max(), a2.max())

			def mina(a1,a2):
				if(a1.size == 0 and a2.size == 0):
					return 0
				elif(a1.size == 0):
					return a2.min()
				elif(a2.size==0):
					return a1.min()
				else:
					return min(a1.min(), a2.min())

			def meana(a1, a2):
				if(a1.size == 0 and a2.size == 0):
					return 0
				elif(a1.size == 0):
					return a2.mean()
				elif(a2.size==0):
					return a1.mean()
				else:
					return 0.5 * (a1.mean() +  a2.mean())

			max_range = np.array([maxa(x1, x2)-mina(x1, x2), maxa(y1, y2)-mina(y1, y2), maxa(z1, z2)-mina(z1, z2)]).max() / 2.0
			mean_x = meana(x1,x2)
			mean_y = meana(y1,y2)
			mean_z = meana(z1,z2)
			self.axes.set_xlim(mean_x - max_range, mean_x + max_range)
			self.axes.set_ylim(mean_y - max_range, mean_y + max_range)
			self.axes.set_zlim(mean_z - max_range, mean_z + max_range)

			self.axes.set_title("Model %d" % self.currentModelNumberIndex)

			#self.axes.set_aspect(1)

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
