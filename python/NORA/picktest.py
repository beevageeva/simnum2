import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt

def line_picker(line, me):
    if me.xdata is None: return False, dict()    
    x, y = me.x, me.y
    xdata, ydata = line.axes.transData.transform(np.array(line.get_data()).T).T
    index = np.arange(len(xdata))
    index2 = np.linspace(0, index[-1], 2000)
    xdata2 = np.interp(index2, index, xdata)
    ydata2 = np.interp(index2, index, ydata)
    d = np.sqrt((xdata2-x)**2. + (ydata2-y)**2.)
    if np.min(d) < 5:
        return True, {}
    else:
        return False, {}

mpl.rcParams['legend.fontsize'] = 10

fig = plt.figure()
ax = fig.gca(projection='3d')
theta = np.linspace(-4 * np.pi, 4 * np.pi, 100)
z = np.linspace(-2, 2, 100)
r = z**2 + 1
x = r * np.sin(theta)
y = r * np.cos(theta)
line1 = ax.plot(x, y, z, label='parametric curve', picker=line_picker)[0]

t = np.linspace(-1, 1, 100) 
x = 4*np.sin(10*t)
y = 4*np.cos(10*t)
z = t**2*5-3

line2 = ax.plot(x, y, z, label="second", picker=line_picker)[0]

ax.legend()

def onpick(event):
    print [line1, line2].index(event.artist)

fig.canvas.mpl_connect('pick_event', onpick)

plt.show()
