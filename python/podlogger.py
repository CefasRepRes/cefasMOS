from __future__ import absolute_import, division, print_function
import numpy as np
import matplotlib.pyplot as plt
import Tkinter as Tk
# import pandas as pd
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
import datetime
import os
import serial
import time


if os.name == 'nt':
    from serial.tools.list_ports_windows import comports
elif os.name == 'posix':
    from serial.tools.list_ports_posix import comports
ports = sorted(comports())

selected_port = 'COM1'
tickrate = 500  # delay in ms

ser = serial.Serial(selected_port, 115200, timeout=1)
print(ser.name)

ser.write('$IDR\r\n')
idr_response = ser.readline()
obs_gain = {'0': 100, '1': 20, '2': 5, '3': 1}
print(idr_response)
if len(idr_response) > 0:
    ser.write('$PN\r\n')    # enable power
    print("Warming up...")
    time.sleep(5)
else:
    raise ValueError('no connection made to pod')

# make x and y arrays
xaxis = np.arange(0, 1, 1)
yaxis = np.array([0])

# build plot
fig = plt.figure(1)  # create new figure
ax = fig.add_subplot(111)  # 1x1 grid 1st subplot
ax.grid(True)  # turn on grid
ax.set_title("Realtime Plot %s" % idr_response)
ax.set_xlabel("Tick (%sHz)" % (1000/tickrate))
ax.set_ylabel("FTU")
line1 = ax.plot(xaxis, yaxis, '-')  # add line
values = [0]

# ts = pd.Series()


def random_gen():
    global values, tickrate, fo
    measurement = np.random.random()
    now = datetime.datetime.utcnow()
    values.append(measurement)
    print("%s,%s,%s\r\n" % (len(values), now.strftime("%Y-%m-%d %H:%M:%S"), measurement))
    root.after(tickrate, random_gen)


def realtime_plotter():
    global values, tickrate, wScale
    xWidth = wScale.get()
    if xWidth > len(values):
        xWidth = len(values)
        wScale.set(xWidth)
    currentXAxis = np.arange(len(values) - xWidth, len(values), 1)
    current_values = np.array(values[-xWidth:])
    line1[0].set_data(currentXAxis, current_values)
    ax.axis([currentXAxis.min(), currentXAxis.max(), 0, max(current_values)])
    canvas.draw()
    root.after(tickrate, realtime_plotter)


def pod_reader():
    global ser, values, tickrate, fo, obs_gain, filename
    ser.write('$SS\r\n')    # take sample
    now = datetime.datetime.utcnow()
    time.sleep(0.01)
    ser.write('$AD\r\n')    # request ASCII data
    ad_response = ser.readline()
    ad_response = ad_response.splitlines()
    if(len(ad_response) > 0):
        ad_response = ad_response[0]
        ad_response = ad_response.split(';')  # status;gain;value
        status = int(ad_response[0])
    else:
        status = 999
        print("%s  %s  %s\r" % (len(values),
                               now.strftime("%Y-%m-%d %H:%M:%S.%f"),
                                "bad reading"))
    if status == 0:
        gain = ad_response[1]
        adc_value = ad_response[2]
        volts = round(int(adc_value)/819, 3)  # accurate to 0.001v
        value = volts * obs_gain[gain]
        print("%s  %s  %s\r" % (len(values),
                               now.strftime("%Y-%m-%d %H:%M:%S.%f"),
                               value))
        values.append(value)
        with open(filename,"ab") as fo:
            fo.write("%s,%s,%s\r\n" % (len(values),
                                       now.strftime("%Y-%m-%d %H:%M:%S.%f"),
                                       value))
    root.after(tickrate, pod_reader)



def _quit():
    root.quit()
    root.destroy()  # cleanly quit

root = Tk.Tk()  # init UI
root.wm_title("Extended Realtime Plotter")

canvas = FigureCanvasTkAgg(fig, master=root)
canvas.get_tk_widget().pack(side=Tk.TOP, fill=Tk.BOTH, expand=1)

toolbar = NavigationToolbar2TkAgg(canvas, root)
toolbar.update()

wScale = Tk.Scale(root, label="View Width", from_=10, to=10*60*60, length=300, orient=Tk.HORIZONTAL)
wScale.pack()

filename = datetime.datetime.utcnow().strftime("%Y%m%d%H%M%S") + "_podlogger.txt"

root.protocol("WM_DELETE_WINDOW", _quit)  # for clean quit
root.after(tickrate, pod_reader)
root.after(tickrate, realtime_plotter)

Tk.mainloop()
fo.close()
