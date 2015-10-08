"""ferryshp: Builds arcGIS shapefiles from the MOS Ferrybox database
Searches Ferrybox database for cruise IDs then builds a shapefile based on
user selection. uses ODBC connection. cruise ID's are cached in "cruises.dat"
Version 1.2 -Built using Python 2.7 by Tom Hull 2012
"""
import pyodbc
import shapefile
import pickle
from datetime import datetime as dt
import time

def toDecimalYear(date):
    def sinceEpoch(date): # returns seconds since epoch
        return time.mktime(date.timetuple())
    s = sinceEpoch

    year = date.year
    startOfThisYear = dt(year=year, month=1, day=1)
    startOfNextYear = dt(year=year+1, month=1, day=1)

    yearElapsed = s(date) - s(startOfThisYear)
    yearDuration = s(startOfNextYear) - s(startOfThisYear)
    fraction = yearElapsed/yearDuration

    return date.year + fraction

db = pyodbc.connect('DSN=Ferrybox')
cursor = db.cursor()
if cursor.tables(table='v_FerryBox_RQ0_DATA').fetchone():
    print 'Ferrybox view table found and connected'

def selectCruise():
    # loads cruises.dat list and offers list for selection
    print "loading list of cruise id's...\n"
    f = open("cruises.dat","r")
    cruisedict = pickle.load(f)
    for i in cruisedict:
        print i, cruisedict[i]
    selection = int(raw_input("Select Cruise for shapefile generation > "))
    selection = cruisedict[selection]
    print "you have selected", selection
    return(selection)

def updateCruises():
    # finds unique cruise id and returns them
    print "Generating list of cruise id's...\n"
    cruises = cursor.execute("SELECT DISTINCT [Cruise Id] as Cruise_Id from v_FerryBox_RQ0_DATA").fetchall()
    cruises.sort()
    # convert rowtype to dictionary for pickling
    cruisedict = {}
    for i in cruises:
        cruisedict[cruises.index(i)] = i.Cruise_Id
    for i in cruisedict:
        print i, cruisedict[i]
    # write to file
    f = open("cruises.dat","w")
    pickle.dump(cruisedict,f)
    f.close()
    print "cruise ID list saved to cruises.dat"

def build(cruise):
    print "Querying ferrybox for pump data for %s..." % cruise
    selectQuery = "SELECT [Cruise Id], Latitude, Longitude, [Date/Time] AS DateTime, [Parameter Code] FROM v_FerryBox_RQ0_DATA WHERE ([Cruise Id] = '%s') AND ([Parameter Code] = 'FWMAIN')" % cruise
    data = cursor.execute(selectQuery).fetchall()
    print "%r rows returned." % len(data)
    # opens a new instance of writer, defines type as point data
    shpWriter = shapefile.Writer(shapefile.POINT)
    # create dateTime fields
    shpWriter.field("DateTime")
    shpWriter.field("Decimal_date","N","40")
    for i in data:
        fmt = "%Y-%m-%d %H:%M:%S"
        # d = datetime.datetime.strptime(i.DateTime,fmt)
        decimal_date = toDecimalYear(i.DateTime)
        shpWriter.point(i.Longitude, i.Latitude)
        shpWriter.record(i.DateTime, decimal_date)
    savename = "./%s/%s" % (cruise, cruise)
    shpWriter.save(savename)
    print "Shapefile Saved."
    # build projection file
    prj = open("./%s/%s.prj" % (cruise, cruise), "w")
    epsg = 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'
    prj.write(epsg)
    prj.close()

option = raw_input("Update cruise ID list? Y/N -> ")
if option.lower() in ['y', 'yes']:
    updateCruises()
build(selectCruise())
