import pyodbc
import csv
import gzip

db = pyodbc.connect('DSN=Ferrybox')
cursor = db.cursor()
if cursor.tables(table='v_FerryBox_RQ0_DATA').fetchone():
    print 'Ferrybox view table found and connected'

print "Querying ferrybox for data"
selectQuery = (
    """
    SELECT
    [Date/Time] as dateTime,
    [Result - Mean] as result,
    [Parameter Code] as parameter,
    [Latitude], [Longitude],
    [Course], [Speed], [Heading], [Cruise Id] as Cruise_ID,
    [Sensor Description] as sensor
    FROM v_FerryBox_RQ0_Data
    WHERE
    [Parameter Code] IN ('TEMP')
    AND [Cruise Id] = 'CEND_13_12'
    ORDER BY dateTime
    """
)
data = cursor.execute(selectQuery).fetchall()
print "%r rows returned " % len(data)
print "writing file.."
with gzip.open("c:/data.csv.gz", 'wb', 5) as f:
    writer = csv.writer(f)
    writer.writerow(["dateTime", "result", "parameter", "Lat", "Long",
                     "course", "speed", "heading", "cruiseId", "sensor"])
    writer.writerows(data)
