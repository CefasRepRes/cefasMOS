import pyodbc
import csv
import gzip

db = pyodbc.connect('DSN=SmartbuoydbLive')
cursor = db.cursor()

print "Querying SmartbuoydbLive for data"
selectQuery = (
    "SELECT Result_TH1_108.DepSensorId, Result_TH1_108.ParCode, Result_TH1_108.ResultTime, Result_TH1_108.BurstNumber, Result_TH1_108.ResultValue, Result_TH1_108.ResultValueQA, Result_TH1_108.ResultFlag, Result_TH1_108.ResultQuality FROM SmartBuoy.SmartBuoyUser.Result_TH1_108 Result_TH1_108 WHERE (Result_TH1_108.DepSensorId=7650) OR (Result_TH1_108.DepSensorId=7645) ORDER BY Result_TH1_108.BurstNumber"
)
data = cursor.execute(selectQuery).fetchall()
print "%r rows returned " % len(data)
print "writing file.."
with gzip.open("c:/sb_data.csv.gz", 'wb', 5) as f:
    writer = csv.writer(f)
    writer.writerow(["DepSensorId","ParCode","ResultTime","BurstNumber","ResultValue","ResultValueQA","ResultFlag","ResultQuality"])
    writer.writerows(data)

# with R
# reshape(x, v.names="result", idvar="dateTime", timevar="parameter", direction = "wide")
    #"AND [Date/Time] BETWEEN '2010-01-01 00:00:00' AND '2012-12-31 00:00:00' "
    #"AND ([Sensor Description] LIKE '%Optode%' OR [Sensor Description] LIKE 'Seapoint%') "
