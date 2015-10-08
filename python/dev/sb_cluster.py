import pyodbc

# db = pyodbc.connect("Driver=FreeTDS; Server=148.252.96.69; UID=SmartBuoyReader; PWD=H4mm3rt1m3; PORT=1433; believeNRows = TRUE")
db = pyodbc.connect("DSN=SmartbuoydbLive")
cursor = db.cursor()
if cursor.tables(table='v_BurstMean_QaData_SmartBuoy').fetchone():
    print 'BurstMean view table found and connected'

print "Querying SmartBuoydb for data"
selectQuery = (
    "SELECT "
    "[Date/Time] as dateTime, "
    "[Result - Mean] as result, "
    "[Parameter Code] as parcode "
    "FROM v_BurstMean_QaData_SmartBuoy "
    "WHERE "
    "[Parameter Code] IN ('SILICA') "
    "ORDER BY dateTime"
)
silicate = cursor.execute(selectQuery).fetchall()
print "%r rows returned " % len(silicate)
