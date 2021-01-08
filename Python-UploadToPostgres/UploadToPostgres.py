import sys
import os
import logging as log
import datetime
import pandas as pd
import json
import psycopg2
from sqlalchemy import create_engine
import io

#Global Variables
ToolName='UploadToPostgres'
ToolVersion='0.5.20210108.1'
ConnectionInfoPath="Connectinfo.json"
LogFileName=ToolName+'.log'
DataFilePath="data.csv"

log.basicConfig(filename=LogFileName,filemode='w',format='%(asctime)s,%(levelname)s,%(message)s', level=log.INFO)
log.info(ToolName+","+ToolVersion)
utc_dt = datetime.datetime.now(datetime.timezone.utc) # UTC time
start=utc_dt.astimezone() # local time
log.debug('Started on '+str(start))
#Read Csv file
if os.path.exists(DataFilePath)==False:
    log.error(DataFilePath+" was not found")
    exit(1)
data=pd.read_csv(DataFilePath)
log.info("read "+str(data.shape[0])+" rows,"+str(data.shape[1])+" columns from "+DataFilePath)

#Read connection Info
if os.path.exists(ConnectionInfoPath)==False:
    log.error(ConnectionInfoPath+" was not found")
    exit(1)
with open(ConnectionInfoPath) as f:
    ConInfo=json.load(f)
log.info("Connection Info-Host:"+ConInfo['host']+",Port:"+str(ConInfo['port'])+",DB:"+ConInfo['database'])

try:
    ConnectionString='postgresql+psycopg2://'+ConInfo['user']+':'+ConInfo['password']+'@'+ConInfo['host']+':'+str(ConInfo['port'])+'/'+ConInfo['database']
    engine = create_engine(ConnectionString)

    data.head(0).to_sql(ConInfo['table'], engine, if_exists='replace',index=False) #truncates the table
    conn = engine.raw_connection()
    cur = conn.cursor()
    output = io.StringIO()
    data.to_csv(output, sep='\t', header=False, index=False)
    output.seek(0)
    contents = output.getvalue()
    cur.copy_from(output, ConInfo['table'], null="") # null values become ''
    conn.commit()   
    conn.close()
    log.info("Data written to database")
    print('Done')
except psycopg2.OperationalError as oe:
    log.error(oe.args[0])
    print("Operational Error occured, check log")
except psycopg2.Error as e:
    log.error(e.args[0])
    print("Error occured, check log")