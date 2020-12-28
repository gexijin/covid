# This program downloads Google Trends data via API
# xijin Ge  11/20/2020
# https://www.npmjs.com/package/google-trends-api
#https://pypi.org/project/pytrends/

from datetime import date

today = date.today()
# Formate dd/mm/YY
today = today.strftime("%Y-%m-%d")
timeframe='2020-04-15 ' + today 
print(timeframe)


# Read CSV file
import pandas as pd  
states = pd.read_csv("../us/US_states.csv")
stateList = states["Region"].tolist()


import pandas as pd  
                    
from pytrends.request import TrendReq


#define object
pytrend = TrendReq()

kw_list = ['covid symptoms', 'covid testing']

import time
trend2 = pd.DataFrame()
for Region in stateList:
  print(Region)
  #prepare payload
  pytrend.build_payload(kw_list, 
                         cat=0, 
                         timeframe='2020-04-15 ' + today , 
                         geo=Region, 
                         gprop='')
  #retrieve data
  trend = pytrend.interest_over_time()

  trend = pd.DataFrame(trend)
  trend["Region"] = Region
  # concatenate 
  trend2 = pd.concat([trend,trend2])
  time.sleep(75)

trend2.to_csv(r'GT_data_time1.csv')


pytrend = TrendReq()
kw_list = ['loss of smell', 'loss of taste']
import time
trend3 = pd.DataFrame()
for Region in stateList:
  print(Region)
  #prepare payload
  pytrend.build_payload(kw_list, 
                         cat=0, 
                         timeframe='2020-04-15 ' + today , 
                         geo=Region, 
                         gprop='')
  #retrieve data
  trend = pytrend.interest_over_time()

  trend = pd.DataFrame(trend)
  trend["Region"] = Region
  # concatenate 
  trend3 = pd.concat([trend,trend3])
  time.sleep(75)

trend3.to_csv(r'GT_data_time2.csv')



pytrend = TrendReq()
kw_list = ['covid', 'iphone']
import time
trend3 = pd.DataFrame()
for Region in stateList:
  print(Region)
  #prepare payload
  pytrend.build_payload(kw_list, 
                         cat=0, 
                         timeframe='2020-04-15 ' + today , 
                         geo=Region, 
                         gprop='')
  #retrieve data
  trend = pytrend.interest_over_time()

  trend = pd.DataFrame(trend)
  trend["Region"] = Region
  # concatenate 
  trend3 = pd.concat([trend,trend3])
  time.sleep(75)

trend3.to_csv(r'GT_data_time3.csv')




