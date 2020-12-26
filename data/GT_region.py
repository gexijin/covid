# This program downloads Google Trends data via API
# xijin Ge  11/20/2020
# https://www.npmjs.com/package/google-trends-api
#https://pypi.org/project/pytrends/

# Read CSV file
import pandas as pd  
                 
from pytrends.request import TrendReq

import time

#define object
pytrend = TrendReq()

kw = ['covid symptoms', 'covid testing', 'loss of smell', 'loss of taste',  'covid', "iphone"]


kw_list = ['covid symptoms']
pytrend = TrendReq()
pytrend.build_payload(kw_list, cat=0, timeframe= 'now 7-d', geo="US", gprop='')
trend = pytrend.interest_by_region(resolution='REGION', inc_low_vol=True, inc_geo_code=True)
trend.to_csv(r'US_states_trend_geo1.csv')
time.sleep(75)

kw_list = ['covid testing']
pytrend = TrendReq()
pytrend.build_payload(kw_list, cat=0, timeframe= 'now 7-d', geo="US", gprop='')
trend = pytrend.interest_by_region(resolution='REGION', inc_low_vol=True, inc_geo_code=True)
trend.to_csv(r'US_states_trend_geo2.csv')
time.sleep(75)

kw_list = ['loss of smell']
pytrend = TrendReq()
pytrend.build_payload(kw_list, cat=0, timeframe= 'now 7-d', geo="US", gprop='')
trend = pytrend.interest_by_region(resolution='REGION', inc_low_vol=True, inc_geo_code=True)
trend.to_csv(r'US_states_trend_geo3.csv')
time.sleep(75)


kw_list = ['loss of taste']
pytrend = TrendReq()
pytrend.build_payload(kw_list, cat=0, timeframe= 'now 7-d', geo="US", gprop='')
trend = pytrend.interest_by_region(resolution='REGION', inc_low_vol=True, inc_geo_code=True)
trend.to_csv(r'US_states_trend_geo4.csv')
time.sleep(75)


kw_list = ['covid']
pytrend = TrendReq()
pytrend.build_payload(kw_list, cat=0, timeframe= 'now 7-d', geo="US", gprop='')
trend = pytrend.interest_by_region(resolution='REGION', inc_low_vol=True, inc_geo_code=True)
trend.to_csv(r'US_states_trend_geo5.csv')
time.sleep(75)

kw_list = ['iphone']
pytrend = TrendReq()
pytrend.build_payload(kw_list, cat=0, timeframe= 'now 7-d', geo="US", gprop='')
trend = pytrend.interest_by_region(resolution='REGION', inc_low_vol=True, inc_geo_code=True)
trend.to_csv(r'US_states_trend_geo6.csv')

