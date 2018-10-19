# -*- coding: utf-8 -*-
"""
Created on Sat Oct 13 13:20:39 2018

@author: ishad
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm

# "df" denotes datetype of "Pandas DataFrame"
# "sr" denotes datetype of "Pandas Series"

print('Reading data file...')
dfRaw0 = pd.read_csv('PortfolioReturnLongShort.txt', delimiter='\t')
dfRaw1 = dfRaw0.loc[:, ['Row Labels', 'Weighted Return Tc', 'Weighted Beta','Long Weighted Return Tc','Short Weighted Return Tc']]  # Remove useless columns
dfRaw2 = pd.read_csv('dbo_famafrench.txt', delimiter='\t')
dfRaw3 = dfRaw2.loc[:, ['d', 'mktrf', 'smb', 'hml', 'rf', 'umd']] 
# De-normalized table
#dfRaw1['Row Labels'].dt.date
dfRaw1['Row Labels']=pd.to_datetime(dfRaw1['Row Labels'])
dfRaw1.set_index('Row Labels',inplace=True)
dfRaw3['d']=dfRaw3['d'].str.split(expand=True)
dfRaw3['d']=pd.to_datetime(dfRaw3['d'])
dfRaw3.set_index('d',inplace=True)

result = dfRaw1.join(dfRaw3, how='inner')

start=pd.datetime(2001,01,01)
end=pd.datetime(2004,12,31)
q2Data=result[(result.index>=start) & (result.index<=end)]

##Q2 (a)
def ComputeAnnualStat(portfolioType,startYear,yearLength):
    annualReturn=[]
    for i in range (yearLength):
        start=pd.datetime(startYear+i,01,01)
        end=pd.datetime(startYear+i,12,31)
        annualData=result[(result.index>=start) & (result.index<=end)]
        annualReturn.append(annualData[portfolioType].sum())
    
    meanAnlReturn=np.average(annualReturn)
    anlVolatility=np.std(annualReturn)
    anlSharpe=meanAnlReturn/anlVolatility
    
    print("{}:\nThe annualized return is:{} \nThe volatility is:{}\nThe Sharpe ratio is:{} \n".format(portfolioType,meanAnlReturn,anlVolatility,anlSharpe))

ComputeAnnualStat('Weighted Return Tc',2001,4)

##Q2 (b)
def Analysis(data):
    X=data['mktrf']
    Y=data['Weighted Return Tc']
    X = sm.add_constant(X)
    # Note the difference in argument order
    model = sm.OLS(Y,X).fit()
    
    # Print out the statistics
    print model.summary()
    print
    
    
    ##Q2 (c)
    X=data[['mktrf','smb','hml','umd']]
    Y=data['Weighted Return Tc']
    X = sm.add_constant(X)
    # Note the difference in argument order
    model = sm.OLS(Y,X).fit()
    
    # Print out the statistics
    print model.summary()
    print
    
    ##Q2 (d)
    sortedQ2Return=data.sort_values(by=['Weighted Return Tc'])
    x=np.linspace(1,sortedQ2Return.shape[0],sortedQ2Return.shape[0])
    plt.figure()
    plt.bar(x,sortedQ2Return['Weighted Return Tc'])
    
    Winner=data[(data['Weighted Return Tc']>0)]
    Loser=data[(data['Weighted Return Tc']<0)]
    
    fracWinner=float(Winner.shape[0])/sortedQ2Return.shape[0]
    fracLoser=float(Loser.shape[0])/sortedQ2Return.shape[0]
    
    medRWinner=np.median(Winner['Weighted Return Tc'])
    medRLoser=np.median(Loser['Weighted Return Tc'])
    
    print("The fraction of Winner is:{} \nThe fraction of Loser is:{}\nThe median return of Winner is:{} \nThe median return of Winner is:{} \n".format(fracWinner,fracLoser,medRWinner,medRLoser))
Analysis(q2Data)
#predictions = model.predict(X) # make the predictions by the model



##Q3 (a)
ComputeAnnualStat('Weighted Return Tc',2005,5)
ComputeAnnualStat('Long Weighted Return Tc',2005,5)
ComputeAnnualStat('Short Weighted Return Tc',2005,5)

##Q3 (b)(c)(d)
start=pd.datetime(2005,01,01)
end=pd.datetime(2009,12,31)
q3Data=result[(result.index>=start) & (result.index<=end)]
Analysis(q3Data)



