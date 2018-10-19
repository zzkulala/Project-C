library("csv")
library('dplyr')
library('readxl')
library('aTSA')
library('outliers')
library('quantmod')
library('ggplot2')
library("outliers")

#1
library('dplyr')
library('readxl')
library('aTSA')
library('outliers')
library('quantmod')
library('ggplot2')

#Q1
Q1=read.csv('/Users/zihaoshao/Documents/MIT/Fall/Fin Data Science/Project C/data/bolong_nb.csv')


#Q1=read_excel('/Users/zihaoshao/Documents/MIT/Fall/Fin Data Science/Project C/data/Query1.xlsx', header=TRUE)
#Q1_beta=read_excel('/Users/zihaoshao/Documents/MIT/Fall/Fin Data Science/Project C/data/Q1_beta.xlsx', header=TRUE)

#index_SPX=which(Q1$dbo_bench_ticker=='SPX')
#Q1_a=Q1[index_SPX,]  

#Q1_a$beta=Q1_beta$value
#colnames(Q1_a)=c('d','bench_ticker','equity_ticker','bench_return','equity_return','beta')

Q1$Date = as.Date(Q1$Date,format="%m/%d/%y")

#data integrity check
sum(is.na(Q1$Date))
sum(is.na(Q1$SPX))
sum(is.na(Q1$SPTR))
sum(is.na(Q1$VIX))
sum(is.na(Q1$GS))
sum(is.na(Q1$Beta))

#a
Q1_a = ggplot(Q1, aes(x = Date, y = Beta)) + geom_line()
Q1_a

#b
Q1_b = ggplot(Q1, aes(x = SPTR, y = GS)) + geom_point()
Q1_b = Q1_b + labs(x = "S&P 500 total return index",y='Stock return', title='Scatter plot of GS return vs. SPTR')

#Add regression line
Q1_b + geom_smooth(method=lm)

#Linear Regression
ols_b = lm(Q1$GS ~ Q1$SPTR)
summary(ols_b)

#c
Q1_c = ggplot(Q1, aes(x = VIX, y = GS)) + geom_point()
Q1_c = Q1_c + labs(x = "VIX",y='Stock return', title='Scatter plot of GS return vs. VIX')

#Add regression line
Q1_c + geom_smooth(method=lm)

#Linear Regression
ols_c = lm(Q1$GS ~ Q1$VIX)
summary(ols_c)


#5

#import data
data_comp <- read.csv('/Users/zihaoshao/Documents/MIT/Fall/Fin Data Science/Project C/Q5 Comp Stocks.csv', header=TRUE)
data_DJIA <- read.csv('/Users/zihaoshao/Documents/MIT/Fall/Fin Data Science/Project C/Q5 DJIA.csv', header=TRUE)

#check data integrity (# days)
data_integrity = group_by(data_comp, ticker)
summarise_integrity = summarise(data_integrity,
                                count=n()
                                )
all(summarise_integrity['count'] == 505)
#only has GM data for 356 days

#transfer to dataframe and data column to 'date' format in R
data_comp = as.data.frame(data_comp)
data_comp$d = as.Date(data_comp$d, format="%m/%d/%y")
data_comp = data_comp[order(data_comp$d),]

#screen out the true component of DJIA on a daily basis
DJIA_component = which((data_comp$d < 2018-02-18 & !(data_comp$ticker %in% c('BAC','CVX','KFT','CSCO','TRV'))) | 
                      (data_comp$d > 2018-02-18 & data_comp$d < 2018-09-22 & !(data_comp$ticker %in% c('MO','HON','KFT','CSCO','TRV'))) |
                      (data_comp$d > 2018-09-22 & data_comp$d < 2019-06-08 & !(data_comp$ticker %in% c('MO','HON','AIG','CSCO','TRV'))) |
                      (data_comp$d > 2019-06-08 & !(data_comp$ticker %in% c('MO','HON','AIG','C','GM')))
)

#apply 'DJIA_component' rows to data_comp
data_comp = data_comp[DJIA_component,]

#generate statistics summary for daily DJIA component
grouped_DJIA <- group_by(data_comp,d)
statistics <- summarise(grouped_DJIA,
                        count = n(),
                        price_sum = sum(price)
) 
statistics = as.data.frame(statistics)

#check if # stocks in DJIA is 30 every day
all(statistics$count==30)

#assign weights to each individual stocks
data_comp$weight = NA

for (i in 1:nrow(data_comp)) {
  for (j in 1:30) {
    data_comp$weight[i] = data_comp$price[i] / statistics$price_sum[ceiling(i/30)]
  }
}

# (a) calculate portfolio variance (sigma_{p}^{2})
DJIA_1m = data_DJIA[data_DJIA$nm_numtype == 'vol_021',]
DJIA_3m = data_DJIA[data_DJIA$nm_numtype == 'vol_063',]

DJIA_1m$DJIA_variance = DJIA_1m$value^2
DJIA_3m$DJIA_variance = DJIA_3m$value^2

data_DJIA_1m = as.data.frame(DJIA_1m)%>%
  select('d','DJIA_variance')
data_DJIA_3m = as.data.frame(DJIA_3m)%>%
  select('d','DJIA_variance')

#prepare grouped_by data for results
#one month window
grouped_1m <- group_by(data_comp,d)
statistics_1m <- summarise(grouped_1m,
                           count = n(),
                           sigma_zero = sum(vol_021^2 * weight^2)
) 
statistics_1m = data.frame(statistics_1m)

#three month window
grouped_3m <- group_by(data_comp,d)
statistics_3m <- summarise(grouped_3m,
                           count = n(),
                           sigma_zero = sum(vol_063^2 * weight^2)
) 
statistics_3m = as.data.frame(statistics_3m)

#assign value of \sigma_{0}^{2}
data_DJIA_1m$sigma_zero = statistics_1m$sigma_zero
data_DJIA_3m$sigma_zero = statistics_3m$sigma_zero

#calculate sigma_{1}^{2}
#suppose (deno=sigma(1)^2 - sigma(0)^2))
data_DJIA_1m$deno = 0
data_DJIA_3m$deno = 0

for (i in (30*(0:504)+1)) {    #starting point of each day
  for (j in (i:(i+28))) {        #starting point of calculation of each day
    for (k in (j:(i+29))) {        #end point of calculation of each day
      data_DJIA_1m$deno[ceiling(i/30)] = data_DJIA_1m$deno[ceiling(i/30)] +
        2 * data_comp$vol_021[j] * data_comp$vol_021[k] * data_comp$weight[j] * data_comp$weight[k]
    }
  }
}

for (i in (30*(0:504)+1)) {    #starting point of each day
  for (j in (i:(i+28))) {        #starting point of calculation of each day
    for (k in (j:(i+29))) {        #end point of calculation of each day
      data_DJIA_3m$deno[ceiling(i/30)] = data_DJIA_3m$deno[ceiling(i/30)] +
        2 * data_comp$vol_063[j] * data_comp$vol_063[k] * data_comp$weight[j] * data_comp$weight[k]
    }
  }
}

data_DJIA_1m$sigma_one = data_DJIA_1m$sigma_zero + data_DJIA_1m$deno
data_DJIA_3m$sigma_one = data_DJIA_3m$sigma_zero + data_DJIA_3m$deno

#calculate average correaltion
data_DJIA_1m$corr = (data_DJIA_1m$DJIA_variance - data_DJIA_1m$sigma_zero) / data_DJIA_1m$deno 
data_DJIA_3m$corr = (data_DJIA_3m$DJIA_variance - data_DJIA_3m$sigma_zero) / data_DJIA_3m$deno

#plot the result
data_DJIA_1m$d = as.Date(data_DJIA_1m$d,format="%m/%d/%y")
ggplot(data_DJIA_1m,aes(d,group=1)) +
  geom_line(aes(y=corr, colour = 'Average (Implied) Correlation')) +
  geom_line(aes(y=DJIA_variance, colour = 'Index Realized Variance')) +
  geom_line(aes(y=sigma_zero, colour = 'Index Variance with Correlation=0')) +
  geom_line(aes(y=sigma_one, colour = 'Index Variance with Correlation=1')) +
  labs(x="Date",y="value",title = 'Average Correlation (1-Month Window)') +
  scale_colour_brewer(palette ="Pastel1",direction = -1)

data_DJIA_3m$d = as.Date(data_DJIA_3m$d,format="%m/%d/%y")
ggplot(data_DJIA_3m,aes(d,group=1)) +
  geom_line(aes(y=corr, colour = 'Average (Implied) Correlation')) +
  geom_line(aes(y=DJIA_variance, colour = 'Index Realized variance')) +
  geom_line(aes(y=sigma_zero, colour = 'Index Variance with Correlation=0')) +
  geom_line(aes(y=sigma_one, colour = 'Index Variance with Correlation=1')) +
  labs(x="Date",y="value",title = 'Average Correlation (3-Month Window)') +
  scale_colour_brewer(palette ="Pastel2",direction = -1)

#locate peak correlation and time it occurs
peak_date_1m=data_DJIA_1m[data_DJIA_1m$corr == max(data_DJIA_1m$corr),1]
peak_date_1m
data_DJIA_1m[data_DJIA_1m$d == peak_date_1m, 6]

peak_date_3m=data_DJIA_3m[data_DJIA_3m$corr == max(data_DJIA_3m$corr),1]
peak_date_3m
data_DJIA_3m[data_DJIA_3m$d == peak_date_3m, 6]
