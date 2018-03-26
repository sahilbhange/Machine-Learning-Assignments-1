install.packages('depmixS4')
install.packages('quantmod')
library('depmixS4')
library('quantmod')
set.seed(1)

# Create the parameters for the bull and
# bear market returns distributions
Nk_lower <- 50
Nk_upper <- 150
bull_mean <- 0.1
bull_var <- 0.1
bear_mean <- -0.05
bear_var <- 0.2

# Create the list of durations (in days) for each regime
days <- replicate(5, sample(Nk_lower:Nk_upper, 1))

# Create the various bull and bear markets returns
market_bull_1 <- rnorm(days[1], bull_mean, bull_var) 
market_bear_2 <- rnorm(days[2], bear_mean, bear_var) 
market_bull_3 <- rnorm(days[3], bull_mean, bull_var) 
market_bear_4 <- rnorm(days[4], bear_mean, bear_var) 
market_bull_5 <- rnorm(days[5], bull_mean, bull_var) 

# Create the list of true regime states and full returns list
true_regimes <- c(rep(1,days[1]), rep(2,days[2]), rep(1,days[3]), rep(2,days[4]), rep(1,days[5]))
returns <- c(market_bull_1, market_bear_2, market_bull_3, market_bear_4, market_bull_5)

plot(returns, type="l", xlab='Days', ylab="Returns") 

# Create and fit the Hidden Markov Model
# The first argument is 'response' and it runs over states (index 1)
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)

# Output both the true regimes and the 
# posterior probabilities of the regimes
post_probs <- posterior(hmmfit)
layout(1:2)
graphics.off()

plot(post_probs$state, type='s', main='True Regimes', xlab='Days', ylab='Regime')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', xlab='Days', ylab='Probability')
legend(x='topright', c('Bull','Bear'), fill=1:2, bty='n')

# Real example

as.POSIXct(strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))

# Obtain S&P500 data from 2004 onwards and
# create the returns stream from this
getSymbols("^GSPC", from="2004-01-01" )

head(GSPC)
head(TSairqual)


?diff
?log
?Cl


gspcRets = diff(log(Cl(GSPC)))
head(gspcRets)

returns = as.numeric(gspcRets)

head(returns)
head(gspcRets)

plot(gspcRets)

# Fit a Hidden Markov Model with two states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='Days', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', xlab='Days', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2'), fill=1:2, bty='n')

# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')









head(gspcRets)

head(GSPC)

diff(log(Cl(GSPC)))

diff(log(Cl(head(GSPC[,c(4)]))))

head(GSPC[,c(4)])




attach(mtcars)
View(mtcars)

head(mtcars)

names(ImputedAirqual)

length(unique(airqual$Date))

aggdata_min <-aggregate(ImputedAirqual$CO.GT., by=list(ImputedAirqual$Date), 
                    FUN=min, na.rm=TRUE)
aggdata_max <-aggregate(ImputedAirqual$CO.GT., by=list(ImputedAirqual$Date), 
                         FUN=max, na.rm=TRUE)
airqual_open <- ImputedAirqual[ImputedAirqual$Time=='0:00:00',]
airqual_close <- ImputedAirqual[ImputedAirqual$Time=='23:00:00',]

dim(airqual_open)
dim(airqual_close)
dim(aggdata_max)
dim(aggdata_min)

names(airqual_open)
names(airqual_close)
names(aggdata_max)
names(aggdata_min)

colnames(aggdata_max)[which(colnames(aggdata_max) == 'Group.1')] <- 'Date'
colnames(aggdata_min)[which(colnames(aggdata_min) == 'Group.1')] <- 'Date'
colnames(aggdata_max)[which(colnames(aggdata_max) == 'x')] <- 'CO.GT.Max'
colnames(aggdata_min)[which(colnames(aggdata_min) == 'x')] <- 'CO.GT.Min'

colnames(airqual_close)[which(colnames(airqual_close) == 'CO.GT.')] <- 'CO.GT.Close'

colnames(airqual_open)[which(colnames(airqual_open) == 'CO.GT.')] <- 'CO.GT.Open'


Merged_data <- merge(aggdata_max, aggdata_min, by="Date")
Merged_data <- merge(Merged_data, airqual_close, by="Date")
Merged_data <- merge(Merged_data, airqual_open, by="Date")

names(Merged_data)

names(aggdata_max)

names(AirQualTMS[,2:5])
names(EURUSD1d[,2:5])

AirQualTMS = Merged_data[,c(1,19,2,3,5)]

View(AirQualTMS)
names(AirQualTMS)

AirQualTMS_cleans <- (AirQualTMS[complete.cases(AirQualTMS), ])

Date<-as.character(AirQualTMS_cleans[,1])
DateTS<- as.POSIXlt(Date, format = "%m/%d/%Y") #create date and time objects
TSData<-data.frame(AirQualTMS_cleans[,2:5],row.names=DateTS)
TSData<-as.xts(TSData) #build our time series data set


ATRindicator<-ATR(TSData[,2:4],n=14) #calculate the indicator
ATR<-ATRindicator[,2] #grab just the ATR

LogChange <- log(AirQualTMS_cleans$CO.GT.Close) - log(AirQualTMS_cleans$CO.GT.Open)

ModelData<-data.frame(LogChange,ATR) #create the data frame for our HMM model


colnames(ModelData)<-c("LogChange","ATR") #name our columns


set.seed(1)
HMM<-depmix(list(LogChange~1,ATR~1),data=ModelData,nstates=3,family=list(gaussian(),gaussian())) 
HMMfit<-fit(HMM, verbose = FALSE)

print(HMMfit) 
summary(HMMfit)

HMMpost<-posterior(HMMfit)

head(HMMpost) 

plot(LogChange)

plot(ATR)

# Fit a Hidden Markov Model with two states 
# to the S&P500 returns stream
hmm <- depmix(ATR~1, family = gaussian(), nstates = 3, data=ModelData)
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(ModelData$ATR, type='l', main='Change Detection', xlab='Days', ylab='Change')
matplot(post_probs[,-1], type='l', main='Change Posterior Probabilities', xlab='Days', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2'), fill=1:2, bty='n')

# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
hmm <- depmix(ATR ~ 1, family = gaussian(), nstates = 3, data=ModelData)
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes

layout(1:2)
plot(ModelData$ATR, type='l', main='Change Detection', xlab='', ylab='Change')
matplot(post_probs[,-1], type='l', main='Change Posterior Probabilities', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')


