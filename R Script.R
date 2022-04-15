### "Predicting the price of Bitcoin with an ARIMA model and exploring other potentially influencing factors by vector autoregressions (VAR)"

### Downloading Data via API
# Downloading levels of S&P500, NVIDIA, and Gold via Yahoo Finance API
library(quantmod)
tickers_index <- c("^GSPC", "NVDA", "GC=F")
stocks <- NULL
btc <- NULL
for (Ticker in tickers_index){
  stocks <- cbind(stocks,
              getSymbols.yahoo(Ticker,
                               from = "2013-12-28",
                               periodicity = "daily",
                               auto.assign=FALSE)[,6])}

colnames(stocks) <- c("S&P500", "NVIDIA", "Gold")

# Downloading levels of Bitcoin via Yahoo Finance API

btc <- cbind(btc, getSymbols.yahoo("BTC-USD",
                                   from = "2013-12-28",
                                   periodicity = "daily",
                                   auto.assign=FALSE)[,6])


colnames(btc) <- "Bitcoin"


### Finding and replacing NA/s
which(is.na(btc$Bitcoin)) # Output: 2040 2215 2218 2219

## Replace NA/s with values from currencies.zone
btc$Bitcoin[2040,]
# 2020-04-17      NA
# http://currencies.zone/historic/bitcoin/us-dollar/april-2020
# 7092 USD
btc$Bitcoin[2040] <- 7092

btc$Bitcoin[2215,]
# 2020-10-09      NA
# http://currencies.zone/historic/bitcoin/us-dollar/october-2020
# 11111 USD
btc$Bitcoin[2215] <- 11111

btc$Bitcoin[2218,]
# 2020-10-12      NA
# http://currencies.zone/historic/bitcoin/us-dollar/october-2020
# 11494
btc$Bitcoin[2218] <- 11494

btc$Bitcoin[2219,]
# 2020-10-13      NA
# http://currencies.zone/historic/bitcoin/us-dollar/october-2020
# 11363.64
btc$Bitcoin[2219] <- 11363

which(is.na(btc$Bitcoin)) # Output: integer(0) -> No N/As left in BTC price


### Plotting BTC, testing for stationarity and differencing
library(tseries)
plot(btc$Bitcoin, main="Bitcoin Price in USD", xlab="Time", ylab="Price in USD")
# A clear upward trend is visible in the time series

# Plot ACF and PAXF
acf(btc$Bitcoin, main="Bitcoin/USD")
# The ACF decays gradually, which tells us that we should difference the data, which would also address the issue of the upward trend.


pacf(btc$Bitcoin, main="Bitcoin/USD")
# The PACF cuts off abruptly


adf.test(btc$Bitcoin) # Result: p-value = 0.7683

# The null hypothesis is not rejected as the p-value is greater than 0.05
## Interpretation: The ADF-test confirms that the level data of Bitcoin is not
## stationary and can thus not yet be modeled using an ARIMA-model.

## First differences of the log transformation of the BTC price to make the time series stationary
bitcoin_returns <- na.omit(diff(log(btc$Bitcoin)))*100

# Plotting Bitcoin returns and testing for stationarity
plot(bitcoin_returns, main="Bitcoin returns", ylab="Return in %")
adf.test(bitcoin_returns) # Result: p-value = 0.01
# The null hypothesis is rejected as the p-value is smaller than 0.05
## Interpretation: according to the ADF-test, first differences of the log
## returns of Bitcoin are stationary and can thus now be modelled using an ARIMA model.

### ACF and PACF of Bitcoin returns
acf(bitcoin_returns, main="Bitcoin returns")
pacf(bitcoin_returns, main="Bitcoin returns")
## Interpretation: Both ACF and PACF are significant at lag 6, which suggests p = 6

# Identifying the orders p and q of the ARIMA(p,1,q)-model by testing different
# model specifications
max.order <- 6 # Allow a maximum of six AR- and/or MA-terms
d <- 1 # Since BTC/USD is non-stationary in levels but stationary in first differences, the order of integration d is set to 1

# Creating the matrix in which values of AICs for different models are stored
arima_aic <- matrix(NA, ncol=max.order+1, nrow=max.order+1)
row.names(arima_aic) <- c(0:max.order) # Order of AR(p) in rows
colnames(arima_aic) <- c(0:max.order) # Order of MA(q) in columns


# Calculating and storing the AICs for different models
library(forecast)
for(i in 0:max.order){
  for(j in 0:max.order){
    arima_aic[i+1,j+1]<-Arima(btc$Bitcoin, order=c(i,d,j), include.constant = TRUE)$aic
  }
}

# Finding the model with the lowest AIC
index <- which(arima_aic == min(arima_aic), arr.ind = TRUE)
ar <- as.numeric(rownames(arima_aic)[index[1]])
ma <- as.numeric(colnames(arima_aic)[index[2]])

## Interpretation: The Akaike information criterion (AIC) is minimized for the 
## ARIMA(4,1,6) including a drift term, hence for an ARIMA including one
## autoregressive and one moving average term.

# Estimating the optimal ARIMA-model and testing for significance of the coefficients
arima <- Arima(btc$Bitcoin, order=c(ar,1,ma), include.constant = TRUE)

library(lmtest)
coeftest(arima)
## Interpretation: All coefficients are significant at the 90% confidence interval
## and all but MA5 at the 95% confidence interval as well


# Forecasting
pred <- forecast(arima, level=0.95, h=14)
pred
plot(pred, ylab="Bitcoin Price in USD")
## Interpretation: The blue line is the Bitcoin forecast for the next 14 days
# The light grey area represents the 95% prediction interval.
# Thus, with a 95% probability, Bitcoin will be in this range at a given time.



###############################################################################
### Vector Auto Regressions
## Get Google Trends for Bitcoin
library(gtrendsR)
library(tidyverse)
library(lubridate)

get_daily_gtrend <- function(keyword = 'Bitcoin', geo = '', from = '2018-12-28', to = '2021-04-24') {
  if (ymd(to) >= floor_date(Sys.Date(), 'month')) {
    to <- floor_date(ymd(to), 'month') - days(1)
    
    if (to < from) {
      stop("Specifying \'to\' date in the current month is not allowed")
    }
  }
  
  mult_m <- gtrends(keyword = keyword, geo = geo, time = paste(from, to))$interest_over_time %>%
    group_by(month = floor_date(date, 'month')) %>%
    summarise(hits = sum(hits)) %>%
    mutate(ym = format(month, '%Y-%m'),
           mult = hits / max(hits)) %>%
    select(month, ym, mult) %>%
    as_tibble()
  
  pm <- tibble(s = seq(ymd(from), ymd(to), by = 'month'), 
               e = seq(ymd(from), ymd(to), by = 'month') + months(1) - days(1))
  
  raw_trends_m <- tibble()
  
  for (i in seq(1, nrow(pm), 1)) {
    curr <- gtrends(keyword, geo = geo, time = paste(pm$s[i], pm$e[i]))
    print(paste('for', pm$s[i], pm$e[i], 'retrieved', count(curr$interest_over_time), 'days of data'))
    raw_trends_m<- rbind(raw_trends_m,
                         curr$interest_over_time)
  }
  
  trend_m <- raw_trends_m %>%
    select(date, hits) %>%
    mutate(ym = format(date, '%Y-%m')) %>%
    as_tibble()
  
  trend_res <- trend_m %>%
    left_join(mult_m, by = 'ym') %>%
    mutate(est_hits = hits * mult) %>%
    select(date, est_hits) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  return(trend_res)
}

# Create dataframe with google trend results
google_trends <- get_daily_gtrend(keyword = 'Bitcoin', geo = '', from = "2018-12-28", to = "2021-04-24")
google_trends$date <- as.Date(google_trends$date)
names(google_trends)[names(google_trends) == "est_hits"] <- "Google_Hits"

# Write to CSV file in case of loss of connection
write_csv(google_trends, "GoogleTrends.csv")

# Load CSV file again
google_trends <- read_csv("GoogleTrends.csv")

# Create XTS file
google_trends <- xts(google_trends[,-1], order.by=google_trends$date)
plot(google_trends)

# Test for stationarity
adf.test(google_trends) # p-value = 0.02274

# Plot Google trends
plot(google_trends) # Clear uptrend visible

# First differences of Google Trends
google_trends_returns <- na.omit(diff(google_trends))

# Plot first differences
plot(google_trends_returns, main="Diff of Google Trends") # Looks stationary

# ADF Test to confirm stationarity
adf.test(google_trends_returns) # p-value = 0.01
# The null hypothesis is rejected as the p-value is smaller than 0.05
## Interpretation: according to the ADF-test, first differences of Google Trends
## are stationary.

# Merge Google Trends time series with Bitcoin price for VAR analysis
google_trends_btc <- merge(google_trends_returns, bitcoin_returns, join = "inner")

### Blockchain.com Data
# Import CSV-files obtained from Blockchain.com and merge them 
hash_rate <- read.csv("hash-rate.csv", header = TRUE)
n_transactions <- read.csv("n-transactions.csv", header = TRUE)
addresses <- read.csv("n-unique-addresses.csv", header = TRUE)
trade_volume <- read.csv("trade-volume.csv", header = TRUE)
transaction_fees <- read.csv("transaction-fees-usd.csv", header = TRUE)

# Merge newly imported dataframes
library(plyr)
df <- join_all(list(hash_rate, n_transactions, addresses, trade_volume, transaction_fees), by = "Timestamp")

# Encode columns correctly
df$Timestamp <- as.Date(df$Timestamp)
df$n.transactions <- as.numeric(df$n.transactions)
df$n.unique.addresses <- as.numeric(df$n.unique.addresses)
df$trade.volume <- as.numeric(df$trade.volume)

# Change column name of "Timestamp" to "date"
names(df)[names(df) == "Timestamp"] <- "date"

# Create XTS file from df
df <- xts(df[,-1], order.by=df[,1])

# Check and drop N/As
sum(is.na(df)) # No N/As in the file

# Save time series in a file in case of loss of data
write.zoo(df, "df.csv", quote = FALSE, sep = ",")

# Plot the individual time series to evaluate stationarity
plot(df$hash.rate, main = "BTC Hashrate")
plot(df$n.transactions, main = "Transactions on the BTC Blockchain")
plot(df$n.unique.addresses, main = "Number of Unique BTC Addresses")
plot(df$trade.volume, main = "BTC Trade Volume")
plot(df$transaction.fees.usd, main = "BTC Transaction fees in USD")

# Interpretation: all time series show a trend and are thus likely not stationary

# Check for stationarity with ADF Test as well
apply(df, 2, adf.test)
# Result: Only the p-value of "n.transactions" is smaller than 0.05 and therefore stationary, but visual
# exploration suggests that this time series be transformed as well

# Apply first differences to the time series
df <- na.omit(diff(df))

# Inspect first differences
plot(df$hash.rate, main = "Diff BTC Hashrate")
plot(df$n.transactions, main = "Log Diff Transactions on the BTC Blockchain")
plot(df$n.unique.addresses, main = "Log Diff Number of Unique BTC Addresses")
plot(df$trade.volume, main = "Log Diff BTC Trade Volume")
plot(df$transaction.fees.usd, main = "Log Diff BTC Transaction fees in USD")
# Interpretation: all time series now look stationary

apply(df, 2, adf.test) # Result: all p-values are smaller than 0.05 and therefore all time series stationary

# Merge time series with the bitcoin price for later VARs
df <- merge(df, bitcoin_returns, join = "inner")

### Prepare Stocks for VAR
# Check for and drop N/As
sum(is.na(stocks))
stocks <- na.omit(stocks)

# Plot the time series
plot(stocks$`S&P500`, main = "S&P 500")
plot(stocks$NVIDIA, main = "NVIDIA")
plot(stocks$Gold, main = "Gold")

# Interpretation: clear upward trend visible, so time series are not stationary

# Perform ADF Test of the time series
apply(stocks, 2, adf.test)
# Result: all p-values are larger than 0.05 and therefore confirms that none of the time series are stationary

# Apply first difference of the log of the time series
stocks.returns <- na.omit(diff(log(stocks)))

# Perform ADF Test of transformed time series
apply(stocks.returns, 2, adf.test)
# Result: all p-values are smaller than 0.05 and therefore all time series stationary

# Merge time series with Bitcoin price for VAR analysis
stocks.merged.var <- merge(stocks.returns, bitcoin_returns, join = "inner")

### VAR analysis
library(vars)

# Google Trends
#---------------------
VAR_google_trends <- VAR(cbind(google_trends_btc$Bitcoin, google_trends_btc$Google_Hits), ic="AIC")
coeftest(VAR_google_trends)
causality(VAR_google_trends, cause="Google_Hits")["Granger"] # p-value = 0.4779
causality(VAR_google_trends, cause="Bitcoin")["Granger"] # p-value = 0.5826
plot(irf(VAR_google_trends, impulse="Google_Hits", response="Bitcoin"))

# Hash Rate
#---------------------
VAR_hash_rate <- VAR(cbind(df$Bitcoin, df$hash.rate), ic="AIC")
coeftest(VAR_hash_rate)
causality(VAR_hash_rate, cause="hash.rate")["Granger"] # p-value = 0.04187
causality(VAR_hash_rate, cause="Bitcoin")["Granger"] # p-value = 0.1424
plot(irf(VAR_hash_rate, impulse="hash.rate", response="Bitcoin"))

# Number of Transactions on the Blockchain
#---------------------
VAR_n.transactions <- VAR(cbind(df$Bitcoin, df$n.transactions), ic="AIC")
coeftest(VAR_n.transactions)
causality(VAR_n.transactions, cause="n.transactions")["Granger"] # p-value = 0.2671
causality(VAR_n.transactions, cause="Bitcoin")["Granger"] # p-value = 0.7368
plot(irf(VAR_n.transactions, impulse="n.transactions", response="Bitcoin"))

# Number of unique BTC addresses
#---------------------
VAR_n.unique.addresses <- VAR(cbind(df$Bitcoin, df$n.unique.addresses), ic="AIC")
coeftest(VAR_n.unique.addresses)
causality(VAR_n.unique.addresses, cause="n.unique.addresses")["Granger"] # p-value = 0.1863
causality(VAR_n.unique.addresses, cause="Bitcoin")["Granger"] # p-value = 0.9657
plot(irf(VAR_n.unique.addresses, impulse="n.unique.addresses", response="Bitcoin"))

# Trade Volume
#---------------------
VAR_trade.volume <- VAR(cbind(df$Bitcoin, df$trade.volume), ic="AIC")
coeftest(VAR_trade.volume)
causality(VAR_trade.volume, cause="trade.volume")["Granger"] # p-value = 0.003007
causality(VAR_trade.volume, cause="Bitcoin")["Granger"] # p-value = 0.1087
plot(irf(VAR_trade.volume, impulse="trade.volume", response="Bitcoin"))

# Transaction Fees
#---------------------
VAR_transaction_fees <- VAR(cbind(df$Bitcoin, df$transaction.fees.usd), ic="AIC")
coeftest(VAR_transaction_fees)
causality(VAR_transaction_fees, cause="transaction.fees.usd")["Granger"] # p-value = 0.01084
causality(VAR_transaction_fees, cause="Bitcoin")["Granger"] # p-value = 0.9021
plot(irf(VAR_transaction_fees, impulse="transaction.fees.usd", response="Bitcoin"))

# Gold Price
#---------------------
VAR_gold_price <- VAR(cbind(stocks.merged.var$Bitcoin, stocks.merged.var$Gold), ic="AIC")
coeftest(VAR_gold_price)
causality(VAR_gold_price, cause="Gold")["Granger"] # p-value = 0.3754
causality(VAR_gold_price, cause="Bitcoin")["Granger"] # p-value = 0.2564
plot(irf(VAR_gold_price, impulse="Gold", response="Bitcoin"))

# NVIDIA
#---------------------
VAR_NVDA <- VAR(cbind(stocks.merged.var$Bitcoin, stocks.merged.var$NVIDIA), ic="AIC")
coeftest(VAR_NVDA)
causality(VAR_NVDA, cause="NVIDIA")["Granger"] # p-value = 0.2933
causality(VAR_NVDA, cause="Bitcoin")["Granger"] # p-value = 0.1046
plot(irf(VAR_NVDA, impulse="NVIDIA", response="Bitcoin"))

# S&P 500
#---------------------
VAR_SP <- VAR(cbind(stocks.merged.var$Bitcoin, stocks.merged.var$S.P500), ic="AIC")
coeftest(VAR_SP)
causality(VAR_SP, cause="S.P500")["Granger"] # p-value = 0.13
causality(VAR_SP, cause="Bitcoin")["Granger"] # p-value = 0.3282
plot(irf(VAR_SP, impulse="S.P500", response="Bitcoin"))

### Results: The time series hash rate, trading volume, and transaction fees Granger-cause Bitcoin prices but not vice-versa.