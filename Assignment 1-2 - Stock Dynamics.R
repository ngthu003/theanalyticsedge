# Loading data
IBM = read.csv('IBMStock.csv')
GE = read.csv('GEStock.csv')
ProcterGamble = read.csv('ProcterGambleStock.csv')
CocaCola = read.csv('CocaColaStock.csv')
Boeing = read.csv('BoeingStock.csv')

# 1.1
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#######################################################

# 2.1
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col = 'red')

# 2.2
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'blue')
abline(v=as.Date(c("2000-03-01")), lwd=1)

# 2.3
abline(v=as.Date(c("1983-03-01")), lwd=1)

#######################################################

# 3.1
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="BLUE")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="black")

# 3.2
abline(v=as.Date(c("2000-03-01")), lwd=1)

# 3.3
abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)

#######################################################

# 4.1
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

# 4.2
tapply(GE$StockPrice, months(GE$Date), mean)
mean(GE$StockPrice)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
mean(CocaCola$StockPrice)
