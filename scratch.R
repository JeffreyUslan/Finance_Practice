
library(ggplot2)
library(Quandl)
Quandl.api_key("YBwvjvgzsT2HBC3beQVv")
library(tidyr)
library(dplyr)
library(gridExtra)
library(forecast)

today=as.Date(Sys.Date())
last_year=as.character(today-(365*10))
today=as.character(today)
#End of the Day US Stock Prices
fin_data=tbl_df(Quandl("EOD/APU", start_date=last_year, end_date=today))
fin_data=fin_data %>% arrange(Date)


fin_data$dummy=1:nrow(fin_data)
lm_fit=lm(data=fin_data,Open~dummy)
fin_data$lm_pred=predict(lm_fit)

fin_data$diff=c(0,diff(fin_data$Open))


#Make a some simple plots

Price_plot=ggplot(fin_data)+theme_bw()+
  geom_line(aes(x=Date,y=Open),color="red")+
  geom_smooth(aes(x=Date,y=Open),color="blue")+
  geom_line(aes(x=Date,y=lm_pred),color="black")
if (any(names(fin_data)=="Dividend")) {
  Dividend_plot= ggplot(fin_data)+theme_bw()+
    geom_line(aes(x=Date,y=Dividend),color="green")
  
  grid.arrange(Price_plot, Dividend_plot, ncol = 1)
} else {
  Price_plot
}

Diff_plot=ggplot(fin_data)+theme_bw()+
  geom_line(aes(x=Date,y=diff))+
  ylab("First Difference")
Diff_plot


#Fourier Exploration

plot.df=fourier_smoother(lm_fit$residuals,Threshold=1,heur="Amplitude")
plot.df=tbl_df(plot.df)
plot.df$X.hold[(round(nrow(plot.df)/2)):nrow(plot.df)]=0
plot.df$X.hold=Mod(plot.df$X.hold)

Frequency_plot=ggplot(plot.df[0:(nrow(plot.df)/2),])+theme_bw()+
  geom_line(aes(x=x,y=X.hold))+
  ylab("Amplitude")+xlab("Frequency")
Frequency_plot

plot.df=plot.df %>% arrange(desc(X.hold))
plot.df$dummy=1:nrow(plot.df)
plot.df$Amp_PCT=round(cumsum(plot.df$X.hold)/sum(plot.df$X.hold),3)
Cum_PCT_plot=ggplot(plot.df[0:(nrow(plot.df)/2),])+theme_bw()+geom_line(aes(x=dummy,y=Amp_PCT))+
  xlab("Highest Amplitudes")+ylab("Cumulative Amplitude Percent")
Cum_PCT_plot

# thresh=100*(1-max(which(plot.df$Amp_PCT<.5))/(nrow(plot.df)/2))

plot.df2=fourier_smoother(lm_fit$residuals,Threshold=95,heur="Amplitude")
plot.df2$sim_data=plot.df2$sim_data+fin_data$lm_pred
plot.df2$clean=plot.df2$clean+fin_data$lm_pred
Smoothed_plot=ggplot(plot.df2)+theme_bw()+
  geom_line(aes(x=x,y=sim_data))+
  geom_line(aes(x=x,y=clean),color="blue",size=1.5)+
  ylab("Simulated Data")+xlab("Time")
Smoothed_plot

#Autocorrelation
acf(fin_data$Open)
pacf(fin_data$Open)
acf(lm_fit$residuals)
acf(diff(fin_data$Open))



#Seasonal Decomposition
stl_obj=stl(ts(fin_data$Open, frequency = 80),t.window=15,s.window="periodic",robust=TRUE)
plot(stl_obj)

#Arima and forecasting
arima_model=auto.arima(fin_data$Open)
summary(arima_model)
fcast=forecast(arima_model)
plot(fcast)
