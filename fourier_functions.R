fourier_smoother=function(sim_data,Threshold,heur) {
  # Threshold=0.5
  # print(Threshold)
  Threshold=Threshold/100
  X.hold = fft(sim_data)
  # plot(sim_data)
  # plot(Mod(X.hold[0:((length(X.hold)-1)/2)]))
  
  if (heur=="Frequency"){
    n=length(X.hold)/2
    thresh_ind_start=round(n*(1-Threshold))
    thresh_ind_end= 2*n-thresh_ind_start
    ind_len=thresh_ind_end-thresh_ind_start
    # print(ind_len)  
    if (ind_len>1) X.hold[thresh_ind_start:thresh_ind_end]=0
  } else {
    cutoff=quantile(Mod(X.hold),Threshold)
    cutoff_ind=which(Mod(X.hold)<cutoff)
    
    if (length(cutoff_ind)>1) X.hold[cutoff_ind]=0
  }
  # plot(Mod(X.hold[0:((length(X.hold)-1)/2)]))
  X.hold.plottable=Mod(X.hold[0:((length(X.hold)-1)/2)])
  X.unf=fft(X.hold,inverse=TRUE)/length(sim_data)
  X.unf=as.double(X.unf)
  # plot(X.unf)
  plot.df=data.frame(x=1:length(sim_data),sim_data=sim_data,clean=X.unf,X.hold)
  return(plot.df)
}

#Function for smoother by high frequency on down
fourier_smoother_high_freq_plot = function(Threshold) {
  plot.df <-fourier_smoother(Threshold,heur="Frequency")
  
  g=ggplot()+theme_bw()+
    geom_line(data=plot.df,aes(x=x,y=sim_data))+
    geom_line(data=plot.df,aes(x=x,y=clean),color="blue",size=1.5)+
    ylab("Simulated Data")+xlab("Time")
  g
} #end fourier_smoother_high_freq_plot

fourier_smoother_high_freq_domain_plot = function(Threshold) {
  if (!exists("plot.df"))  plot.dffourier_smoother(Threshold,heur="Frequency")
  
  plot.df$X.hold=Mod(plot.df$X.hold)
  plot.df=plot.df[0:(dim(plot.df)[1]/2),]
  g=ggplot()+theme_bw()+
    geom_line(data=plot.df,aes(x=x,y=X.hold))+
    ylab("Amplitude")+xlab("Frequency")
  g
} # end of fourier_smoother_high_freq_domain_plot


#Function for smoother by low amplitude on up

fourier_smoother_low_amp_plot = function(Threshold){
  
  plot.df <<- fourier_smoother(Threshold,heur="Amplitude")
  g=ggplot()+theme_bw()+
    geom_line(data=plot.df,aes(x=x,y=sim_data))+
    geom_line(data=plot.df,aes(x=x,y=clean),color="blue",size=1)+
    ylab("Simulated Data")+xlab("Time")
  g
  
  
  
} #end fourier_smoother_low_amp_plot


fourier_smoother_low_amp_domain_plot = function(Threshold) {
  if (!exists("plot.df"))  plot.df <<- fourier_smoother(Threshold,heur="Amplitude")
  
  plot.df$X.hold=Mod(plot.df$X.hold)
  plot.df=plot.df[0:(dim(plot.df)[1]/2),]
  g=ggplot()+theme_bw()+
    geom_line(data=plot.df,aes(x=x,y=X.hold))+
    ylab("Amplitude")+xlab("Frequency")
  g
} # end of fourier_smoother_low_amp_domain_plot