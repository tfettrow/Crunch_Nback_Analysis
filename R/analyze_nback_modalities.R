analyze_nback_modalities  <- function(subject_id)
{
  fNRIS<-read.csv(file.choose(), header=T)
  EEG<-read.csv(file.choose(), header=T)
  
  xdata=rbind(fNRIS$nback_level,EEG$nback_level)
  
  ydata=rbind(fNRIS$percent_correct,EEG$percent_correct)
  
  
  
  plot(xdata,ydata,col=c("red", "blue"),xlab = "Nback Level",ylab = "Percent Correct", main = "Nback Level vs Percent Correct")
}