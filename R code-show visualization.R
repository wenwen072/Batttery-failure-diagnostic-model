# Visualization
# Plot graph to see the variance captured by the number of pricipal components
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2),
     xlab = 'Number of Principal Components',
     ylab = 'Cummulative of Variances Captured',
     main = 'Cummulative of Data Variances Captured vs Number of Principal Components')

# Plot reconstruaction performace of PCA
ggplot(soc_recon_one_long[ind,])+
  geom_line(aes(x=date,y=SOC,group=imei),linetype=2)+
  geom_line(data=soc_tidy_1[ind,],
            aes(x=as.numeric(date),y=SOC,group=imei),col='red') +
  ggtitle("Reconstruction performance of 1 PCA")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(soc_recon_75_long[ind,])+
  geom_line(aes(x=date,y=SOC,group=imei),linetype=2)+
  geom_line(data=soc_tidy_1[ind,],
            aes(x=as.numeric(date),y=SOC,group=imei),col='red') +
  ggtitle("Reconstruction performance of 75 PCA")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(soc_recon_150_long[ind,])+
  geom_line(aes(x=date,y=SOC,group=imei),linetype=2)+
  geom_line(data=soc_tidy_1[ind,],
            aes(x=as.numeric(date),y=SOC,group=imei),col='red') +
  ggtitle("Reconstruction performance of 150 PCA")+
  theme(plot.title = element_text(hjust = 0.5)) 

#Plot profile for the correct prediction (selected)
ggplot(soc_recon_150_long[ind,])+
  geom_line(aes(x=date,y=SOC,group=imei),linetype=2)+
  geom_line(data=soc_tidy_1[ind,],
            aes(x=as.numeric(date),y=SOC,group=imei),col='red') +
  ggtitle("Reconstruction performance of 150 PCA")+
  theme(plot.title = element_text(hjust = 0.5)) 

g_1 = ggplot(soc_recon_75_long)+
  geom_line(aes(x=as.numeric(date),y=SOC),col='green') +
  ggtitle("Reconstruction (75 PCA) of predicted 'pass' battery")+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time")+
  ylab("SOC") +
  scale_y_continuous(limits = c(0,115))
g_1

g_2 = ggplot(soc_recon_75_long)+
  geom_line(aes(x=as.numeric(date),y=SOC),col='red') +
  ggtitle("Reconstruction (75 PCA) of predicted 'fail' battery")+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time")+
  ylab("SOC") +
  scale_y_continuous(limits = c(0,115))
g_2
