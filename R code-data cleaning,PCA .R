setwd("~/Documents/ESDA UCL 2019:2020/BENV0091 Energy Data Analysis/R Group Project/Battery/Data")

library(tidyverse)

#
# Data cleaning
# Data from InfluxDB are exported in .xlsx files for ease of use and are given the labels
# Data contains 4 columns: label, date, imei and voltage
# Import excel
influxdata <- read.csv("influxdata.csv", stringsAsFactors = F)

# Delete duplicate header
delheader <- which(influxdata$time == "time")
length(delheader)
influxdata_clean <- influxdata[-delheader,]
influxdata_clean$label <- as.factor(influxdata_clean$label)
influxdata_clean$imei <- as.factor(influxdata_clean$imei)
influxdata_clean$voltage <- as.numeric(influxdata_clean$voltage)
str(influxdata_clean)

# Add date column
influxdata_clean$date <- rep(1:8761, times=length(summary(influxdata_clean$imei)))

# Delete 8761
deldate <- which(influxdata_clean$date == "8761")
influxdata_new <- influxdata_clean[-deldate,]

# From long to wide format
influxdata_wide <- spread(influxdata_new, "imei", "voltage", nrow(influxdata_new))

# Handle missing data or NAs with imputeTS package
# Check how many missing values or NAs
table(is.na(influxdata_wide))

library(imputeTS)
influxdata_tidy <- na_kalman(influxdata_wide)
str(influxdata_tidy)
table(is.na(influxdata_tidy))

# From wide to long format
influxdata_long <- gather(influxdata_tidy,"imei","voltage",3:217) 

# Export to folder
path_rproject <- "~/Documents/ESDA UCL 2019:2020/BENV0091 Energy Data Analysis/R Group Project/Battery/Data"
write.csv(influxdata_long, file.path(path_rproject, "influxdata_long.csv"), row.names=FALSE)

#
# Voltage method for SOC estimations are calculated in Excel
# Processed data from Excel is in long format (in .csv format) and contains 5 columns: label, imei, date, voltage and SOC
soc_df <- read.csv("soc_df.csv", stringsAsFactors = F)

soc_df$imei <- as.factor(soc_df$imei)
soc_df$label <- as.factor(soc_df$label)
str(soc_df)
summary(soc_df$imei)
summary(soc_df$label)

soc_tidy <- subset(soc_df, select = c(label,imei,date,SOC))
soc_wide <- spread(soc_tidy, "date", "SOC", nrow(soc_df))
soc_tidy_1 = gather(soc_wide,date,SOC,3:8762)

#
# Perform dimensionality reduction: PCA
pca <- prcomp(soc_wide[,3:8762],center = FALSE,scale=FALSE)
plot(pca)

# PCA with one principal component
soc_recon_one <- pca$x[,1]%*%t(pca$rotation[,1]) # matrix multiplication of coefficient, back to original dimension
soc_recon_one <- data.frame(soc_recon_one)
soc_recon_one$imei <- soc_wide$imei
soc_recon_one_long <- gather(soc_recon_one,date,SOC,1:8760) # Columns are in X1-8760, imei and label order
soc_recon_one_long$date <- as.numeric(gsub("X","",(soc_recon_one_long$date))) 

# PCA with 75 principal components
soc_recon_75 <- pca$x[,1:75]%*%t(pca$rotation[,1:75]) 
soc_recon_75 <- data.frame(soc_recon_75)
soc_recon_75$imei <- soc_wide$imei
soc_recon_75_long <- gather(soc_recon_75,date,SOC,1:8760) # Columns are in X1-8760, imei and label order
soc_recon_75_long$date <- as.numeric(gsub("X","",(soc_recon_75_long$date))) 

# PCA with 150 principal components
soc_recon_150 <- pca$x[,1:150]%*%t(pca$rotation[,1:150]) 
soc_recon_150 <- data.frame(soc_recon_150)
soc_recon_150$imei <- soc_wide$imei
soc_recon_150_long <- gather(soc_recon_150,date,SOC,1:8760) # Columns are in X1-8760, imei and label order
soc_recon_150_long$date <- as.numeric(gsub("X","",(soc_recon_150_long$date))) 

# Plot graph to see the variance captured by the number of pricipal components
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2),
     xlab = 'Number of Principal Components',
     ylab = 'Cummulative of Variances Captured',
     main = 'Cummulative of Data Variances Captured vs Number of Principal Components')

# Plot graph 1 random original SOC profile
uniq_imei = unique(soc_tidy_1$imei) 
random_imei = uniq_imei[sample(length(uniq_imei), 1)] 
random_imei

ind = NULL
for(i in 1:length(random_imei)) {
  ind = c(ind,which(soc_tidy_1$imei==random_imei[i]))
}
ind

ggplot(soc_tidy_1[ind,]) + geom_line(aes(x=as.numeric(date),y = SOC, group = imei))

# Comparing PCA with one principal components and original data
ggplot(soc_recon_one_long[ind,])+
  geom_line(aes(x=date,y=SOC,group=imei),linetype=2)+
  geom_line(data=soc_tidy_1[ind,],
            aes(x=as.numeric(date),y=SOC,group=imei),col='red')
  
# Comparing PCA with 75 principal components and original data
ggplot(soc_recon_75_long[ind,])+
  geom_line(aes(x=date,y=SOC,group=imei),linetype=2)+
  geom_line(data=soc_tidy_1[ind,],
            aes(x=as.numeric(date),y=SOC,group=imei),col='red')

# Comparing PCA with 150 principal components and original data
ggplot(soc_recon_150_long[ind,])+
  geom_line(aes(x=date,y=SOC,group=imei),linetype=2)+
  geom_line(data=soc_tidy_1[ind,],
            aes(x=as.numeric(date),y=SOC,group=imei),col='red')+
  ggtitle("Reconstruction performance of 150 PCA")+
  theme(plot.title = element_text(hjust = 0.5)) 

#
# Create train and test set for machine learning
# Choose only label and date 1-8760
set.seed(42) # set seed for reproducibility

soc_1 <- subset(soc_recon_one,select = c(8762,1:8760))
soc_1$label <- as.factor(soc_1$label)
rows <- sample(nrow(soc_1))
shuffled_train_clean1 <- soc_1[rows,]
split <- round(nrow(shuffled_train_clean1)*0.8)
train1 <- droplevels(shuffled_train_clean1[1:split,])
test1 <- droplevels(shuffled_train_clean1[(split+1):nrow(shuffled_train_clean1),])
str(train1)
str(test1)

soc_75 <- subset(soc_recon_75,select = c(8762,1:8760))
soc_75$label <- as.factor(soc_75$label)
rows75 <- sample(nrow(soc_75))
shuffled_train_clean75 <- soc_75[rows75,]
split75 <- round(nrow(shuffled_train_clean75)*0.8)
train75 <- droplevels(shuffled_train_clean75[1:split75,])
test75 <- droplevels(shuffled_train_clean75[(split75+1):nrow(shuffled_train_clean75),])
str(train75)
str(test75)

soc_150 <- subset(soc_recon_150,select = c(8762,1:8760))
soc_150$label <- as.factor(soc_150$label)
rows150 <- sample(nrow(soc_150))
shuffled_train_clean150 <- soc_150[rows150,]
split150 <- round(nrow(shuffled_train_clean150)*0.8)
train150 <- droplevels(shuffled_train_clean150[1:split150,])
test150 <- droplevels(shuffled_train_clean150[(split150+1):nrow(shuffled_train_clean150),])
str(train150)
str(test150)


