setwd("C:\\Users\\varun\\OneDrive - The University of Texas at Dallas\\_University\\BA with R\\Proj")
#rm

#-----------------------------------Loading data-----------------------------------
df = read.csv("Online Retail(UK2).csv")
nrow(df) #440374

#-----------------------------------Data Cleaning-----------------------------------
#check na
na = is.na(df)
na = apply(na, 2, sum)
na  #135080

#remove na
df.na = na.omit(df)
na = is.na(df)
na = apply(na, 2, sum)
head(df.na);nrow(df.na) #322868

library(dplyr)
#correct description from unique.items
df2 <- df.na %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "", 
                               Description[which(Description != "")[1]],
                               Description))
df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "?", 
                               Description[which(Description != "?")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "??", 
                               Description[which(Description != "??")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "?? missing", 
                               Description[which(Description != "?? missing")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "?missing", 
                               Description[which(Description != "?missing")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "amazon", 
                               Description[which(Description != "amazon")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "Amazon", 
                               Description[which(Description != "Amazon")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "amazon adjust", 
                               Description[which(Description != "amazon adjust")[1]],
                               Description))
df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "Amazon Adjustment", 
                               Description[which(Description != "Amazon Adjustment")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "amazon sales", 
                               Description[which(Description != "amazon sales")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "Amazon sold sets", 
                               Description[which(Description != "Amazon sold sets")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "sold as set on dotcom and amazon", 
                               Description[which(Description != "sold as set on dotcom and amazon")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "? sold as sets?", 
                               Description[which(Description != "? sold as sets?")[1]],
                               Description))

df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "????missing", 
                               Description[which(Description != "????missing")[1]],
                               Description))
df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "???missing", 
                               Description[which(Description != "???missing")[1]],
                               Description))

df2 <- df2 %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "???", 
                               Description[which(Description != "???")[1]],
                               Description))

df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "????damages????", 
                               Description[which(Description != "????damages????")[1]],
                               Description))
df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "???lost", 
                               Description[which(Description != "???lost")[1]],
                               Description))
df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "20713", 
                               Description[which(Description != "20713")[1]],
                               Description))

df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "?display?", 
                               Description[which(Description != "?display?")[1]],
                               Description))
df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "?lost", 
                               Description[which(Description != "?lost")[1]],
                               Description))

df2 <- df2 %>%  
  group_by(StockCode) %>%
  mutate(Description = ifelse(Description == "?sold as sets?", 
                               Description[which(Description != "?sold as sets?")[1]],
                               Description))


#remove if invoiceNo starts with C (These are cancelled orders)

df.noc <- df2[!grepl("^C", df2$InvoiceNo), ]
df.noc

#imputes mean value where UnitPrice is 0    2352 values imputed
nozero.conv <- df.noc %>%  
  group_by(StockCode) %>%
  mutate(UnitPrice = ifelse(UnitPrice == 0, 
                               mean(UnitPrice[which(UnitPrice != 0)]),
                               UnitPrice))




write.csv(nozero.conv, file = "Test1.csv", row.names = FALSE)  #1470 values imputed


#filter only UK data
dfuk <- nozero.conv[nozero.conv$Country == "United Kingdom",]
nrow(dfuk)

#mutate to create amount column
dfuk <- dfuk %>% 
  mutate(Amount = Quantity * UnitPrice)

View(dfuk)

write.csv(dfuk, file = "TestUK.csv", row.names = FALSE)

#-------------------------------------Visualizations----------------------------------------------

#compute total quantity for each StockCode
 df.count <- dfuk%>%
   group_by(StockCode,Description,Country) %>%
   summarize(TotalQuantity = sum(Quantity, na.rm = TRUE))

#sort by total quantity
df.count <- df.count[order(-df.count$TotalQuantity),]

# top 10 items in uk by quantity
library(ggplot2)
ggplot(df.count[1:10,], aes(x = reorder(Description, -TotalQuantity), y = TotalQuantity, fill= Description))+ 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  labs(x = "Item", y = "Total Quantity")


#avg time between orders
dfuk$InvoiceDate <- as.Date(dfuk$InvoiceDate, format = "%m/%d/%Y %H:%M")
df_avg_time <- dfuk %>%
  group_by(CustomerID) %>%
  mutate(avg_time = as.numeric(difftime(InvoiceDate, lag(InvoiceDate), units = "days"))) %>%
  ungroup()

#filter customers from UK
#df_avg_time <- df_avg_time[df_avg_time$Country == "United Kingdom",]

#calculate average time
df_avg_time <- df_avg_time %>%
  group_by(CustomerID) %>%
  summarize(avg_time = mean(avg_time, na.rm = TRUE))


df_avg_time$avg_time<-round(df_avg_time$avg_time, digits = 0)
View(df_avg_time)
summary(df_avg_time$avg_time)

#write.csv(df_avg_time, file = "avg_time.csv", row.names = FALSE)

#append avg time to original data depending on customerID
# merged_df <- merge(dfuk, df_avg_time, by = "CustomerID", all.x = TRUE)
# dfuk$avg_time <- merged_df$avg_time
# #quantity >500
# View(merged_df)

#revenue by day of week
dfuk$InvoiceDate <- as.Date(dfuk$InvoiceDate, format = "%m/%d/%Y %H:%M")

days<-dfuk %>%
  group_by(dayOFweek = weekdays(InvoiceDate)) %>%
  summarize(revenue = sum(Quantity*UnitPrice, na.rm = TRUE))

ggplot(days,aes(x=dayOFweek,y=revenue))+geom_col()+labs(x="Day of Week",y="Revenue",title = "Revenue by Day of Week")
View(days)

#number of transactions by day of week
weekTrans<-dfuk %>%
  group_by(CustomerID,InvoiceNo)%>%
  summarise(transactions=n(),day_of_week=weekdays(InvoiceDate))
  

ggplot(weekTrans,aes(x=day_of_week,y=transactions))+geom_boxplot()+labs(x="Day of Week",y="Transactions",title = "Transactions by Day of Week")

View(weekTrans)


#revenue by customer
custRev<-dfuk%>%
  group_by(CustomerID)%>%
  summarise(revenue = sum(Quantity*UnitPrice, na.rm = TRUE))

#plot revenue by customer
ggplot(custRev,aes(x=revenue))+geom_histogram(bins=100)+labs(x="Revenue",y="Count of Customers",title = "Revenue by Customer")

#plot log scale revenue by customer
library(scales)
ggplot(custRev,aes(x=revenue))+geom_histogram(bins=25)+labs(x="Revenue",y="Count of Customers",title = "Revenue by Customer (log scale)")+scale_x_log10(labels = label_number(scale = 1))

#transactions by customer
# transCust<-dfuk%>%
#   group_by(CustomerID)%>%
#   summarise(transactions = n())

# ggplot(transCust,aes(x=transactions))+geom_histogram()+labs(x="Transactions",y="Count of Customers",title = "Transactions by Customer (log scale)")+scale_x_log10(labels = label_number(scale = 1))

#--------------------------Clustering---------------------------------------------
Data=read.csv("TestUK.csv")
Data_uk=Data[Data$Country=="United Kingdom",]
#Data_uk$InvoiceDate

x <- as.Date(Data_uk[,"InvoiceDate"], "%m/ %d/ %Y")
#x


max_date=max(x)
max_date

Data_uk$active=max_date-x
View(Data_uk)
Data_uk$active=as.numeric(Data_uk$active)

library(dplyr)
Data_Cust_active <- Data_uk %>%
  group_by(CustomerID) %>%
  summarize(Amount=sum(Amount),active=min(active),Invoice=count(data.frame(InvoiceNo)))

View(Data_Cust_active)


z=quantile(Data_Cust_active$Amount,0.05)
y=quantile(Data_Cust_active$Amount,0.95)
Inter_quanile_R=y-z
Inter_quanile_R
Data_Cust_active=Data_Cust_active[Data_Cust_active$Amount>z-1.5*Inter_quanile_R & Data_Cust_active$Amount<=y+1.5*Inter_quanile_R,]
Data_Cust_active


Inv_x=quantile(Data_Cust_active$Invoice$n,0.05)
Inv_y=quantile(Data_Cust_active$Invoice$n,0.95)
Inter_r=Inv_y-Inv_x
Data_Cust_active=Data_Cust_active[Data_Cust_active$Invoice>Inv_x-1.5*Inter_r & Data_Cust_active$Invoice<=Inv_y+1.5*Inter_r,]
Data_Cust_active

Act_x=quantile(Data_Cust_active$active,0.05)
Act_y=quantile(Data_Cust_active$active,0.95)
InterA_r=Act_y-Act_x
Data_Cust_active=Data_Cust_active[Data_Cust_active$active>Act_x-1.5*InterA_r & Data_Cust_active$active<=Act_y+1.5*InterA_r,]
Data_Cust_active



Data_scale=sapply(Data_Cust_active,scale)
Data_scale




pca_n<- prcomp(Data_Cust_active[,c(2,3,4)])
summary(pca_n)


scores1 <- as.data.frame(pca_n$x)
# scores1
set.seed(123)
clus<- kmeans(scores1,  centers = 2, nstart = 10)

library(cluster)
choosek <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(choosek) <- c("numClusters", "totWithinSS", "avg_silhouette")
for (k in 1:10) {
  set.seed(123)
  tempkm <- kmeans(scores1,  centers = k, nstart = 10)
  
  if (k==1) {
    ss <- 0
  } else {
    ss <- silhouette(tempkm$cluster, dist(scores1))[, 3]
  }
  
  # append statistics
  tempdf <- data.frame(numClusters = k, totWithinSS = tempkm$tot.withinss, avg_silhouette = mean(ss))
  choosek <- rbind(choosek, tempdf)
}

library(ggplot2)
g <- ggplot(choosek, aes(numClusters, avg_silhouette))
g <- g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Average Silhouette")
g + geom_text(aes(label=round(avg_silhouette, 3)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))

#alternatively, use fviz_nbclust() function
library(factoextra)
set.seed(123)
fviz_nbclust(scores1, kmeans, nstart = 25, k.max = 10, method = "wss")

set.seed(123)
fviz_nbclust(scores1, kmeans, nstart = 25, k.max = 10, method = "silhouette")

#We chose k=3 based on the elbow plot and silhouette plot
#for analysis purpose
# k = 3
# km = kmeans(Data_Cust_active[,c(2,3,4)], k)
# sil <- silhouette(km$cluster, dist(Data_Cust_active[,c(2,3,4)]))
#cat("Silhouette Score:", mean(sil[,3])) 


clus_new=kmeans(Data_Cust_active[,c(2,3,4)],centers = 3,nstart = 50)
Data_Cust_active$cluster=clus_new$cluster
Data_Cust_active

aggdata <- aggregate(cbind(Amount,active,Invoice$n) ~ cluster, data=Data_Cust_active, mean )
aggdata

Data_Cust_active$cluster=as.factor(Data_Cust_active$cluster)


library(ggplot2)
ggplot(Data_Cust_active,aes(group=cluster,y=Amount))+geom_boxplot(aes(cluster,Amount),outlier.colour = "red")+xlab("cluster")+ylab("Amount")

ggplot(Data_Cust_active,aes(group=cluster,y=active))+geom_boxplot(aes(cluster,active),outlier.colour = "red")+xlab("cluster")+ylab("Active")


ggplot(Data_Cust_active,aes(group=cluster,y=Invoice$n))+geom_boxplot(aes(cluster,Invoice$n),outlier.colour = "red")+xlab("cluster")+ylab("Invoice")

ggplot(aggdata, aes(cluster, active)) +geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)

# ggplot(Data_Cust_active,aes(active,Amount))+geom_point(aes(color=cluster))
# ggplot(Data_Cust_active,aes(Amount))+geom_histogram()
# ggplot(Data_Cust_active,aes(active))+geom_histogram()

# scores1 <- as.data.frame(pca_n$x)
# set.seed(123)
# clus_f<- kmeans(scores1,  centers = 3, nstart = 10)
# # visualize the cluster using PC1 & PC2
# scores1$cluster <- as.character(clus_f$cluster)
# g <- ggplot(scores1, aes(PC1, PC2))
# g + geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0) # label dot using the cluster name
# # alternatively use factoextra
# fviz_cluster(clus_f, geom="point", data = scores1[,1:2]) + ggtitle("k=3")
# summary(clus_f)

# survey <- cbind(Data_Cust_active[,c(2,3,4)], cluster = clus_f$cluster)
# summary(survey)

# survey$cluster <- as.factor(survey$cluster)

# aggdata <- aggregate(. ~ cluster, data=clus_f, mean )
# aggdata

# ggplot(aggdata, aes(cluster, active)) +geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)
