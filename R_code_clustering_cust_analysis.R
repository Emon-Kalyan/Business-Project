library(tidyverse)
library(gt)
library(dplyr)
library(cluster)
library(factoextra)
library(GPArotation)
library(psych)
library(sqldf)
##Understanding the data
x_data_updated <- read.csv("x_data.csv")
View(x_data_updated)
str(x_data_updated)
dim(x_data_updated)
names(x_data_updated)
x_data_updated <- x_data_updated %>% drop_na()
x_cust_data <- x_data_updated %>% group_by(customer_id) %>% summarise(age = max(age),
                                      basketsizes = basketsizes, mean_price_per_product = mean_price_by_customers)
cust_data <- unique(x_cust_data)
View(cust_data)                        
###EDA
#Let us check the correlation amongst the variables which we will use for the clustering
corrm <- cor(cust_data[c('age', 'mean_price_per_product', 'basketsizes')])
View(corrm)
##We see that there is little correlation amongst the variables age, mean_pricebytransaction and basketsizes
eigenvalues <- mutate(data.frame(eigen(corrm)$values), cum_sum_eigen=cumsum(eigen.corrm..values), pct_var=eigen.corrm..values/sum(eigen.corrm..values), cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))
View(eigenvalues)
##From the eigenvalues and the correlation matrix we can see that,
##we can use all the three variables - age, mean_pricebytransaction and basketsizes
##for the clustering
scree(corrm, factors = TRUE, pc = T, hline = NULL, add = FALSE )
##The scree plot suggests the ideal number of clusters between 2 to 3 and
##the eigen values suggest that the ideal number of clusters is 3. So, we take 3.
##
cust_data_num = cust_data[c('age', 'mean_price_per_product', 'basketsizes')]
cust_data_scaled  = scale(cust_data_num)
View(cust_data_scaled)
cluster3xdata <- kmeans(cust_data_scaled, 3 )
fviz_cluster(cluster3xdata, data = cust_data_scaled, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
cluster2xdata <- kmeans(cust_data_scaled, 2 )
fviz_cluster(cluster2xdata, data = cust_data_scaled, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
cust_data$kmeanscluster <- cluster3xdata$cluster
View(cust_data)
table(cluster3xdata$cluster)
prop.table(table(cluster3xdata$cluster))
kmeanssegments <- aggregate(cust_data_num, by = list(cluster = cust_data$kmeanscluster), mean)
View(kmeanssegments)
write.csv(kmeanssegments, 'kmeanssegments_summ.csv')
x_data_updated <- x_data_updated %>% inner_join(cust_data, by='customer_id')
View(x_data_updated)
write.csv(x_data_updated, 'x_data_withSeptembersegemnts.csv')
x_data_widsegments <- x_data_updated
summary(aov(basketsizes.x~kmeanscluster, data = x_data_widsegments))

###--------checking the association of variables for basket sizes-----------###
colnames(x_data_updated)
summary(aov(basketsizes.x~kmeanscluster, data = x_data_updated))
summary(aov(basketsizes.x~garment_group_name, data = x_data_updated))
summary(aov(basketsizes.x~graphical_appearance_name, data = x_data_updated))
summary(aov(basketsizes.x~product_type_name, data = x_data_updated))



