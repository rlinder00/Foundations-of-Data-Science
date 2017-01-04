#install.packages("rmarkdown")
#library(rmarkdown)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("flexclust")
library(flexclust)
#install.packages("NbClust")
library(NbClust)
setwd("/users/teams/nad/ryhl/Capstone/")

#rmarkdown::render("Capstone_Content_Recommendation_Engine.Rmd")

set.seed(1)
split_ratio = .7

data_new <- read.csv("/users/teams/nad/ryhl/Capstone/DIGITAL_ENGAGEMENT_FLAG/model_data_clean.csv")
row.names(data_new) <- NULL
data_new <- filter(data_new, DIGITAL_ENGAGEMENT_FLAG == 1) #use only digitally engaged population 
data_new <- data_new[, -c(1, 3)] #remove row number column and digital engagement flag from data


#train_t <- sample_frac(data_new, split_ratio)
#test_t <- setdiff(data_new, train_t)
#train_t[,-1] <- scale(train_t[, -1])
#test_t[,-1] <- scale(test_t[, -1])
train_t <- data_new
train_t[,-1] <- scale(train_t[, -1])

write.csv(train_t, "/users/teams/nad/ryhl/Capstone/DIGITAL_ENGAGEMENT_FLAG/clustering_train_t.csv")

###################################################################

###### K-MEANS CLUSTERING

###################################################################

# scale or not
row.names(train_t) <- NULL
head(train_t)

# run k-means on chosen cluster amount 
train_k_means <- kmeans(train_t[,-1], 6, iter.max = 1000, algorithm = "Lloyd")
str(train_k_means)

train_t_cluster <- train_k_means$cluster
train_t <- cbind(train_t, unname(train_t_cluster))
#colnames(train)
train_t <- rename(train_t, c("unname(train_t_cluster)" ="CLUSTER"))
train_t$CLUSTER <- as.factor(train_t$CLUSTER)

###################################################################
###################################################################

#centers <- KMC$centers
train_k_means_kcca <- as.kcca(KMC, train)
test_cluster <- predict(KMC.kcca, new_data = test)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

# determine optimal number of cluster
#nc <- NbClust(train_t[,-1], min.nc=5, max.nc=15, method="kmeans")
#barplot(table(nc$Best.n[1,]),
#        xlab="Numer of Clusters", ylab="Number of Criteria",
#        main="Number of Clusters Chosen by 26 Criteria")

#NbClust(m3, distance="euclidean", min.nc=2, max.nc=20, method="kmeans", index="dindex")



###################################################################

###### JOINING BACK TO BEHAVIOR TABLE

###################################################################

#xtract_data("select * from [MSS_U_NADBI_S].[dbo].[RYHL_CAPSTONE_DATA]", "lasr", "/users/teams/nad/ryhl/Capstone/Content_Rec_Data_Raw.xdf")
data_w_con_pillars <- rxReadXdf("/users/teams/nad/ryhl/Capstone/Content_Rec_Data_Raw.xdf", stringsAsFactors = T)
temp <- left_join(train_t, data_w_con_pillars, by = "FINCL_ITRMY_PRSON_UID")
new_train <- temp[colnames(temp) %in% c("FINCL_ITRMY_PRSON_UID", paste0(colnames(train_t[,-1]), ".x"), "CLUSTER", "CONTENT_PILLAR_CATEGORY", "CLICK_CNT")] 


#######

pie_chart <- 
  new_train %>% 
  dplyr::select(CLUSTER, CONTENT_PILLAR_CATEGORY, CLICK_CNT) %>% 
  dplyr::group_by(CLUSTER, CONTENT_PILLAR_CATEGORY) %>% 
  dplyr::summarise(CLICKS = sum(CLICK_CNT)) %>% 
  dplyr::arrange(CLUSTER, CONTENT_PILLAR_CATEGORY, CLICKS)

write.csv(pie_chart, "/users/teams/nad/ryhl/Capstone/pie_chart_k_means.csv")

cluster <- '2'
pie_chart_c <- 
  pie_chart %>% 
  ungroup %>% 
  filter(CLUSTER == cluster) %>% 
  arrange(desc(CLICKS))

sum <- sum(pie_chart_c$CLICKS)

par(mar = c(11,4,4,2) + 0.1)
plot(pie_chart_c$CLICKS/sum, xaxt="n", xlab = '', ylab = 'Proportion of Clicks By Pillar')
axis(1, at=1:12, pie_chart_c$CONTENT_PILLAR_CATEGORY, las = 2, cex.axis = 0.6)





pie(pie_chart_c$CLICKS,labels = pie_chart_c$CONTENT_PILLAR_CATEGORY,  
    col=rainbow(length(pie_chart_c$CONTENT_PILLAR_CATEGORY)),
    init.angle = 90,
    main=paste0("CONTENT PREFERENCES FROM CLUSTER ", cluster),
    clockwise = T)

dev.off() 



pie_chart_c$CONTENT_PILLAR_CATEGORY







 ###################################################################

###### HEIRARCHICAL CLUSTERING

###################################################################

#calculate distince
dists <- dist(train, method = "euclidean") 

#build clusters
clusts <- hclust(dists, method = "ward")
plot(clusts)

#choose number of clusters
clustGroup <- cutree(clusts, 6)
train <- cbind(train, unname(clustGroup))
train <- rename(train, c("unname(clustGroup)" ="CLUSTER"))
train$CLUSTER <- as.factor(train$CLUSTER)

#tapply(movies$Action, clustGroup, mean)




#ggplot(data = pie_chart[pie_chart$CLUSTER == 1], aes(x = as.factor(pie_chart[pie_chart$CLUSTER == 1]$CONTENT_PILLAR_CATEGORY), fill = factor(pie_chart[pie_chart$CLUSTER == 1]$CLICKS))) + geom_bar(width = 1) + coord_polar(theta = "y")
