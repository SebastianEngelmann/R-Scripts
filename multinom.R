rm(list = ls(all.names = TRUE))
setwd("/home/sebastian/Dokumente/GitHubRepos/WINFO_Projekt/")
#setwd("/Users/sebastianengelmann/Documents/GitHub/WINFO_Projekt/")

library(scales)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)
library(nnet)
library(rpart)
library(caret)
library(AER)

# import dataset
# dataset <- fread("Kundenkonto.csv", header = TRUE, sep = ";", encoding = "UTF-8")
cluster <- fread("OrderNo_ClusterID_Clustering.csv")
#forest <- fread("OrderNo_ClusterID_RandomForest.csv")
berlin <- fread("Berlin_Header_Final.csv")
#berlin <- fread("Berlin_Header_2017_01_11.csv")

vec <- (cluster$cluster_id == forest$ClusterID)
sum(vec) / length(vec)

# tmp <- count(dataset, Email)

# ggplot(tmp, aes(x = Email, y = n)) + geom_point(color = "blue", alpha = .5)

# Unplausible Beobachtungen
# dim(filter(tmp, n >= 96))[1] # 4 Bestellungen im Monat
# dim(filter(tmp, n >= 72))[1] # 3 Bestellungen im Monat
# dim(filter(tmp, n >= 48))[1] # 2 Bestellungen im Monat
# dim(filter(tmp, n >= 24))[1] # 1 Bestellung im Monat
# filter(tmp, n >= 250)

# Anzahl Kunden: 1 Bestellung
# dim(filter(tmp, n == 1))[1]
# Anzahl Kunden: 2 Bestellungen
# dim(filter(tmp, n == 2))[1]
# Anzahl Kunden: 3 Bestellung
# dim(filter(tmp, n == 3))[1]
# Anzahl Kunden: 4 Bestellungen
# dim(filter(tmp, n == 4))[1]
# Anzahl Kunden: 5 Bestellungen
# dim(filter(tmp, n == 5))[1]

#cluster$OrderNos <- as.numeric(cluster$OrderNos)
#berlin$OrderNo <- as.character(berlin$OrderNo) 
colnames(cluster)[2] <- "OrderNo"

intersect(berlin$OrderNo, cluster$OrderNo)
berlin$OrderNo[1]
cluster$OrderNo[1]

cluster$cluster_id <- as.factor(cluster$cluster_id)

cluster$OrderNo <- as.numeric(gsub(",", ".", gsub("\\.", "", cluster$OrderNo)))
berlin$lat <- as.numeric(gsub(",", ".", gsub("\\.", "", berlin$lat)))
berlin$lon <- as.numeric(gsub(",", ".", gsub("\\.", "", berlin$lon)))
#clustering <- merge(berlin, cluster, by = "OrderNo")
clustering <- merge(berlin, cluster, by = "OrderNo")

random_forest <- merge(berlin, forest, by = "OrderNo")


dat <- clustering
#dat <- random_forest

#dat$TimeSlot[dat$LowerBound == "10:00" & dat$UpperBound == "12:00"] <- "10:00-12:00"
#dat$TimeSlot[dat$LowerBound == "12:00" & dat$UpperBound == "14:00"] <- "12:00-14:00"
#dat$TimeSlot[dat$LowerBound == "14:00" & dat$UpperBound == "16:00"] <- "14:00-16:00"
#dat$TimeSlot[dat$LowerBound == "16:00" & dat$UpperBound == "18:00"] <- "16:00-18:00"
#dat$TimeSlot[dat$LowerBound == "18:00" & dat$UpperBound == "20:00"] <- "18:00-20:00"
#dat$TimeSlot[dat$LowerBound == "20:00" & dat$UpperBound == "22:00"] <- "20:00-22:00"

dat$TimeSlot <- factor(dat$TimeSlot)
dat$lat <- as.numeric(dat$lat)
dat$lon <- as.numeric(dat$lon)
summary(dat$lat)
summary(dat$lon)

#dat <- dat[!is.na(dat$TimeSlot), ]
dat$DeliveryType <- factor(dat$DeliveryType)
dat$DeliveryType <- relevel(dat$DeliveryType, ref = "STANDARD")


dat$OTZ[dat$OrderTime >= "00:00" & dat$OrderTime < "02:00"] <- "00:00-02:00"
dat$OTZ[dat$OrderTime >= "02:00" & dat$OrderTime < "04:00"] <- "02:00-04:00"
dat$OTZ[dat$OrderTime >= "04:00" & dat$OrderTime < "06:00"] <- "04:00-06:00"
dat$OTZ[dat$OrderTime >= "06:00" & dat$OrderTime < "08:00"] <- "06:00-08:00"
dat$OTZ[dat$OrderTime >= "08:00" & dat$OrderTime < "10:00"] <- "08:00-10:00"
dat$OTZ[dat$OrderTime >= "10:00" & dat$OrderTime < "12:00"] <- "10:00-12:00"
dat$OTZ[dat$OrderTime >= "12:00" & dat$OrderTime < "14:00"] <- "12:00-14:00"
dat$OTZ[dat$OrderTime >= "14:00" & dat$OrderTime < "16:00"] <- "14:00-16:00"
dat$OTZ[dat$OrderTime >= "16:00" & dat$OrderTime < "18:00"] <- "16:00-18:00"
dat$OTZ[dat$OrderTime >= "18:00" & dat$OrderTime < "20:00"] <- "18:00-20:00"
dat$OTZ[dat$OrderTime >= "20:00" & dat$OrderTime < "22:00"] <- "20:00-22:00"
dat$OTZ[dat$OrderTime >= "22:00" & dat$OrderTime < "24:00"] <- "22:00-00:00"

dat$OTZ <- factor(dat$OTZ)

dat$Eltern <- factor(dat$Eltern)
dat$ProductCount

summary(dat$TimeSlot)
#dat <- filter(dat, TimeSlot != "" & TimeSlot != "06:30-08:00" & TimeSlot != "07:00-09:00" & TimeSlot != "09:00-11:00")
dat <- filter(dat, TimeSlot != "06:30-08:00" & TimeSlot != "07:00-09:00" & TimeSlot != "09:00-11:00")
dat$TimeSlot <- droplevels(dat$TimeSlot)
summary(dat$TimeSlot)

summary(dat$TimeSlot) / sum(summary(dat$TimeSlot))
summary(dat$DeliveryType) / sum(summary(dat$DeliveryType))

#dat <- filter(dat, TimeSlot != "18:00-20:00")
#dat$TimeSlot <- droplevels(dat$TimeSlot)

str(dat)
dat$SelectedDeliveryWeekday <- as.factor(dat$SelectedDeliveryWeekday)
dat$VoucherType <- as.factor(dat$VoucherType)


# set sample size to 67 percent of data
smp_size <- floor(0.67 * nrow(dat))
#smp_size <- floor(0.01 * nrow(dat))
# set the seed to make partion reproductible
set.seed(1982)

# create set 1
train_ids1 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train1 <- dat[train_ids1, ]
# dertermine test data set
test1 <- dat[-train_ids1, ]

# create set 2
train_ids2 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train2 <- dat[train_ids2, ]
# dertermine test data set
test2 <- dat[-train_ids2, ]

# create set 3
train_ids3 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train3 <- dat[train_ids3, ]
# dertermine test data set
test3 <- dat[-train_ids3, ]

# create set 4
train_ids4 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train4 <- dat[train_ids4, ]
# dertermine test data set
test4 <- dat[-train_ids4, ]

# create set 5
train_ids5 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train5 <- dat[train_ids5, ]
# dertermine test data set
test5 <- dat[-train_ids5, ]

# create set 6
train_ids6 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train6 <- dat[train_ids6, ]
# dertermine test data set
test6 <- dat[-train_ids6, ]

# create set 7
train_ids7 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train7 <- dat[train_ids7, ]
# dertermine test data set
test7 <- dat[-train_ids7, ]

# create set 8
train_ids8 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train8 <- dat[train_ids8, ]
# dertermine test data set
test8 <- dat[-train_ids8, ]

# create set 9
train_ids9 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train9 <- dat[train_ids9, ]
# dertermine test data set
test9 <- dat[-train_ids9, ]

# create set 10
train_ids10 <- sample(seq_len(nrow(dat)), size = smp_size)
# determine training data set
train10 <- dat[train_ids10, ]
# dertermine test data set
test10 <- dat[-train_ids10, ]

# dat <- filter(dat, TimeSlot != "18:00-20:00")

str(dat)

# NewCustomerFreq
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$OrderSum))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$VoucherType))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$lat))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$lon))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$cluster_id))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$OTZ))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$ProductCount))
cor(as.numeric(dat$NewCustomerFreq), as.numeric(dat$Eltern))


# SelectedDeliveryWeekday
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$OrderSum))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$VoucherType))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$lat))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$lon))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$cluster_id))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$OTZ))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$ProductCount))
cor(as.numeric(dat$SelectedDeliveryWeekday), as.numeric(dat$Eltern))


# DiffOrderSelectedDelivery
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$OrderSum))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$VoucherType))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$lat))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$lon))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$cluster_id))
cor(as.numeric(dat$DiffOrderSelectedDelivery), as.numeric(dat$OTZ))
cor(as.numeric(dat$DaysToDelivery), as.numeric(dat$ProductCount))
cor(as.numeric(dat$DaysToDelivery), as.numeric(dat$Eltern))

# DeliveryType
cor(as.numeric(dat$DeliveryType), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$OrderSum))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$VoucherType))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$lat))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$lon))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$cluster_id))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$OTZ))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$ProductCount))
cor(as.numeric(dat$DeliveryType), as.numeric(dat$Eltern))

# TimeSlot
cor(as.numeric(dat$TimeSlot), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$OrderSum))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$VoucherType))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$lat))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$lon))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$cluster_id))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$OTZ))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$ProductCount))
cor(as.numeric(dat$TimeSlot), as.numeric(dat$Eltern))

# OrderSum
cor(as.numeric(dat$OrderSum), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$OrderSum), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$OrderSum), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$OrderSum), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$OrderSum), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$OrderSum), as.numeric(dat$VoucherType))
cor(as.numeric(dat$OrderSum), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$OrderSum), as.numeric(dat$lat))
cor(as.numeric(dat$OrderSum), as.numeric(dat$lon))
cor(as.numeric(dat$OrderSum), as.numeric(dat$cluster_id))
cor(as.numeric(dat$OrderSum), as.numeric(dat$OTZ))
cor(as.numeric(dat$OrderSum), as.numeric(dat$ProductCount))
cor(as.numeric(dat$OrderSum), as.numeric(dat$Eltern))

# VoucherType
cor(as.numeric(dat$VoucherType), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$VoucherType), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$VoucherType), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$VoucherType), as.numeric(dat$ProductCount))
cor(as.numeric(dat$VoucherType), as.numeric(dat$Eltern))

# VoucherValue
cor(as.numeric(dat$VoucherValue), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$OrderSum))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$VoucherType))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$lat))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$lon))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$cluster_id))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$OTZ))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$ProductCount))
cor(as.numeric(dat$VoucherValue), as.numeric(dat$Eltern))

# lat
cor(as.numeric(dat$lat), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$lat), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$lat), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$lat), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$lat), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$lat), as.numeric(dat$OrderSum))
cor(as.numeric(dat$lat), as.numeric(dat$VoucherType))
cor(as.numeric(dat$lat), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$lat), as.numeric(dat$lon))
cor(as.numeric(dat$lat), as.numeric(dat$cluster_id))
cor(as.numeric(dat$lat), as.numeric(dat$OTZ))
cor(as.numeric(dat$lat), as.numeric(dat$ProductCount))
cor(as.numeric(dat$lon), as.numeric(dat$Eltern))

# lon
cor(as.numeric(dat$lon), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$lon), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$lon), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$lon), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$lon), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$lon), as.numeric(dat$OrderSum))
cor(as.numeric(dat$lon), as.numeric(dat$VoucherType))
cor(as.numeric(dat$lon), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$lon), as.numeric(dat$lat))
cor(as.numeric(dat$lon), as.numeric(dat$cluster_id))
cor(as.numeric(dat$lon), as.numeric(dat$OTZ))
cor(as.numeric(dat$lon), as.numeric(dat$ProductCount))
cor(as.numeric(dat$lon), as.numeric(dat$Eltern))

# cluster_id
cor(as.numeric(dat$cluster_id), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$cluster_id), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$cluster_id), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$cluster_id), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$cluster_id), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$cluster_id), as.numeric(dat$OrderSum))
cor(as.numeric(dat$cluster_id), as.numeric(dat$VoucherType))
cor(as.numeric(dat$cluster_id), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$cluster_id), as.numeric(dat$lat))
cor(as.numeric(dat$cluster_id), as.numeric(dat$lon))
cor(as.numeric(dat$cluster_id), as.numeric(dat$OTZ))
cor(as.numeric(dat$cluster_id), as.numeric(dat$ProductCount))
cor(as.numeric(dat$cluster_id), as.numeric(dat$Eltern))

# OTZ
cor(as.numeric(dat$OTZ), as.numeric(dat$NewCustomerFreq))
cor(as.numeric(dat$OTZ), as.numeric(dat$SelectedDeliveryWeekday))
cor(as.numeric(dat$OTZ), as.numeric(dat$DiffOrderSelectedDelivery))
cor(as.numeric(dat$OTZ), as.numeric(dat$DeliveryType))
cor(as.numeric(dat$OTZ), as.numeric(dat$TimeSlot))
cor(as.numeric(dat$OTZ), as.numeric(dat$OrderSum))
cor(as.numeric(dat$OTZ), as.numeric(dat$VoucherType))
cor(as.numeric(dat$OTZ), as.numeric(dat$VoucherValue))
cor(as.numeric(dat$OTZ), as.numeric(dat$lat))
cor(as.numeric(dat$OTZ), as.numeric(dat$lon))
cor(as.numeric(dat$OTZ), as.numeric(dat$cluster_id))
cor(as.numeric(dat$OTZ), as.numeric(dat$ProductCount))
cor(as.numeric(dat$OTZ), as.numeric(dat$Eltern))

f <- formula(DeliveryType ~ NewCustomerFreq + SelectedDeliveryWeekday + DaysToDelivery + OrderSum + VoucherType + VoucherValue + lat + lon + cluster_id + OTZ + Eltern + OrderSum)
g <- c(dat$NewCustomerFreq, dat$SelectedDeliveryWeekday, dat$DaysToDelivery, dat$OrderSum, dat$VoucherType, dat$VoucherValue, dat$lat, dat$lon, dat$cluster_id, dat$OTZ, dat$Eltern, dat$OrderSum)
#f <- formula(TimeSlot ~ NewCustomerFreq + SelectedDeliveryWeekday + DiffOrderSelectedDelivery + OrderSum + VoucherType + VoucherValue + lat + lon + OTZ)


# Interpretation
# beta_k < 0 => exp(beta_k) < 1
# Die Chance der Realisierung von der Kategorie j nimmt relativ zur Referenzkategorie m mit Zunahme von X_k ab 
# beta = 0 => exp(beta) = 1
# Die Chance der Realisierung von der Kategorie j bleibt relativ zur Referenzkategorie m mit Zunahme von X_k gleich 
# beta > 0 => exp(beta) > 1
# Die Chance der Realisierung von der Kategorie j nimmt relativ zur Referenzkategorie m mit Zunahme von X_k zu
# Bsp: Fuer exp(beta_k) = 0.95: Wenn X_k um eine Einheit zunimmt, sinkt die Realisierungschance von der Kategorie j 
# relativ zur Referenzkategorie m um den Faktor 0.95
# binaer:
#
#      (1, wenn Baum nicht geduengt
# x = <
#      (0, wenn Baum geduengt
#
# beta_k = 1.163387 => exp(beta_k) = 3.2
# Die Chance eines nicht geduengten Baumes, eher eine eine leichte Schaedigung (y) zu haben, als keine Schaedigung 
# (y-Referenz) ist gegenueber einem geduengtem Baum 3.2 mal hoeher

# gleiche Gruppengroessen
# standard <- dat[dat$DeliveryType == "STANDARD", ]
# npday <- dat[dat$DeliveryType == "NP_DAY", ]
# rows <- sample(seq_len(nrow(npday)), size = floor((3833 / 7930) * nrow(npday)))
# npday <- npday[rows, ]
# npevening <- dat[dat$DeliveryType == "NP_EVENING", ]
# rows <- sample(seq_len(nrow(npevening)), size = floor((3833 / 20715) * nrow(npevening)))
# npevening <- npevening[rows, ]

# X <- rbind(standard, npday, npevening)
# X$DeliveryType <- relevel(X$DeliveryType, ref = "STANDARD")

# g <- c(X$NewCustomerFreq, X$SelectedDeliveryWeekday, X$DiffOrderSelectedDelivery, X$OrderSum, X$VoucherType, X$VoucherValue, X$lat, X$lon, X$OTZ)

# fit <- multinom(f, data = X, Hess = TRUE)
# predicted_class <- predict(fit, test1, type = "class")
# table(predicted_class, test1$DeliveryType)
# mean(as.character(predicted_class) == as.character(test1$DeliveryType))

# model 1
fit1 <- multinom(f, data = train1, Hess = TRUE)

predicted_scores1 <- predict(fit1, test1, type = "probs")
head(predicted_scores1)
predicted_class1 <- predict(fit1, test1, type = "class")
table(predicted_class1)

table(predicted_class1, test1$DeliveryType)
fit1Accuracy <- mean((predicted_class1) == (test1$DeliveryType))
fit1Accuracy
mean((predicted_class1) != (test1$DeliveryType))

# model 2
fit2 <- multinom(f, data = train2, Hess = TRUE)

predicted_scores2 <- predict(fit2, test2, type = "probs")
head(predicted_scores2)
predicted_class2 <- predict(fit2, test2, type = "class")
table(predicted_class2)

table(predicted_class2, test2$DeliveryType)
fit2Accuracy <- mean((predicted_class2) == (test2$DeliveryType))
fit2Accuracy
mean((predicted_class2) != (test2$DeliveryType))

# model 3
fit3 <- multinom(f, data = train3, Hess = TRUE)

predicted_scores3 <- predict(fit3, test3, type = "probs")
head(predicted_scores3)
predicted_class3 <- predict(fit3, test3, type = "class")
table(predicted_class3)

table(predicted_class3, test3$DeliveryType)
fit3Accuracy <- mean((predicted_class3) == (test3$DeliveryType))
fit3Accuracy
mean((predicted_class3) != (test3$DeliveryType))

# model 4
fit4 <- multinom(f, data = train4, Hess = TRUE)

predicted_scores4 <- predict(fit4, test4, type = "probs")
head(predicted_scores4)
predicted_class4 <- predict(fit4, test4, type = "class")
table(predicted_class4)

table(predicted_class4, test4$DeliveryType)
fit4Accuracy <- mean((predicted_class4) == (test4$DeliveryType))
fit4Accuracy
mean((predicted_class4) != (test4$DeliveryType))

# model 5
fit5 <- multinom(f, data = train5, Hess = TRUE)

predicted_scores5 <- predict(fit5, test5, type = "probs")
head(predicted_scores5)
predicted_class5 <- predict(fit5, test5, type = "class")
table(predicted_class5)

table(predicted_class5, test5$DeliveryType)
fit5Accuracy <- mean((predicted_class5) == (test5$DeliveryType))
fit5Accuracy
mean((predicted_class5) != (test5$DeliveryType))

# model 6
fit6 <- multinom(f, data = train6, Hess = TRUE)

predicted_scores6 <- predict(fit6, test6, type = "probs")
head(predicted_scores6)
predicted_class6 <- predict(fit6, test6, type = "class")
table(predicted_class6)

table(predicted_class6, test6$DeliveryType)
fit6Accuracy <- mean((predicted_class6) == (test6$DeliveryType))
fit6Accuracy
mean((predicted_class6) != (test6$DeliveryType))

# model 7
fit7 <- multinom(f, data = train7, Hess = TRUE)

predicted_scores7 <- predict(fit7, test7, type = "probs")
head(predicted_scores7)
predicted_class7 <- predict(fit7, test7, type = "class")
table(predicted_class7)

table(predicted_class7, test7$DeliveryType)
fit7Accuracy <- mean((predicted_class7) == (test7$DeliveryType))
fit7Accuracy
mean((predicted_class7) != (test7$DeliveryType))

# STANDARD
8 / dim(filter(test7, DeliveryType == "STANDARD"))[1]
# NP_DAY
1331 / dim(filter(test7, DeliveryType == "NP_DAY"))[1]
# NP_EVENING
2648 / dim(filter(test7, DeliveryType == "NP_EVENING"))[1]

# model 8
fit8 <- multinom(f, data = train8, Hess = TRUE)

predicted_scores8 <- predict(fit8, test8, type = "probs")
head(predicted_scores8)
predicted_class8 <- predict(fit8, test8, type = "class")
table(predicted_class8)

table(predicted_class8, test8$DeliveryType)
fit8Accuracy <- mean((predicted_class8) == (test8$DeliveryType))
fit8Accuracy
mean((predicted_class8) != (test8$DeliveryType))

# model 9
fit9 <- multinom(f, data = train9, Hess = TRUE)

predicted_scores9 <- predict(fit9, test9, type = "probs")
head(predicted_scores9)
predicted_class9 <- predict(fit9, test9, type = "class")
table(predicted_class9)

table(predicted_class9, test9$DeliveryType)
fit9Accuracy <- mean((predicted_class9) == (test9$DeliveryType))
fit9Accuracy
mean((predicted_class9) != (test9$DeliveryType))

# model 10
fit10 <- multinom(f, data = train10, Hess = TRUE)

predicted_scores10 <- predict(fit10, test10, type = "probs")
head(predicted_scores10)
predicted_class10 <- predict(fit10, test10, type = "class")
table(predicted_class10)

table(predicted_class10, test10$DeliveryType)
fit10Accuracy <- mean((predicted_class10) == (test10$DeliveryType))
fit10Accuracy
mean((predicted_class10) != (test10$DeliveryType))

max(c(fit1Accuracy, fit2Accuracy, fit3Accuracy, fit4Accuracy, fit5Accuracy, fit6Accuracy, fit7Accuracy, fit8Accuracy, fit9Accuracy, 
      fit10Accuracy))
# => Model 7
