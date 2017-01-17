# clear workspace
rm(list = ls(all.names = TRUE))
setwd("~/Documents/GitHub/WINFO_Projekt")

# load packages
library(scales)
library(plyr)
library(dplyr)
library(dtplyr)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)

# import dataset
source("read_script.R")
dataset1 <- readOrderHeader()
dataset2 <- readOrderLine()
dataset3 <- fread("Product_Groups_verbessert.csv", header = TRUE, sep = ";", encoding = "UTF-8")
dataset4 <- readTracking()

# Merge von dataset1 und dataset4
dataset5 <- merge(dataset1, dataset4, by = "OrderNo", all = FALSE)
# Entfernen der leeren TimeSlots und mehrfacher OrderNo
dataset5 <- dataset5[dataset5$LowerBound != "NA" & !duplicated(dataset5$OrderNo)]
# Entfernen einer falschen Beobachtung (Mail von Marco)
dataset5 <-dataset5[!filter(dataset5, State == "Saarland"), ]
# Erstellen der Unteren Schranke fuer TimeSlot (minus 15 Minuten Toleranz)
dataset5$LowerBoundWT <- format(strptime(dataset5$LowerBound, "%H:%M") - 15 * 60, format = "%H:%M")
# Erstellen der Oberen Schranke fuer TimeSlot (plus 15 Minuten Toleranz)
dataset5$UpperBoundWT <- format(strptime(dataset5$UpperBound, "%H:%M") + 15 * 60, format = "%H:%M")
# Erstellen von DeliveryInTimeWT (binaer)
dataset5$DeliveryInTimeWT <- vector(mode = 'integer', length = length(dataset5$OrderNo))
dataset5$DeliveryInTimeWT[dataset5$SelectedDeliveryDate == dataset5$DeliveryDate & dataset5$DeliveryTime >= 
                            dataset5$LowerBoundWT & dataset5$DeliveryTime <= dataset5$UpperBoundWT] <- 1
# Umwandeln DeliveryInTime in factor
dataset5$DeliveryInTimeWT <- factor(dataset5$DeliveryInTimeWT) 
# Neue Levels fuer DeliveryInTime (Yes/No)
levels(dataset5$DeliveryInTimeWT) <- list("Yes" = 1, "No" = 0)
# Erstellen PostCodeArea (erste Stelle der PLZ)
dataset5$PostCodeArea <- substr(dataset5$PostCode, start = 1, stop = 1)
# Ordnen der Zeilen von dataset5 nach DeliveryInTime und PostCodeArea
dataset5 <- arrange(dataset5, DeliveryInTimeWT, PostCodeArea)
# Erstellen von DeliveryInTime (binaer)
dataset5$DeliveryInTime <- vector(mode = 'integer', length = length(dataset5$OrderNo))
dataset5$DeliveryInTime[dataset5$SelectedDeliveryDate == dataset5$DeliveryDate & dataset5$DeliveryTime >= 
                          dataset5$LowerBound & dataset5$DeliveryTime <= dataset5$UpperBound] <- 1
# Umwandeln DeliveryInTime in factor
dataset5$DeliveryInTime <- factor(dataset5$DeliveryInTime) 
# Neue Levels fuer DeliveryInTime (Yes/No)
levels(dataset5$DeliveryInTime) <- list("Yes" = 1, "No" = 0)

# Erstellung eines leeren Vektors fuer Verspaetungen (in Minuten) mit Toleranz
dataset5$DelayWT <- vector(mode = 'numeric', length = length(dataset5$OrderNo))
dataset5$DelayWT <- 0
# zu zeitige Zustellungen in Minuten (negative Werte)
dataset5$DelayWT[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBoundWT)] <- as.numeric(strptime(paste(dataset5$DeliveryDate[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBoundWT)], dataset5$DeliveryTime[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBoundWT)]), "%Y-%m-%d %H:%M:%S") - strptime(paste(dataset5$SelectedDeliveryDate[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBoundWT)], dataset5$LowerBoundWT[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBoundWT)]), "%Y-%m-%d %H:%M"), units = "mins")
# zu spaete Zustellungen in Minuten (positive Werte)
dataset5$DelayWT[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBoundWT)] <- as.numeric(strptime(paste(dataset5$DeliveryDate[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBoundWT)], dataset5$DeliveryTime[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBoundWT)]), "%Y-%m-%d %H:%M:%S") - strptime(paste(dataset5$SelectedDeliveryDate[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBoundWT)], dataset5$UpperBoundWT[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBoundWT)]), "%Y-%m-%d %H:%M"), units = "mins")

# Erstellung eines leeren Vektors fuer Verspaetungen (in Minuten) ohne Toleranz
dataset5$Delay <- vector(mode = 'numeric', length = length(dataset5$OrderNo))
dataset5$Delay <- 0
# zu zeitige Zustellungen in Minuten (negative Werte)
dataset5$Delay[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBound)] <- as.numeric(strptime(paste(dataset5$DeliveryDate[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBound)], dataset5$DeliveryTime[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBound)]), "%Y-%m-%d %H:%M:%S") - strptime(paste(dataset5$SelectedDeliveryDate[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBound)], dataset5$LowerBound[dataset5$DeliveryDate < dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime < dataset5$LowerBound)]), "%Y-%m-%d %H:%M"), units = "mins")
# zu spaete Zustellungen in Minuten (positive Werte)
dataset5$Delay[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBound)] <- as.numeric(strptime(paste(dataset5$DeliveryDate[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBound)], dataset5$DeliveryTime[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBound)]), "%Y-%m-%d %H:%M:%S") - strptime(paste(dataset5$SelectedDeliveryDate[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBound)], dataset5$UpperBound[dataset5$DeliveryDate > dataset5$SelectedDeliveryDate | (dataset5$DeliveryDate == dataset5$SelectedDeliveryDate & dataset5$DeliveryTime > dataset5$UpperBound)]), "%Y-%m-%d %H:%M"), units = "mins")

# Plot der Dichten (unpuenktliche Lieferungen)
grid.arrange(ggplot(subset(dataset5, (DelayWT < 0)), aes(x = DelayWT)) + geom_density(fill = "blue", colour = NA, alpha = .2) + geom_line(stat = "density") + ggtitle("Delivery with Tolerance\n(too early)") + labs(x = "Delay (in minutes)"), ggplot(subset(dataset5, (DelayWT > 0)), aes(x = DelayWT)) + geom_density(fill = "blue", colour = NA, alpha = .2) + geom_line(stat = "density") + ggtitle("Delivery with Tolerance\n(too late)") + labs(x = "Delay (in minutes)"),ggplot(subset(dataset5, (Delay < 0)), aes(x = Delay)) + geom_density(fill = "blue", colour = NA, alpha = .2) + geom_line(stat = "density") + ggtitle("Delivery without Tolerance\n(too early)") + labs(x = "Delay (in minutes)"), ggplot(subset(dataset5, (Delay > 0)), aes(x = Delay)) + geom_density(fill = "blue", colour = NA, alpha = .2) + geom_line(stat = "density") + ggtitle("Delivery without Tolerance\n(too late)") + labs(x = "Delay (in minutes)"), ncol = 2, nrow = 2)

# Erstellen eines kompakten Datensatz fuer Plot DeliveryInTimeWT
PostCodeArea <- factor(rep(c(0:9), 2))
DeliveryInTimeWT <- factor(c(rep("Yes", 10), rep("No", 10)))
dataset6 <- data.frame(PostCodeArea, DeliveryInTimeWT)
# Berechnen der Anteile fuer Zustellung im TimeSlot
dataset6$Percentage[dataset6$PostCodeArea == "0" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "0" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "0"]) * 100, 2)
dataset6$Percentage[dataset6$PostCodeArea == "1" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "1" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "1"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "2" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "2" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "2"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "3" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "3" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "3"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "4" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "4" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "4"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "5" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "5" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "5"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "6" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "6" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "6"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "7" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "7" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "7"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "8" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "8" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "8"]) * 100, 2) 
dataset6$Percentage[dataset6$PostCodeArea == "9" & dataset6$DeliveryInTimeWT == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "9" & as.integer(dataset5$DeliveryInTimeWT) == 1])) / length(dataset5$DeliveryInTimeWT[dataset5$PostCodeArea == "9"]) * 100, 2)
# Berechnung der Anteile fuer Zustellung ausserhalb des TimeSlot
dataset6$Percentage[dataset6$PostCodeArea == "0" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "0" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "1" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "1" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "2" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "2" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "3" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "3" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "4" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "4" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "5" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "5" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "6" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "6" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "7" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "7" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "8" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "8" & dataset6$DeliveryInTimeWT == "Yes"], 2)
dataset6$Percentage[dataset6$PostCodeArea == "9" & dataset6$DeliveryInTimeWT == "No"] <- round(100 - dataset6$Percentage[dataset6$PostCodeArea == "9" & dataset6$DeliveryInTimeWT == "Yes"], 2)
# Berechnung fuer Position der Anteilswerte im Plot
dataset6 <- ddply(dataset6, .(PostCodeArea), transform, Position = cumsum(Percentage) - (0.5 * Percentage))
# Vertauschen deR Level fuer Plot
#levels(dataset6$DeliveryInTime) <- list("Yes" = 1, "No" = 0)
levels(dataset6$DeliveryInTimeWT) <- list("No" = 0, "Yes" = 1)
# Umordnung der Level von PostCodeArea (Plot: y-Achse absteigend)
dataset6$PostCodeArea <- factor(dataset6$PostCodeArea, levels = c("9", "8", "7", "6", "5", "4", "3", "2", "1", "0"))
# Erstellen des Plot
ggplot() + geom_bar(aes(y = Percentage, x = PostCodeArea, fill = DeliveryInTimeWT), data = dataset6, stat = "identity") + geom_text(data = dataset6, aes(x = PostCodeArea, y = Position, label = paste0(Percentage, "%")), size = 4) + 
  scale_fill_manual(values = c("darkred", "darkgreen"), breaks = c("Yes", "No")) + ggtitle("DeliverySurvey (with Tolerance)") + xlab("PostCodeArea") + 
  ylab("DeliveryInTime") + theme(legend.position = "bottom", legend.direction = "horizontal", 
                                 legend.title = element_blank()) + scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + coord_flip()

# Erstellen eines kompakten Datensatz fuer Plot DeliveryInTime
PostCodeArea <- factor(rep(c(0:9), 2))
DeliveryInTime <- factor(c(rep("Yes", 10), rep("No", 10)))
dataset8 <- data.frame(PostCodeArea, DeliveryInTime)
# Berechnen der Anteile fuer Zustellung im TimeSlot
dataset8$Percentage[dataset8$PostCodeArea == "0" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "0" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "0"]) * 100, 2)
dataset8$Percentage[dataset8$PostCodeArea == "1" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "1" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "1"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "2" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "2" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "2"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "3" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "3" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "3"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "4" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "4" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "4"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "5" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "5" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "5"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "6" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "6" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "6"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "7" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "7" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "7"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "8" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "8" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "8"]) * 100, 2) 
dataset8$Percentage[dataset8$PostCodeArea == "9" & dataset8$DeliveryInTime == "Yes"] <- round(sum(as.integer(dataset5$DeliveryInTime[dataset5$PostCodeArea == "9" & as.integer(dataset5$DeliveryInTime) == 1])) / length(dataset5$DeliveryInTime[dataset5$PostCodeArea == "9"]) * 100, 2)
# Berechnung der Anteile fuer Zustellung ausserhalb des TimeSlot
dataset8$Percentage[dataset8$PostCodeArea == "0" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "0" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "1" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "1" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "2" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "2" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "3" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "3" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "4" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "4" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "5" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "5" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "6" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "6" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "7" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "7" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "8" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "8" & dataset8$DeliveryInTime == "Yes"], 2)
dataset8$Percentage[dataset8$PostCodeArea == "9" & dataset8$DeliveryInTime == "No"] <- round(100 - dataset8$Percentage[dataset8$PostCodeArea == "9" & dataset8$DeliveryInTime == "Yes"], 2)
# Berechnung fuer Position der Anteilswerte im Plot
dataset8 <- ddply(dataset8, .(PostCodeArea), transform, Position = cumsum(Percentage) - (0.5 * Percentage))
# Vertauschen deR Level fuer Plot
#levels(dataset8$DeliveryInTime) <- list("Yes" = 1, "No" = 0)
levels(dataset8$DeliveryInTime) <- list("No" = 0, "Yes" = 1)
# Umordnung der Level von PostCodeArea (Plot: y-Achse absteigend)
dataset8$PostCodeArea <- factor(dataset8$PostCodeArea, levels = c("9", "8", "7", "6", "5", "4", "3", "2", "1", "0"))
# Erstellen des Plot
ggplot() + geom_bar(aes(y = Percentage, x = PostCodeArea, fill = DeliveryInTime), data = dataset8, stat = "identity") + geom_text(data = dataset8, aes(x = PostCodeArea, y = Position, label = paste0(Percentage, "%")), size = 4) + 
  scale_fill_manual(values = c("darkred", "darkgreen"), breaks = c("Yes", "No")) + ggtitle("DeliverySurvey (without Tolerance)") + xlab("PostCodeArea") + 
  ylab("DeliveryInTime") + theme(legend.position = "bottom", legend.direction = "horizontal", 
                                 legend.title = element_blank()) + scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + coord_flip()

# Erstellen eines kompakten Datensatz fuer Plot DeliveryInTime
State <- factor(rep(c("Baden-Wurttemberg", "Bayern", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thueringen"), 2))
DeliveryInTimeWT <- factor(c(rep("Yes", 16), rep("No", 16)))
dataset7 <- data.frame(State, DeliveryInTimeWT)
# Berechnen der Anteile fuer Zustellung im TimeSlot
dataset7$Percentage[dataset7$State == District & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == District & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == District))[1] * 100, 2) 
dataset7$Percentage[dataset7$State == "Bayern" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Bayern" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Bayern"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Berlin" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Berlin" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Berlin"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Brandenburg" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Brandenburg" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Brandenburg"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Bremen" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Bremen" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Bremen"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Hamburg" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Hamburg" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Hamburg"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Hessen" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Hessen" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Hessen"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Mecklenburg-Vorpommern" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Mecklenburg-Vorpommern" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Mecklemburg-Vorpommern"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Niedersachsen" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Niedersachsen" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Niedersachsen"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Nordrhein-Westfalen" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Nordrhein-Westfalen" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Nordrhein-Westfalen"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Rheinland-Pfalz" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Rheinland-Pfalz" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Rheinland-Pfalz"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Saarland" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Saarland" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Saarland"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Sachsen" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Sachsen" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Sachsen"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Sachsen-Anhalt" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Sachsen-Anhalt" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Sachsen-Anhalt"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Schleswig-Holstein" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Schleswig-Holstein" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Schleswig-Holstein"))[1] * 100, 2)
dataset7$Percentage[dataset7$State == "Thueringen" & dataset7$DeliveryInTimeWT == "Yes"] <- round(dim(filter(dataset5, State == "Thueringen" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(dataset5, State == "Thueringen"))[1] * 100, 2)
# Berechnung der Anteile fuer Zustellung ausserhalb des TimeSlot
dataset7$Percentage[dataset7$State == District & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == District & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Bayern" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Bayern" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Berlin" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Berlin" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Brandenburg" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Brandenburg" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Bremen" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Bremen" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Hamburg" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Hamburg" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Hessen" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Hessen" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Mecklenburg-Vorpommern" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Mecklenburg-Vorpommern" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Niedersachsen" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Niedersachsen" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Nordrhein-Westfalen" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Nordrhein-Westfalen" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Rheinland-Pfalz" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Rheinland-Pfalz" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Saarland" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Saarland" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Sachsen" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Sachsen" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Sachsen-Anhalt" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Sachsen-Anhalt" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Schleswig-Holstein" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Schleswig-Holstein" & dataset7$DeliveryInTimeWT == "Yes"], 2)
dataset7$Percentage[dataset7$State == "Thueringen" & dataset7$DeliveryInTimeWT == "No"] <- round(100 - dataset7$Percentage[dataset7$State == "Thueringen" & dataset7$DeliveryInTimeWT == "Yes"], 2)
# Berechnung fuer Position der Anteilswerte im Plot
dataset7 <- ddply(dataset7, .(State), transform, Position = cumsum(Percentage) - (0.5 * Percentage))
# Vertauschen deR Level fuer Plot
#levels(dataset6$DeliveryInTime) <- list("Yes" = 1, "No" = 0)
levels(dataset6$DeliveryInTimeWT) <- list("No" = 0, "Yes" = 1)
# Umordnung der Level von PostCodeArea (Plot: y-Achse absteigend)
dataset7$State <- factor(dataset7$State, levels = c("Thueringen", "Schleswig-Holstein", "Sachsen-Anhalt", "Sachsen", "Saarland", "Rheinland-Pfalz", "Nordrhein-Westfalen", "Niedersachsen", "Mecklenburg-Vorpommern", "Hessen", "Hamburg", "Bremen", "Brandenburg", "Berlin", "Bayern", District))
# Erstellen des Plot
ggplot() + geom_bar(aes(y = Percentage, x = State, fill = DeliveryInTimeWT), data = dataset7, stat = "identity") + geom_text(data = dataset7, aes(x = State, y = Position, label = paste0(Percentage, "%")), size = 4) + 
  scale_fill_manual(values = c("darkred", "darkgreen"), breaks = c("Yes", "No")) + ggtitle("DeliverySurvey (with Tolerance)") + xlab("State") + 
  ylab("DeliveryInTime") + theme(legend.position = "bottom", legend.direction = "horizontal", 
                                 legend.title = element_blank()) + scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + coord_flip()

# Erstellen eines kompakten Datensatz fuer Plot DeliveryInTime
State <- factor(rep(c(District, "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thueringen"), 2))
DeliveryInTime <- factor(c(rep("Yes", 16), rep("No", 16)))
dataset9 <- data.frame(State, DeliveryInTime)
# Berechnen der Anteile fuer Zustellung im TimeSlot
dataset9$Percentage[dataset9$State == District & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == District & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == District))[1] * 100, 2) 
dataset9$Percentage[dataset9$State == "Bayern" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Bayern" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Bayern"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Berlin" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Berlin" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Berlin"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Brandenburg" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Brandenburg" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Brandenburg"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Bremen" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Bremen" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Bremen"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Hamburg" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Hamburg" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Hamburg"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Hessen" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Hessen" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Hessen"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Mecklenburg-Vorpommern" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Mecklenburg-Vorpommern" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Mecklemburg-Vorpommern"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Niedersachsen" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Niedersachsen" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Niedersachsen"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Nordrhein-Westfalen" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Nordrhein-Westfalen" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Nordrhein-Westfalen"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Rheinland-Pfalz" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Rheinland-Pfalz" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Rheinland-Pfalz"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Saarland" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Saarland" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Saarland"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Sachsen" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Sachsen" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Sachsen"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Sachsen-Anhalt" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Sachsen-Anhalt" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Sachsen-Anhalt"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Schleswig-Holstein" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Schleswig-Holstein" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Schleswig-Holstein"))[1] * 100, 2)
dataset9$Percentage[dataset9$State == "Thueringen" & dataset9$DeliveryInTime == "Yes"] <- round(dim(filter(dataset5, State == "Thueringen" & DeliveryInTime == "Yes"))[1] / dim(filter(dataset5, State == "Thueringen"))[1] * 100, 2)
# Berechnung der Anteile fuer Zustellung ausserhalb des TimeSlot
dataset9$Percentage[dataset9$State == District & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == District & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Bayern" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Bayern" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Berlin" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Berlin" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Brandenburg" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Brandenburg" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Bremen" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Bremen" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Hamburg" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Hamburg" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Hessen" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Hessen" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Mecklenburg-Vorpommern" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Mecklenburg-Vorpommern" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Niedersachsen" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Niedersachsen" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Nordrhein-Westfalen" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Nordrhein-Westfalen" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Rheinland-Pfalz" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Rheinland-Pfalz" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Saarland" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Saarland" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Sachsen" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Sachsen" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Sachsen-Anhalt" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Sachsen-Anhalt" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Schleswig-Holstein" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Schleswig-Holstein" & dataset9$DeliveryInTime == "Yes"], 2)
dataset9$Percentage[dataset9$State == "Thueringen" & dataset9$DeliveryInTime == "No"] <- round(100 - dataset9$Percentage[dataset9$State == "Thueringen" & dataset9$DeliveryInTime == "Yes"], 2)
# Berechnung fuer Position der Anteilswerte im Plot
dataset9 <- ddply(dataset9, .(State), transform, Position = cumsum(Percentage) - (0.5 * Percentage))
# Vertauschen deR Level fuer Plot
#levels(dataset6$DeliveryInTime) <- list("Yes" = 1, "No" = 0)
levels(dataset7$DeliveryInTime) <- list("No" = 0, "Yes" = 1)
# Umordnung der Level von PostCodeArea (Plot: y-Achse absteigend)
dataset9$State <- factor(dataset9$State, levels = c("Thueringen", "Schleswig-Holstein", "Sachsen-Anhalt", "Sachsen", "Saarland", "Rheinland-Pfalz", "Nordrhein-Westfalen", "Niedersachsen", "Mecklenburg-Vorpommern", "Hessen", "Hamburg", "Bremen", "Brandenburg", "Berlin", "Bayern", District))

grid.arrange(ggplot() + geom_bar(aes(y = Percentage, x = State, fill = DeliveryInTimeWT), data = dataset7, stat = "identity") + geom_text(data = dataset7, aes(x = State, y = Position, label = paste0(Percentage, "%")), size = 4) + 
               scale_fill_manual(values = c("darkred", "darkgreen"), breaks = c("Yes", "No")) + ggtitle("DeliverySurvey (with Tolerance)") + xlab("State") + 
               ylab("DeliveryInTime") + theme(legend.position = "bottom", legend.direction = "horizontal", 
                                              legend.title = element_blank()) + scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + coord_flip(), ggplot() + geom_bar(aes(y = Percentage, x = State, fill = DeliveryInTime), data = dataset9, stat = "identity") + geom_text(data = dataset9, aes(x = State, y = Position, label = paste0(Percentage, "%")), size = 4) + 
               scale_fill_manual(values = c("darkred", "darkgreen"), breaks = c("Yes", "No")) + ggtitle("DeliverySurvey (without Tolerance)") + xlab("State") + 
               ylab("DeliveryInTime") + theme(legend.position = "bottom", legend.direction = "horizontal", 
                                              legend.title = element_blank()) + scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + coord_flip(), ncol = 2, nrow = 1)


#####################
berlin <- filter(dataset5, State == "Berlin")
berlin$District[berlin$PostCode == "10115" | berlin$PostCode == "10117"	| berlin$PostCode == "10119" |
                  berlin$PostCode == "10178" | berlin$PostCode == "10179" | berlin$PostCode == "10435" | 
                  berlin$PostCode == "10551" | berlin$PostCode == "10553" | berlin$PostCode == "10555" |
                  berlin$PostCode == "10557" | berlin$PostCode == "10559" | berlin$PostCode == "10623" | 
                  berlin$PostCode == "10785" | berlin$PostCode == "10787" | berlin$PostCode == "10963" | 
                  berlin$PostCode == "10969" | berlin$PostCode == "13347" | berlin$PostCode == "13349" | 
                  berlin$PostCode == "13351" | berlin$PostCode == "13353" | berlin$PostCode == "13355" | 
                  berlin$PostCode == "13357" | berlin$PostCode == "13359" | berlin$PostCode == "13405" | 
                  berlin$PostCode == "13407" | berlin$PostCode == "13409"] <- "Mitte" 	

berlin$District[berlin$PostCode == "10179" | berlin$PostCode ==	"10243" | berlin$PostCode == "10245" |
                  berlin$PostCode == "10247" | berlin$PostCode == "10249" | berlin$PostCode == "10367" | 
                  berlin$PostCode == "10785" | berlin$PostCode == "10961" | berlin$PostCode == "10963" | 
                  berlin$PostCode == "10965" | berlin$PostCode == "10967" | berlin$PostCode == "10969" | 
                  berlin$PostCode == "10997" | berlin$PostCode == "10999" | berlin$PostCode == "12045" | 
                  berlin$PostCode == "10178"] <- "Friedrichshain-Kreuzberg"

berlin$District[berlin$PostCode == "10119" | berlin$PostCode == "10247" | berlin$PostCode == "10249" | 
                  berlin$PostCode == "10405" | berlin$PostCode == "10407" | berlin$PostCode == "10409" | 
                  berlin$PostCode == "10435" | berlin$PostCode == "10437" | berlin$PostCode == "10439" | 
                  berlin$PostCode == "13051" | berlin$PostCode == "13053" | berlin$PostCode == "13086" | 
                  berlin$PostCode == "13088" | berlin$PostCode == "13089" | berlin$PostCode == "13125" | 
                  berlin$PostCode == "13127" | berlin$PostCode == "13129" | berlin$PostCode == "13156" | 
                  berlin$PostCode == "13158" | berlin$PostCode == "13159" | berlin$PostCode == "13187" | 
                  berlin$PostCode == "13189"] <- "Pankow"

berlin$District[berlin$PostCode == "10553" | berlin$PostCode == "10585" | berlin$PostCode == "10587" | 
                  berlin$PostCode == "10589" | berlin$PostCode == "10623" | berlin$PostCode == "10625" | 
                  berlin$PostCode == "10627" | berlin$PostCode == "10629" | berlin$PostCode == "10707" | 
                  berlin$PostCode == "10709" | berlin$PostCode == "10711" | berlin$PostCode == "10713" | 
                  berlin$PostCode == "10715" | berlin$PostCode == "10717" | berlin$PostCode == "10719" | 
                  berlin$PostCode == "10777" | berlin$PostCode == "10779" | berlin$PostCode == "10787" | 
                  berlin$PostCode == "10789" | berlin$PostCode == "10825" | berlin$PostCode == "13353" | 
                  berlin$PostCode == "13597" | berlin$PostCode == "13627" | berlin$PostCode == "13629" | 
                  berlin$PostCode == "14050" | berlin$PostCode == "14052" | berlin$PostCode == "14053" | 
                  berlin$PostCode == "14055" | berlin$PostCode == "14057" | berlin$PostCode == "14059" | 
                  berlin$PostCode == "14193" | berlin$PostCode == "14195" | berlin$PostCode == "14197" | 
                  berlin$PostCode == "14199"] <- "Charlottenburg-Wilmersdorf"

berlin$District[berlin$PostCode == "13581" | berlin$PostCode == "13583" | berlin$PostCode == "13585" | 
                  berlin$PostCode == "13587" | berlin$PostCode == "13589" | berlin$PostCode == "13591" | 
                  berlin$PostCode == "13593" | berlin$PostCode == "13595" | berlin$PostCode == "13597" | 
                  berlin$PostCode == "13599" | berlin$PostCode == "13627" | berlin$PostCode == "13629" | 
                  berlin$PostCode == "14052" | berlin$PostCode == "14089"] <- "Spandau"

berlin$District[berlin$PostCode == "12157" | berlin$PostCode == "12161" | berlin$PostCode == "12163" | 
                  berlin$PostCode == "12165" | berlin$PostCode == "12167" | berlin$PostCode == "12169" | 
                  berlin$PostCode == "12203" | berlin$PostCode == "12205" | berlin$PostCode == "12207" | 
                  berlin$PostCode == "12209" | berlin$PostCode == "12247" | berlin$PostCode == "12249" | 
                  berlin$PostCode == "12277" | berlin$PostCode == "12279" | berlin$PostCode == "14109" | 
                  berlin$PostCode == "14129" | berlin$PostCode == "14163" | berlin$PostCode == "14165" | 
                  berlin$PostCode == "14167" | berlin$PostCode == "14169" | berlin$PostCode == "14193" | 
                  berlin$PostCode == "14195" | berlin$PostCode == "14197" | berlin$PostCode == "14199"] <- "Steglitz-Zehlendorf"

berlin$District[berlin$PostCode == "10777" | berlin$PostCode == "10779" | berlin$PostCode == "10781" | 
                  berlin$PostCode == "10783" | berlin$PostCode == "10785" | berlin$PostCode == "10787" | 
                  berlin$PostCode == "10789" | berlin$PostCode == "10823" | berlin$PostCode == "10825" | 
                  berlin$PostCode == "10827" | berlin$PostCode == "10829" | berlin$PostCode == "10965" | 
                  berlin$PostCode == "12099" | berlin$PostCode == "12101" | berlin$PostCode == "12103" | 
                  berlin$PostCode == "12105" | berlin$PostCode == "12107" | berlin$PostCode == "12109" | 
                  berlin$PostCode == "12157" | berlin$PostCode == "12159" | berlin$PostCode == "12161" | 
                  berlin$PostCode == "12163" | berlin$PostCode == "12169" | berlin$PostCode == "12249" | 
                  berlin$PostCode == "12277" | berlin$PostCode == "12279" | berlin$PostCode == "12305" | 
                  berlin$PostCode == "12307" | berlin$PostCode == "12309" | berlin$PostCode == "12347" | 
                  berlin$PostCode == "14197"] <- "Tempelhof-Schoeneberg"

berlin$District[berlin$PostCode == "10965" | berlin$PostCode == "10967" | berlin$PostCode == "12043" | 
                  berlin$PostCode == "12045" | berlin$PostCode == "12047" | berlin$PostCode == "12049" | 
                  berlin$PostCode == "12051" | berlin$PostCode == "12053" | berlin$PostCode == "12055" | 
                  berlin$PostCode == "12057" | berlin$PostCode == "12059" | berlin$PostCode == "12099" | 
                  berlin$PostCode == "12107" | berlin$PostCode == "12305" | berlin$PostCode == "12347" | 
                  berlin$PostCode == "12349" | berlin$PostCode == "12351" | berlin$PostCode == "12353" | 
                  berlin$PostCode == "12355" | berlin$PostCode == "12357" | berlin$PostCode == "12359"] <- "Neukoelln"

berlin$District[berlin$PostCode == "12435" | berlin$PostCode == "12437" | berlin$PostCode == "12439" | 
                  berlin$PostCode == "12459" | berlin$PostCode == "12487" | berlin$PostCode == "12489" | 
                  berlin$PostCode == "12524" | berlin$PostCode == "12526" | berlin$PostCode == "12527" | 
                  berlin$PostCode == "12555" | berlin$PostCode == "12557" | berlin$PostCode == "12559" | 
                  berlin$PostCode == "12587" | berlin$PostCode == "12589" | berlin$PostCode == "12623"] <- "Treptow-Koepenick"

berlin$District[berlin$PostCode == "12555" | berlin$PostCode == "12619" | berlin$PostCode == "12621" | 
                  berlin$PostCode == "12623" | berlin$PostCode == "12627" | berlin$PostCode == "12629" | 
                  berlin$PostCode == "12679" | berlin$PostCode == "12681" | berlin$PostCode == "12683" | 
                  berlin$PostCode == "12685" | berlin$PostCode == "12687" | berlin$PostCode == "12689"] <- "Marzahn-Hellersdorf"

berlin$District[berlin$PostCode == "10315" | berlin$PostCode == "10317" | berlin$PostCode == "10318" | 
                  berlin$PostCode == "10319" | berlin$PostCode == "10365" | berlin$PostCode == "10367" | 
                  berlin$PostCode == "10369" | berlin$PostCode == "13051" | berlin$PostCode == "13053" | 
                  berlin$PostCode == "13055" | berlin$PostCode == "13057" | berlin$PostCode == "13059"] <- "Lichtenberg"

berlin$District[berlin$PostCode == "13403" | berlin$PostCode == "13405" | berlin$PostCode == "13407" | 
                  berlin$PostCode == "13409" | berlin$PostCode == "13435" | berlin$PostCode == "13437" | 
                  berlin$PostCode == "13439" | berlin$PostCode == "13465" | berlin$PostCode == "13467" | 
                  berlin$PostCode == "13469" | berlin$PostCode == "13503" | berlin$PostCode == "13505" | 
                  berlin$PostCode == "13507" | berlin$PostCode == "13509" | berlin$PostCode == "13599" | 
                  berlin$PostCode == "13629"] <- "Reinickendorf"

table(berlin$District)

# Charlottenburg-Wilmersdorf
table(select(filter(berlin, District == "Charlottenburg-Wilmersdorf"), DeliveryInTimeWT))
table(select(filter(berlin, District == "Charlottenburg-Wilmersdorf"), DeliveryInTime))
dim(filter(berlin, District == "Charlottenburg-Wilmersdorf" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(berlin, District == "Charlottenburg-Wilmersdorf"))[1]
dim(filter(berlin, District == "Charlottenburg-Wilmersdorf" & DeliveryInTime == "Yes"))[1] / dim(filter(berlin, District == "Charlottenburg-Wilmersdorf"))[1]
dim(filter(berlin, District == "Mitte" & DeliveryInTimeWT == "Yes"))[1] / dim(filter(berlin, District == "Mitte"))[1]
dim(filter(berlin, District == "Mitte" & DeliveryInTime == "Yes"))[1] / dim(filter(berlin, District == "Mitte"))[1]



dat01 <- filter(berlin, DeliveryDate >= "2014-07-01" & DeliveryDate <= "2014-07-31")
dat02 <- filter(berlin, DeliveryDate >= "2014-08-01" & DeliveryDate <= "2014-08-31")
dat03 <- filter(berlin, DeliveryDate >= "2014-09-01" & DeliveryDate <= "2014-09-30")
dat04 <- filter(berlin, DeliveryDate >= "2014-10-01" & DeliveryDate <= "2014-10-31")
dat05 <- filter(berlin, DeliveryDate >= "2014-11-01" & DeliveryDate <= "2014-11-30")
dat06 <- filter(berlin, DeliveryDate >= "2014-12-01" & DeliveryDate <= "2014-12-31")
dat07 <- filter(berlin, DeliveryDate >= "2015-01-01" & DeliveryDate <= "2015-01-31")
dat08 <- filter(berlin, DeliveryDate >= "2015-02-01" & DeliveryDate <= "2015-02-28")
dat09 <- filter(berlin, DeliveryDate >= "2015-03-01" & DeliveryDate <= "2015-03-31")
dat10 <- filter(berlin, DeliveryDate >= "2015-04-01" & DeliveryDate <= "2015-04-30")
dat11 <- filter(berlin, DeliveryDate >= "2015-05-01" & DeliveryDate <= "2015-05-31")
dat12 <- filter(berlin, DeliveryDate >= "2015-06-01" & DeliveryDate <= "2015-06-30")
dat13 <- filter(berlin, DeliveryDate >= "2015-07-01" & DeliveryDate <= "2015-07-31")
dat14 <- filter(berlin, DeliveryDate >= "2015-08-01" & DeliveryDate <= "2015-08-31")
dat15 <- filter(berlin, DeliveryDate >= "2015-09-01" & DeliveryDate <= "2015-09-30")
dat16 <- filter(berlin, DeliveryDate >= "2015-10-01" & DeliveryDate <= "2015-10-31")
dat17 <- filter(berlin, DeliveryDate >= "2015-11-01" & DeliveryDate <= "2015-11-30")
dat18 <- filter(berlin, DeliveryDate >= "2015-12-01" & DeliveryDate <= "2015-12-31")
dat19 <- filter(berlin, DeliveryDate >= "2016-01-01" & DeliveryDate <= "2016-01-31")
dat20 <- filter(berlin, DeliveryDate >= "2016-02-01" & DeliveryDate <= "2016-02-29")
dat21 <- filter(berlin, DeliveryDate >= "2016-03-01" & DeliveryDate <= "2016-03-31")
dat22 <- filter(berlin, DeliveryDate >= "2016-04-01" & DeliveryDate <= "2016-04-30")
dat23 <- filter(berlin, DeliveryDate >= "2016-05-01" & DeliveryDate <= "2016-05-31")
dat24 <- filter(berlin, DeliveryDate >= "2016-06-01" & DeliveryDate <= "2016-06-30")

# Berlin Mitte
District <- rep("Mitte", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_mitte <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat01, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat01, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat02, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat02, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat03, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat03, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat04, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat04, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat05, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat05, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat06, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat06, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat07, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat07, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat08, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat08, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat09, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat09, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat10, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat10, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat11, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat11, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat12, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat12, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat13, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat13, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat14, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat14, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat15, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat15, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat16, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat16, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat17, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat17, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat18, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat18, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat19, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat19, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat20, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat20, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat21, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat21, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat22, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat22, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat23, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat23, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat24, District == "Mitte"))[1]
berlin_mitte$DeliveryInTime[berlin_mitte$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat24, District == "Mitte"))[1]

plot01 <- ggplot(data = berlin_mitte, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Mitte")

# Berlin Friedrichshain-Kreuzberg
District <- rep("Friedrichshain-Kreuzberg", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)
DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_friedrichshain_kreuzberg <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot))

berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat01, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat01, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat02, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat02, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat03, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat03, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat04, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat04, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat05, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat05, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat06, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat06, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat07, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat07, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat08, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat08, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat09, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat09, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat10, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat10, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat11, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat11, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat12, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat12, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat13, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat13, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat14, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat14, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat15, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat15, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat16, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat16, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat17, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat17, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat18, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat18, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat19, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat19, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat20, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat20, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat21, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat21, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat22, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat22, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat23, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat23, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat24, District == "Friedrichshain-Kreuzberg"))[1]
berlin_friedrichshain_kreuzberg$DeliveryInTime[berlin_friedrichshain_kreuzberg$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Friedrichshain-Kreuzberg"))[1] / dim(filter(dat24, District == "Friedrichshain-Kreuzberg"))[1]

plot02 <- ggplot(data = berlin_friedrichshain_kreuzberg, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Friedrichshain-Kreuzberg")

# Berlin Pankow
District <- rep("Pankow", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_pankow <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat01, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat01, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat02, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat02, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat03, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat03, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat04, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat04, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat05, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat05, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat06, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat06, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat07, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat07, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat08, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat08, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat09, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat09, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat10, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat10, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat11, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat11, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat12, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat12, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat13, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat13, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat14, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat14, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat15, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat15, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat16, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat16, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat17, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat17, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat18, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat18, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat19, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat19, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat20, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat20, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat21, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat21, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat22, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat22, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat23, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat23, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Pankow"))[1] / dim(filter(dat24, District == "Pankow"))[1]
berlin_pankow$DeliveryInTime[berlin_pankow$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Pankow"))[1] / dim(filter(dat24, District == "Pankow"))[1]

plot03 <- ggplot(data = berlin_pankow, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Pankow")

# Berlin Charlottenburg-Wilmersdorf
District <- rep("Charlottenburg-Wilmersdorf", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_charlottenburg_wilmersdorf <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat01, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat01, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat02, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat02, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat03, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat03, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat04, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat04, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat05, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat05, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat06, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat06, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat07, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat07, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat08, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat08, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat09, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat09, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat10, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat10, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat11, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat11, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat12, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat12, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat13, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat13, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat14, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat14, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat15, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat15, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat16, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat16, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat17, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat17, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat18, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat18, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat19, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat19, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat20, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat20, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat21, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat21, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat22, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat22, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat23, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat23, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat24, District == "Charlottenburg-Wilmersdorf"))[1]
berlin_charlottenburg_wilmersdorf$DeliveryInTime[berlin_charlottenburg_wilmersdorf$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Charlottenburg-Wilmersdorf"))[1] / dim(filter(dat24, District == "Charlottenburg-Wilmersdorf"))[1]

plot04 <- ggplot(data = berlin_charlottenburg_wilmersdorf, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Charlottenburg-Wilmersdorf")

# Berlin Spandau
District <- rep("Spandau", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_spandau <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat01, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat01, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat02, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat02, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat03, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat03, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat04, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat04, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat05, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat05, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat06, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat06, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat07, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat07, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat08, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat08, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat09, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat09, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat10, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat10, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat11, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat11, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat12, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat12, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat13, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat13, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat14, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat14, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat15, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat15, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat16, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat16, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat17, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat17, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat18, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat18, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat19, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat19, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat20, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat20, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat21, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat21, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat22, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat22, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat23, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat23, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Spandau"))[1] / dim(filter(dat24, District == "Spandau"))[1]
berlin_spandau$DeliveryInTime[berlin_spandau$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Spandau"))[1] / dim(filter(dat24, District == "Spandau"))[1]

plot05 <- ggplot(data = berlin_spandau, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Spandau")

# Berlin Steglitz Zehlendorf
District <- rep("Steglitz-Zehlendorf", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_steglitz_zehlendorf <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat01, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat01, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat02, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat02, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat03, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat03, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat04, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat04, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat05, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat05, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat06, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat06, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat07, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat07, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat08, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat08, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat09, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat09, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat10, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat10, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat11, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat11, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat12, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat12, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat13, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat13, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat14, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat14, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat15, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat15, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat16, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat16, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat17, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat17, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat18, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat18, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat19, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat19, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat20, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat20, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat21, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat21, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat22, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat22, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat23, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat23, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat24, District == "Steglitz-Zehlendorf"))[1]
berlin_steglitz_zehlendorf$DeliveryInTime[berlin_steglitz_zehlendorf$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Steglitz-Zehlendorf"))[1] / dim(filter(dat24, District == "Steglitz-Zehlendorf"))[1]

plot06 <- ggplot(data = berlin_steglitz_zehlendorf, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Steglitz-Zehlendorf")

# Berlin Tempelhof-Schoenberg
District <- rep("Tempelhof-Schoeneberg", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_tempelhof_schoeneberg <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat01, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat01, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat02, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat02, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat03, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat03, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat04, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat04, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat05, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat05, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat06, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat06, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat07, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat07, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat08, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat08, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat09, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat09, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat10, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat10, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat11, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat11, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat12, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat12, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat13, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat13, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat14, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat14, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat15, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat15, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat16, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat16, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat17, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat17, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat18, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat18, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat19, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat19, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat20, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat20, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat21, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat21, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat22, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat22, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat23, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat23, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat24, District == "Tempelhof-Schoeneberg"))[1]
berlin_tempelhof_schoeneberg$DeliveryInTime[berlin_tempelhof_schoeneberg$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Tempelhof-Schoeneberg"))[1] / dim(filter(dat24, District == "Tempelhof-Schoeneberg"))[1]

plot07 <- ggplot(data = berlin_tempelhof_schoeneberg, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Tempelhof-Schoenberg")

# Berlin Neukoelln
District <- rep("Neukoelln", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_neukoelln <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat01, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat01, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat02, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat02, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat03, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat03, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat04, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat04, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat05, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat05, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat06, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat06, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat07, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat07, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat08, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat08, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat09, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat09, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat10, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat10, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat11, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat11, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat12, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat12, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat13, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat13, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat14, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat14, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat15, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat15, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat16, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat16, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat17, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat17, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat18, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat18, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat19, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat19, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat20, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat20, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat21, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat21, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat22, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat22, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat23, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat23, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat24, District == "Neukoelln"))[1]
berlin_neukoelln$DeliveryInTime[berlin_neukoelln$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Neukoelln"))[1] / dim(filter(dat24, District == "Neukoelln"))[1]

plot08 <- ggplot(data = berlin_neukoelln, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Neukoelln")

# Berlin Treptow-Koepenick
District <- rep("Treptow-Koepenick", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_treptow_koepenick <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat01, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat01, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat02, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat02, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat03, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat03, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat04, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat04, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat05, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat05, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat06, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat06, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat07, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat07, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat08, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat08, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat09, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat09, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat10, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat10, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat11, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat11, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat12, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat12, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat13, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat13, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat14, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat14, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat15, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat15, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat16, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat16, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat17, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat17, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat18, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat18, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat19, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat19, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat20, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat20, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat21, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat21, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat22, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat22, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat23, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat23, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat24, District == "Treptow-Koepenick"))[1]
berlin_treptow_koepenick$DeliveryInTime[berlin_treptow_koepenick$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Treptow-Koepenick"))[1] / dim(filter(dat24, District == "Treptow-Koepenick"))[1]

plot09 <- ggplot(data = berlin_treptow_koepenick, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Treptow-Koepenick")

# Berlin Marzahn-Hellersdorf
District <- rep("Marzahn-Hellersdorf", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_marzahn_hellersdorf <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat01, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat01, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat02, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat02, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat03, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat03, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat04, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat04, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat05, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat05, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat06, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat06, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat07, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat07, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat08, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat08, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat09, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat09, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat10, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat10, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat11, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat11, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat12, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat12, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat13, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat13, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat14, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat14, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat15, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat15, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat16, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat16, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat17, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat17, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat18, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat18, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat19, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat19, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat20, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat20, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat21, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat21, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat22, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat22, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat23, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat23, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat24, District == "Marzahn-Hellersdorf"))[1]
berlin_marzahn_hellersdorf$DeliveryInTime[berlin_marzahn_hellersdorf$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Marzahn-Hellersdorf"))[1] / dim(filter(dat24, District == "Marzahn-Hellersdorf"))[1]

plot10 <- ggplot(data = berlin_marzahn_hellersdorf, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Marzahn-Hellersdorf")

# Berlin Lichtenberg
District <- rep("Lichtenberg", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_lichtenberg <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat01, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat01, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat02, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat02, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat03, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat03, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat04, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat04, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat05, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat05, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat06, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat06, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat07, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat07, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat08, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat08, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat09, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat09, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat10, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat10, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat11, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat11, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat12, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat12, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat13, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat13, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat14, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat14, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat15, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat15, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat16, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat16, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat17, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat17, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat18, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat18, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat19, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat19, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat20, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat20, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat21, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat21, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat22, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat22, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat23, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat23, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat24, District == "Lichtenberg"))[1]
berlin_lichtenberg$DeliveryInTime[berlin_lichtenberg$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Lichtenberg"))[1] / dim(filter(dat24, District == "Lichtenberg"))[1]

plot11 <- ggplot(data = berlin_lichtenberg, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Lichtenberg")

# Berlin Reinickendorf
District <- rep("Reinickendorf", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
berlin_reinickendorf <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat01, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat01, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat02, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat02, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat03, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat03, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat04, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat04, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat05, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat05, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat06, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat06, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat07, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat07, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat08, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat08, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat09, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat09, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat10, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat10, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat11, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat11, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat12, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat12, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat13, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat13, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat14, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat14, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat15, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat15, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat16, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat16, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat17, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat17, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat18, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat18, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat19, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat19, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat20, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat20, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat21, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat21, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat22, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat22, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat23, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat23, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat24, District == "Reinickendorf"))[1]
berlin_reinickendorf$DeliveryInTime[berlin_reinickendorf$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Reinickendorf"))[1] / dim(filter(dat24, District == "Reinickendorf"))[1]

plot12 <- ggplot(data = berlin_reinickendorf, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Berlin Reinickendorf")

grid.arrange(plot01, plot02, plot03, plot04, plot05, plot06, plot07, plot08, plot09, plot10, plot11, plot12, ncol = 2, nrow = 6)








muenchen <- filter(dataset5, District == "Bayern")
muenchen$District[muenchen$PostCode == "80995" | muenchen$PostCode == "80997" | muenchen$PostCode == "80999" | 
                    muenchen$PostCode == "81247" | muenchen$PostCode == "81249"] <- "Allach-Untermenzing"

muenchen$District[muenchen$PostCode == "80331" | muenchen$PostCode == "80333" | muenchen$PostCode == "80335" | 
                    muenchen$PostCode == "80336" | muenchen$PostCode == "80469" | muenchen$PostCode == "80538" | 
                    muenchen$PostCode == "80539"] <- "Altstadt-Lehel"

muenchen$District[muenchen$PostCode == "81541" | muenchen$PostCode == "81543" | muenchen$PostCode == "81667" | 
                    muenchen$PostCode == "81669" | muenchen$PostCode == "81671" | muenchen$PostCode == "81675" | 
                    muenchen$PostCode == "81677"] <- "Au-Haidhausen"

muenchen$District[muenchen$PostCode == "81243" | muenchen$PostCode == "81245" | muenchen$PostCode == "81249"] <- "Aubing-Lochhausen-Langwied"

muenchen$District[muenchen$PostCode == "81671" | muenchen$PostCode == "81673" | muenchen$PostCode == "81735" | 
                    muenchen$PostCode == "81825"] <- "Berg am Laim"

muenchen$District[muenchen$PostCode == "81675" | muenchen$PostCode == "81677" | muenchen$PostCode == "81679" | 
                    muenchen$PostCode == "81925" | muenchen$PostCode == "81927" | muenchen$PostCode == "81929"] <- "Bogenhausen"

muenchen$District[muenchen$PostCode == "80933" | muenchen$PostCode == "80935" | muenchen$PostCode == "80995"] <- "Feldmoching-Hasenbergl"

muenchen$District[muenchen$PostCode == "80689" | muenchen$PostCode == "81375" | muenchen$PostCode == "81377"] <- "Hadern"

muenchen$District[muenchen$PostCode == "80686" | muenchen$PostCode == "80687" | muenchen$PostCode == "80689"] <- "Laim"

muenchen$District[muenchen$PostCode == "80335" | muenchen$PostCode == "80336" | muenchen$PostCode == "80337" | 
                    muenchen$PostCode == "80469"] <- "Ludwigsvorstadt-Isarvorstadt"

muenchen$District[muenchen$PostCode == "80333" | muenchen$PostCode == "80335" | muenchen$PostCode == "80539" | 
                    muenchen$PostCode == "80636" | muenchen$PostCode == "80797" | muenchen$PostCode == "80798" | 
                    muenchen$PostCode == "80799" | muenchen$PostCode == "80801" | muenchen$PostCode == "80802"] <- "Maxvorstadt"

muenchen$District[muenchen$PostCode == "80807" | muenchen$PostCode == "80809" | muenchen$PostCode == "80937" | 
                    muenchen$PostCode == "80939"] <- "Milbertshofen-Am Hart"

muenchen$District[muenchen$PostCode == "80637" | muenchen$PostCode == "80638" | muenchen$PostCode == "80992" | 
                    muenchen$PostCode == "80993" | muenchen$PostCode == "80997"] <- "Moosach"

muenchen$District[muenchen$PostCode == "80634" | muenchen$PostCode == "80636" | muenchen$PostCode == "80637" | 
                    muenchen$PostCode == "80638" | muenchen$PostCode == "80639"] <- "Neuhausen-Nymphenburg"

muenchen$District[muenchen$PostCode == "81539" | muenchen$PostCode == "81541" | muenchen$PostCode == "81547" | 
                    muenchen$PostCode == "81549"] <- "Obergiesing"

muenchen$District[muenchen$PostCode == "80687" | muenchen$PostCode == "80689" | muenchen$PostCode == "81241" | 
                    muenchen$PostCode == "81243" | muenchen$PostCode == "81245" | muenchen$PostCode == "81247"] <- "Pasing-Obermenzing"

muenchen$District[muenchen$PostCode == "81539" | muenchen$PostCode == "81549" | muenchen$PostCode == "81669" | 
                    muenchen$PostCode == "81671" | muenchen$PostCode == "81735" | muenchen$PostCode == "81737" | 
                    muenchen$PostCode == "81739"] <- "Ramersdorf-Perlach"

muenchen$District[muenchen$PostCode == "80538" | muenchen$PostCode == "80801" | muenchen$PostCode == "80802" | 
                    muenchen$PostCode == "80803" | muenchen$PostCode == "80804" | muenchen$PostCode == "80805" | 
                    muenchen$PostCode == "80807" | muenchen$PostCode == "80939"] <- "Schwabing-Freimann"

muenchen$District[muenchen$PostCode == "80796" | muenchen$PostCode == "80797" | muenchen$PostCode == "80798" | 
                    muenchen$PostCode == "80799" | muenchen$PostCode == "80801" | muenchen$PostCode == "80803" | 
                    muenchen$PostCode == "80804" | muenchen$PostCode == "80809"] <- "Schwabing-West"

muenchen$District[muenchen$PostCode == "80335" | muenchen$PostCode == "80339"] <- "Schwanthalerhoehe"

muenchen$District[muenchen$PostCode == "80336" | muenchen$PostCode == "80337" | muenchen$PostCode == "80469" | 
                    muenchen$PostCode == "81369" | muenchen$PostCode == "81371" | muenchen$PostCode == "81373" | 
                    muenchen$PostCode == "81379"] <- "Sendling"

muenchen$District[muenchen$PostCode == "80686" | muenchen$PostCode == "81369" | muenchen$PostCode == "81373" | 
                    muenchen$PostCode == "81377" | muenchen$PostCode == "81379"] <- "Sendling-Westpark"

muenchen$District[muenchen$PostCode == "81379" | muenchen$PostCode == "81475" | muenchen$PostCode == "81476" | 
                    muenchen$PostCode == "81477" | muenchen$PostCode == "81479"] <- "Thalkirchen-Obersendling-Fuerstenried-Forstenried-Solln"

muenchen$District[muenchen$PostCode == "81735" | muenchen$PostCode == "81825" | muenchen$PostCode == "81827" | 
                    muenchen$PostCode == "81829"] <- "Trudering-Riem"

muenchen$District[muenchen$PostCode == "81543" | muenchen$PostCode == "81545" | muenchen$PostCode == "81547"] <- "Untergiesing-Harlaching"

table(muenchen$District)

hamburg <- filter(dataset5, State == "Hamburg")
hamburg$District[hamburg$PostCode == "22113" | hamburg$PostCode == "22111" | hamburg$PostCode == "22113" | 
                   hamburg$PostCode == "22115" | hamburg$PostCode == "22117" | hamburg$PostCode == "22119" | 
                   hamburg$PostCode == "20535" | hamburg$PostCode == "20537" | hamburg$PostCode == "21129" | 
                   hamburg$PostCode == "20457" | hamburg$PostCode == "20539" | hamburg$PostCode == "20095" | 
                   hamburg$PostCode == "20099" | hamburg$PostCode == "20457" | hamburg$PostCode == "20459" | 
                   hamburg$PostCode == "27499" | hamburg$PostCode == "20097" | hamburg$PostCode == "20535" | 
                   hamburg$PostCode == "20537" | hamburg$PostCode == "22087" | hamburg$PostCode == "22089" | 
                   hamburg$PostCode == "22111" | hamburg$PostCode == "20095" | hamburg$PostCode == "20097" | 
                   hamburg$PostCode == "20537" | hamburg$PostCode == "22111" | hamburg$PostCode == "22113" | 
                   hamburg$PostCode == "22119" | hamburg$PostCode == "20457" | hamburg$PostCode == "20539" | 
                   hamburg$PostCode == "20354" | hamburg$PostCode == "20355" | hamburg$PostCode == "20359" | 
                   hamburg$PostCode == "20457" | hamburg$PostCode == "20459" | hamburg$PostCode == "20539" | 
                   hamburg$PostCode == "20095" | hamburg$PostCode == "20097" | hamburg$PostCode == "20099" | 
                   hamburg$PostCode == "20354" | hamburg$PostCode == "20355" | hamburg$PostCode == "20357" | 
                   hamburg$PostCode == "20359" | hamburg$PostCode == "20459" | hamburg$PostCode == "22767" | 
                   hamburg$PostCode == "22769" | hamburg$PostCode == "20457" | hamburg$PostCode == "21107" | 
                   hamburg$PostCode == "20539" | hamburg$PostCode == "21109" | hamburg$PostCode == "21129" | 
                   hamburg$PostCode == "20539" | hamburg$PostCode == "21107" | hamburg$PostCode == "21109"] <- "Mitte"

hamburg$District[hamburg$PostCode == "20359" | hamburg$PostCode == "22765" | hamburg$PostCode == "22767" | 
                   hamburg$PostCode == "22769" | hamburg$PostCode == "20257" | hamburg$PostCode == "20357" | 
                   hamburg$PostCode == "22765" | hamburg$PostCode == "22769" | hamburg$PostCode == "22525" | 
                   hamburg$PostCode == "22547" | hamburg$PostCode == "22549" | hamburg$PostCode == "22605" | 
                   hamburg$PostCode == "22607" | hamburg$PostCode == "22761" | hamburg$PostCode == "22769" | 
                   hamburg$PostCode == "22587" | hamburg$PostCode == "22589" | hamburg$PostCode == "22605" | 
                   hamburg$PostCode == "22607" | hamburg$PostCode == "22609" | hamburg$PostCode == "22589" | 
                   hamburg$PostCode == "22525" | hamburg$PostCode == "22547" | hamburg$PostCode == "22549" | 
                   hamburg$PostCode == "22587" | hamburg$PostCode == "22607" | hamburg$PostCode == "22609" | 
                   hamburg$PostCode == "22549" | hamburg$PostCode == "22587" | hamburg$PostCode == "22589" | 
                   hamburg$PostCode == "22609" | hamburg$PostCode == "22605" | hamburg$PostCode == "22607" | 
                   hamburg$PostCode == "22609" | hamburg$PostCode == "22763" | hamburg$PostCode == "22763" | 
                   hamburg$PostCode == "22765" | hamburg$PostCode == "22767" | hamburg$PostCode == "22559" | 
                   hamburg$PostCode == "22587" | hamburg$PostCode == "20357" | hamburg$PostCode == "20359" | 
                   hamburg$PostCode == "22767" | hamburg$PostCode == "22769" | hamburg$PostCode == "22587" | 
                   hamburg$PostCode == "22589"] <- "Altona"

hamburg$District[hamburg$PostCode == "22457" | hamburg$PostCode == "22523" | hamburg$PostCode == "22525" | 
                   hamburg$PostCode == "22527" | hamburg$PostCode == "22547" | hamburg$PostCode == "20144" | 
                   hamburg$PostCode == "20253" | hamburg$PostCode == "20255" | hamburg$PostCode == "20257" | 
                   hamburg$PostCode == "20259" | hamburg$PostCode == "20357" | hamburg$PostCode == "22525" | 
                   hamburg$PostCode == "22527" | hamburg$PostCode == "22769" | hamburg$PostCode == "20144" | 
                   hamburg$PostCode == "20146" | hamburg$PostCode == "20148" | hamburg$PostCode == "20149" |
                   hamburg$PostCode == "20249" | hamburg$PostCode == "20253" | hamburg$PostCode == "20253" | 
                   hamburg$PostCode == "20255" | hamburg$PostCode == "22529" | hamburg$PostCode == "20253" | 
                   hamburg$PostCode == "20255" | hamburg$PostCode == "22527" | hamburg$PostCode == "22529" | 
                   hamburg$PostCode == "22453" | hamburg$PostCode == "22455" | hamburg$PostCode == "22457" | 
                   hamburg$PostCode == "22459" | hamburg$PostCode == "22529" | hamburg$PostCode == "20144" | 
                   hamburg$PostCode == "20146" | hamburg$PostCode == "20148" | hamburg$PostCode == "20149" |
                   hamburg$PostCode == "20354" | hamburg$PostCode == "20357" | hamburg$PostCode == "22455" | 
                   hamburg$PostCode == "22457" | hamburg$PostCode == "22459" | hamburg$PostCode == "20255" | 
                   hamburg$PostCode == "22525" | hamburg$PostCode == "22527" | hamburg$PostCode == "22529" | 
                   hamburg$PostCode == "22769"] <- "Eimsbuettel"

hamburg$District[hamburg$PostCode == "20251" | hamburg$PostCode == "22297" | hamburg$PostCode == "22335" | 
                   hamburg$PostCode == "22337" | hamburg$PostCode == "22297" | hamburg$PostCode == "22303" | 
                   hamburg$PostCode == "22305" | hamburg$PostCode == "22307" | hamburg$PostCode == "22309" | 
                   hamburg$PostCode == "22081" | hamburg$PostCode == "22083" | hamburg$PostCode == "22085" | 
                   hamburg$PostCode == "22305" | hamburg$PostCode == "22049" | hamburg$PostCode == "20249" | 
                   hamburg$PostCode == "20251" | hamburg$PostCode == "22529" | hamburg$PostCode == "22335" | 
                   hamburg$PostCode == "22339" | hamburg$PostCode == "22415" | hamburg$PostCode == "22453" | 
                   hamburg$PostCode == "22297" | hamburg$PostCode == "22335" | hamburg$PostCode == "22453" | 
                   hamburg$PostCode == "22529" | hamburg$PostCode == "20144" | hamburg$PostCode == "20249" | 
                   hamburg$PostCode == "20251" | hamburg$PostCode == "20253" | hamburg$PostCode == "22087" | 
                   hamburg$PostCode == "22089" | hamburg$PostCode == "22415" | hamburg$PostCode == "22417" | 
                   hamburg$PostCode == "22419" | hamburg$PostCode == "22309" | hamburg$PostCode == "22335" | 
                   hamburg$PostCode == "22337" | hamburg$PostCode == "22391" | hamburg$PostCode == "22081" | 
                   hamburg$PostCode == "22085" | hamburg$PostCode == "22087" | hamburg$PostCode == "20249" | 
                   hamburg$PostCode == "22297" | hamburg$PostCode == "22299" | hamburg$PostCode == "22301" | 
                   hamburg$PostCode == "22303" | hamburg$PostCode == "22305"] <- "Nord"

hamburg$District[hamburg$PostCode == "22359" | hamburg$PostCode == "22395" | hamburg$PostCode == "22047" | 
                   hamburg$PostCode == "22159" | hamburg$PostCode == "22175" | hamburg$PostCode == "22177" | 
                   hamburg$PostCode == "22179" | hamburg$PostCode == "22309" | hamburg$PostCode == "22391" | 
                   hamburg$PostCode == "22393" | hamburg$PostCode == "22397" | hamburg$PostCode == "22089" | 
                   hamburg$PostCode == "22145" | hamburg$PostCode == "22159" | hamburg$PostCode == "22339" | 
                   hamburg$PostCode == "22391" | hamburg$PostCode == "22399" | hamburg$PostCode == "22415" | 
                   hamburg$PostCode == "22417" | hamburg$PostCode == "22043" | hamburg$PostCode == "22045" | 
                   hamburg$PostCode == "22397" | hamburg$PostCode == "22399" | hamburg$PostCode == "22041" | 
                   hamburg$PostCode == "22043" | hamburg$PostCode == "22089" | hamburg$PostCode == "22391" | 
                   hamburg$PostCode == "22393" | hamburg$PostCode == "22395" | hamburg$PostCode == "22399" | 
                   hamburg$PostCode == "22143" | hamburg$PostCode == "22145" | hamburg$PostCode == "22147" | 
                   hamburg$PostCode == "22149" | hamburg$PostCode == "22359" | hamburg$PostCode == "22159" | 
                   hamburg$PostCode == "22391" | hamburg$PostCode == "22393" | hamburg$PostCode == "22395" | 
                   hamburg$PostCode == "22177" | hamburg$PostCode == "22309" | hamburg$PostCode == "22041" | 
                   hamburg$PostCode == "22043" | hamburg$PostCode == "22045" | hamburg$PostCode == "22047" | 
                   hamburg$PostCode == "22149" | hamburg$PostCode == "22159" | hamburg$PostCode == "22359" | 
                   hamburg$PostCode == "22041" | hamburg$PostCode == "22047" | hamburg$PostCode == "22049" | 
                   hamburg$PostCode == "22089" | hamburg$PostCode == "22391" | hamburg$PostCode == "22393" | 
                   hamburg$PostCode == "22395" | hamburg$PostCode == "22397"] <- "Wandsbek"

hamburg$District[hamburg$PostCode == "21035" | hamburg$PostCode == "21037" | hamburg$PostCode == "22113" | 
                   hamburg$PostCode == "21029" | hamburg$PostCode == "21039" | hamburg$PostCode == "21029" | 
                   hamburg$PostCode == "21031" | hamburg$PostCode == "21033" | hamburg$PostCode == "21035" | 
                   hamburg$PostCode == "21039" | hamburg$PostCode == "21033" | hamburg$PostCode == "21035" | 
                   hamburg$PostCode == "22113" | hamburg$PostCode == "21029" | hamburg$PostCode == "21037" | 
                   hamburg$PostCode == "21039" | hamburg$PostCode == "21037" | hamburg$PostCode == "21031" | 
                   hamburg$PostCode == "21033" | hamburg$PostCode == "22113" | hamburg$PostCode == "22115" | 
                   hamburg$PostCode == "22113" | hamburg$PostCode == "21035" | hamburg$PostCode == "21037" | 
                   hamburg$PostCode == "21039" | hamburg$PostCode == "21037" | hamburg$PostCode == "21037" | 
                   hamburg$PostCode == "21037" | hamburg$PostCode == "21037"] <- "Bergedorf"

hamburg$District[hamburg$PostCode == "21129" | hamburg$PostCode == "21129" | hamburg$PostCode == "21073" | 
                   hamburg$PostCode == "21075" | hamburg$PostCode == "21077" | hamburg$PostCode == "21129" | 
                   hamburg$PostCode == "21079" | hamburg$PostCode == "21073" | hamburg$PostCode == "21075" | 
                   hamburg$PostCode == "21079" | hamburg$PostCode == "21075" | hamburg$PostCode == "21079" | 
                   hamburg$PostCode == "21147" | hamburg$PostCode == "21149" | hamburg$PostCode == "21073" | 
                   hamburg$PostCode == "21075" | hamburg$PostCode == "21079" | hamburg$PostCode == "21077" | 
                   hamburg$PostCode == "21079" | hamburg$PostCode == "21077" | hamburg$PostCode == "21079" | 
                   hamburg$PostCode == "21129" | hamburg$PostCode == "21129" | hamburg$PostCode == "21147" | 
                   hamburg$PostCode == "21149" | hamburg$PostCode == "21079" | hamburg$PostCode == "21077" | 
                   hamburg$PostCode == "21079" | hamburg$PostCode == "21077" | hamburg$PostCode == "21079" | 
                   hamburg$PostCode == "21073" | hamburg$PostCode == "21077" | hamburg$PostCode == "21079"] <- "Harburg"

table(hamburg$District)

dat01 <- filter(hamburg, DeliveryDate >= "2014-07-01" & DeliveryDate <= "2014-07-31")
dat02 <- filter(hamburg, DeliveryDate >= "2014-08-01" & DeliveryDate <= "2014-08-31")
dat03 <- filter(hamburg, DeliveryDate >= "2014-09-01" & DeliveryDate <= "2014-09-30")
dat04 <- filter(hamburg, DeliveryDate >= "2014-10-01" & DeliveryDate <= "2014-10-31")
dat05 <- filter(hamburg, DeliveryDate >= "2014-11-01" & DeliveryDate <= "2014-11-30")
dat06 <- filter(hamburg, DeliveryDate >= "2014-12-01" & DeliveryDate <= "2014-12-31")
dat07 <- filter(hamburg, DeliveryDate >= "2015-01-01" & DeliveryDate <= "2015-01-31")
dat08 <- filter(hamburg, DeliveryDate >= "2015-02-01" & DeliveryDate <= "2015-02-28")
dat09 <- filter(hamburg, DeliveryDate >= "2015-03-01" & DeliveryDate <= "2015-03-31")
dat10 <- filter(hamburg, DeliveryDate >= "2015-04-01" & DeliveryDate <= "2015-04-30")
dat11 <- filter(hamburg, DeliveryDate >= "2015-05-01" & DeliveryDate <= "2015-05-31")
dat12 <- filter(hamburg, DeliveryDate >= "2015-06-01" & DeliveryDate <= "2015-06-30")
dat13 <- filter(hamburg, DeliveryDate >= "2015-07-01" & DeliveryDate <= "2015-07-31")
dat14 <- filter(hamburg, DeliveryDate >= "2015-08-01" & DeliveryDate <= "2015-08-31")
dat15 <- filter(hamburg, DeliveryDate >= "2015-09-01" & DeliveryDate <= "2015-09-30")
dat16 <- filter(hamburg, DeliveryDate >= "2015-10-01" & DeliveryDate <= "2015-10-31")
dat17 <- filter(hamburg, DeliveryDate >= "2015-11-01" & DeliveryDate <= "2015-11-30")
dat18 <- filter(hamburg, DeliveryDate >= "2015-12-01" & DeliveryDate <= "2015-12-31")
dat19 <- filter(hamburg, DeliveryDate >= "2016-01-01" & DeliveryDate <= "2016-01-31")
dat20 <- filter(hamburg, DeliveryDate >= "2016-02-01" & DeliveryDate <= "2016-02-29")
dat21 <- filter(hamburg, DeliveryDate >= "2016-03-01" & DeliveryDate <= "2016-03-31")
dat22 <- filter(hamburg, DeliveryDate >= "2016-04-01" & DeliveryDate <= "2016-04-30")
dat23 <- filter(hamburg, DeliveryDate >= "2016-05-01" & DeliveryDate <= "2016-05-31")
dat24 <- filter(hamburg, DeliveryDate >= "2016-06-01" & DeliveryDate <= "2016-06-30")

# Hamburg Mitte
District <- rep("Mitte", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
hamburg_mitte <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat01, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat01, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat02, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat02, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat03, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat03, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat04, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat04, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat05, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat05, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat06, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat06, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat07, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat07, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat08, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat08, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat09, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat09, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat10, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat10, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat11, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat11, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat12, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat12, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat13, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat13, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat14, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat14, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat15, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat15, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat16, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat16, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat17, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat17, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat18, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat18, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat19, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat19, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat20, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat20, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat21, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat21, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat22, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat22, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat23, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat23, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Mitte"))[1] / dim(filter(dat24, District == "Mitte"))[1]
hamburg_mitte$DeliveryInTime[hamburg_mitte$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Mitte"))[1] / dim(filter(dat24, District == "Mitte"))[1]

plot13 <- ggplot(data = hamburg_mitte, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Hamburg Mitte")

# Hamburg Altona
District <- rep("Altona", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
hamburg_altona <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat01, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat01, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat02, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat02, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat03, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat03, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat04, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat04, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat05, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat05, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat06, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat06, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat07, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat07, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat08, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat08, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat09, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat09, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat10, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat10, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat11, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat11, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat12, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat12, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat13, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat13, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat14, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat14, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat15, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat15, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat16, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat16, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat17, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat17, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat18, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat18, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat19, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat19, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat20, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat20, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat21, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat21, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat22, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat22, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat23, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat23, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Altona"))[1] / dim(filter(dat24, District == "Altona"))[1]
hamburg_altona$DeliveryInTime[hamburg_altona$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Altona"))[1] / dim(filter(dat24, District == "Altona"))[1]

plot14 <- ggplot(data = hamburg_altona, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Hamburg Altona")

# Hamburg Eimsbuettel
District <- rep("Eimsbuettel", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
hamburg_eimsbuettel <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat01, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat01, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat02, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat02, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat03, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat03, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat04, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat04, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat05, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat05, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat06, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat06, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat07, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat07, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat08, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat08, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat09, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat09, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat10, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat10, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat11, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat11, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat12, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat12, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat13, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat13, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat14, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat14, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat15, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat15, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat16, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat16, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat17, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat17, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat18, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat18, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat19, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat19, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat20, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat20, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat21, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat21, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat22, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat22, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat23, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat23, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat24, District == "Eimsbuettel"))[1]
hamburg_eimsbuettel$DeliveryInTime[hamburg_eimsbuettel$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Eimsbuettel"))[1] / dim(filter(dat24, District == "Eimsbuettel"))[1]

plot15 <- ggplot(data = hamburg_eimsbuettel, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Hamburg Eimsbuettel")

# Hamburg Nord
District <- rep("Nord", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
hamburg_nord <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat01, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat01, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat02, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat02, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat03, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat03, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat04, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat04, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat05, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat05, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat06, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat06, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat07, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat07, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat08, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat08, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat09, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat09, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat10, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat10, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat11, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat11, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat12, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat12, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat13, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat13, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat14, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat14, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat15, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat15, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat16, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat16, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat17, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat17, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat18, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat18, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat19, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat19, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat20, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat20, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat21, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat21, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat22, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat22, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat23, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat23, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Nord"))[1] / dim(filter(dat24, District == "Nord"))[1]
hamburg_nord$DeliveryInTime[hamburg_nord$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Nord"))[1] / dim(filter(dat24, District == "Nord"))[1]

plot16 <- ggplot(data = hamburg_nord, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Hamburg Nord")

# Hamburg Wandsbek
District <- rep("Wandsbek", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
hamburg_wandsbek <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat01, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat01, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat02, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat02, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat03, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat03, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat04, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat04, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat05, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat05, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat06, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat06, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat07, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat07, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat08, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat08, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat09, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat09, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat10, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat10, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat11, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat11, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat12, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat12, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat13, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat13, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat14, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat14, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat15, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat15, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat16, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat16, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat17, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat17, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat18, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat18, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat19, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat19, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat20, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat20, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat21, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat21, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat22, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat22, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat23, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat23, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat24, District == "Wandsbek"))[1]
hamburg_wandsbek$DeliveryInTime[hamburg_wandsbek$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Wandsbek"))[1] / dim(filter(dat24, District == "Wandsbek"))[1]

plot17 <- ggplot(data = hamburg_wandsbek, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Hamburg Wandsbek")

# Hamburg Bergedorf
District <- rep("Bergedorf", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
hamburg_bergedorf <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat01, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat01, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat02, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat02, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat03, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat03, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat04, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat04, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat05, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat05, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat06, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat06, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat07, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat07, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat08, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat08, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat09, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat09, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat10, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat10, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat11, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat11, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat12, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat12, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat13, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat13, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat14, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat14, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat15, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat15, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat16, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat16, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat17, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat17, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat18, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat18, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat19, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat19, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat20, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat20, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat21, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat21, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat22, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat22, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat23, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat23, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat24, District == "Bergedorf"))[1]
hamburg_bergedorf$DeliveryInTime[hamburg_bergedorf$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Bergedorf"))[1] / dim(filter(dat24, District == "Bergedorf"))[1]

plot18 <- ggplot(data = hamburg_bergedorf, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +ggtitle("Time Series: Hamburg Bergedorf")

# Hamburg Harburg
District <- rep("Harburg", 48)
Date <- rep(c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
              "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12", "2016-01", "2016-02", 
              "2016-03", "2016-04", "2016-05", "2016-06"), 2)

DeliveryInTime <- rep(NA, 48)
TimeSlot <- c(rep("with tolerance", 24), rep("without tolerance", 24))
hamburg_harburg <- data.frame(District, Date, DeliveryInTime, as.factor(TimeSlot)) 

hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-07" & TimeSlot == "with tolerance"] <- dim(filter(dat01, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat01, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-07" & TimeSlot == "without tolerance"] <- dim(filter(dat01, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat01, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-08" & TimeSlot == "with tolerance"] <- dim(filter(dat02, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat02, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-08" & TimeSlot == "without tolerance"] <- dim(filter(dat02, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat02, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-09" & TimeSlot == "with tolerance"] <- dim(filter(dat03, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat03, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-09" & TimeSlot == "without tolerance"] <- dim(filter(dat03, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat03, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-10" & TimeSlot == "with tolerance"] <- dim(filter(dat04, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat04, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-10" & TimeSlot == "without tolerance"] <- dim(filter(dat04, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat04, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-11" & TimeSlot == "with tolerance"] <- dim(filter(dat05, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat05, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-11" & TimeSlot == "without tolerance"] <- dim(filter(dat05, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat05, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-12" & TimeSlot == "with tolerance"] <- dim(filter(dat06, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat06, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2014-12" & TimeSlot == "without tolerance"] <- dim(filter(dat06, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat06, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-01" & TimeSlot == "with tolerance"] <- dim(filter(dat07, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat07, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-01" & TimeSlot == "without tolerance"] <- dim(filter(dat07, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat07, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-02" & TimeSlot == "with tolerance"] <- dim(filter(dat08, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat08, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-02" & TimeSlot == "without tolerance"] <- dim(filter(dat08, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat08, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-03" & TimeSlot == "with tolerance"] <- dim(filter(dat09, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat09, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-03" & TimeSlot == "without tolerance"] <- dim(filter(dat09, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat09, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-04" & TimeSlot == "with tolerance"] <- dim(filter(dat10, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat10, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-04" & TimeSlot == "without tolerance"] <- dim(filter(dat10, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat10, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-05" & TimeSlot == "with tolerance"] <- dim(filter(dat11, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat11, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-05" & TimeSlot == "without tolerance"] <- dim(filter(dat11, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat11, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-06" & TimeSlot == "with tolerance"] <- dim(filter(dat12, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat12, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-06" & TimeSlot == "without tolerance"] <- dim(filter(dat12, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat12, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-07" & TimeSlot == "with tolerance"] <- dim(filter(dat13, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat13, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-07" & TimeSlot == "without tolerance"] <- dim(filter(dat13, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat13, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-08" & TimeSlot == "with tolerance"] <- dim(filter(dat14, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat14, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-08" & TimeSlot == "without tolerance"] <- dim(filter(dat14, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat14, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-09" & TimeSlot == "with tolerance"] <- dim(filter(dat15, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat15, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-09" & TimeSlot == "without tolerance"] <- dim(filter(dat15, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat15, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-10" & TimeSlot == "with tolerance"] <- dim(filter(dat16, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat16, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-10" & TimeSlot == "without tolerance"] <- dim(filter(dat16, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat16, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-11" & TimeSlot == "with tolerance"] <- dim(filter(dat17, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat17, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-11" & TimeSlot == "without tolerance"] <- dim(filter(dat17, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat17, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-12" & TimeSlot == "with tolerance"] <- dim(filter(dat18, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat18, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2015-12" & TimeSlot == "without tolerance"] <- dim(filter(dat18, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat18, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-01" & TimeSlot == "with tolerance"] <- dim(filter(dat19, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat19, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-01" & TimeSlot == "without tolerance"] <- dim(filter(dat19, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat19, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-02" & TimeSlot == "with tolerance"] <- dim(filter(dat20, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat20, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-02" & TimeSlot == "without tolerance"] <- dim(filter(dat20, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat20, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-03" & TimeSlot == "with tolerance"] <- dim(filter(dat21, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat21, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-03" & TimeSlot == "without tolerance"] <- dim(filter(dat21, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat21, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-04" & TimeSlot == "with tolerance"] <- dim(filter(dat22, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat22, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-04" & TimeSlot == "without tolerance"] <- dim(filter(dat22, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat22, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-05" & TimeSlot == "with tolerance"] <- dim(filter(dat23, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat23, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-05" & TimeSlot == "without tolerance"] <- dim(filter(dat23, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat23, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-06" & TimeSlot == "with tolerance"] <- dim(filter(dat24, DeliveryInTimeWT == "Yes" & District == "Harburg"))[1] / dim(filter(dat24, District == "Harburg"))[1]
hamburg_harburg$DeliveryInTime[hamburg_harburg$Date == "2016-06" & TimeSlot == "without tolerance"] <- dim(filter(dat24, DeliveryInTime == "Yes" & District == "Harburg"))[1] / dim(filter(dat24, District == "Harburg"))[1]

plot19 <- ggplot(data = hamburg_harburg, aes(x = Date, y = DeliveryInTime, color = TimeSlot, group = TimeSlot)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_color_manual(values = c("blue", "red")) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + ggtitle("Time Series: Hamburg Harburg")

grid.arrange(plot13, plot14, plot15, plot16, plot17, plot18, plot19, ncol = 2, nrow = 4)
