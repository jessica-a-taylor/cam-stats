library(ggplot2)
library(readxl)
attach(TA_data)
library(stringr)
library(nortest)
library(rstatix)
library(purrr)
library(ggforce)
library(tidystats)
library(writexl)
library(MASS)
library(ggh4x)
library(ggpubr)


# TA comparisons

# Make sure plots save to the right place
wd <- getwd()
setwd(wd)
TA_data <- read_xlsx(file.choose())

mean_diff <- TA_data[c("Sample", "Dawn-Dusk (µmol H+ g-1 fwt)")]
lookinggood <- theme(axis.title = element_text(size = 4, color = "black"),
                     axis.text.x=element_text(colour="gray30", size = 4, vjust = 1),
                     axis.text.y=element_text(colour="gray30", size = 3, hjust = 1),
                     strip.text = element_text(size = 3),
                     strip.text.x = element_text(margin = margin(0.1)),
                     legend.text = element_text(size = 4),
                     legend.title = element_text(size=6))

# Comparing mean Dawn-Dusk TAs between leaf ages within each plant
plants <- list(
  "ZA"
  ,"ZB"
  ,"ZC"
  ,"ZD"
  ,"ZE"
  ,"ZF"
  ,"ZG"
  ,"ZH"
  ,"ZI"
  ,"ZJ"
)

# Initialise maxLeaf... we'll use it later :)
maxLeaf = 0

combined_graph <- data.frame(Plants = character(), Leaf_Age = integer(), Means = double())
plant_means <- data.frame(Individual = character(), Mean = double())

# For each plant string in plants
for (plant in plants) {
  # Filter mean_diff to only leaves from that plant
  LP <- mean_diff[str_detect(mean_diff$Sample, plant),]
  print(LP)
  

  # Find means across all leaves for each plant
  plant_means <- rbind(plant_means, data.frame(Individual=plant,
                                               Mean = mean(LP$`Dawn-Dusk (µmol H+ g-1 fwt)`, na.rm = TRUE)))
  
  # Get the ordered list of names for the graph
  titles <- LP$Sample
  
  # Make the graph
  graph <- ggplot(LP, aes(x = Sample, y = `Dawn-Dusk (µmol H+ g-1 fwt)`)) +
    geom_bar(stat = "identity", fill="grey") + theme_minimal() + lookingfine +
    scale_x_discrete(limits = titles) +
    labs(y = "Mean difference", x = "Leaf pair")

  # Save the graph
  ggsave(paste("plots/betweenLeaves/", plant, ".png", sep=""))
  
  # Do tests
  print(ad.test(LP$`Dawn-Dusk (µmol H+ g-1 fwt)`))
  print(shapiro_test(LP$`Dawn-Dusk (µmol H+ g-1 fwt)`))
  
  png(paste(file="plots/histograms/", plant, ".png", sep=""))
  hist(LP$`Dawn-Dusk (µmol H+ g-1 fwt)`, breaks=9)
  dev.off()
  
  
  # Update maxLeaf if the last leaf has a higher number than what's already there
  maxLeaf = max(maxLeaf, strtoi(str_extract(tail(LP$Sample, n=1), regex("[0-9]+$"))))
  print(maxLeaf)
  
  combined_graph <- rbind(combined_graph, data.frame(Plants = rep(plant, times=strtoi(str_extract(tail(LP$Sample, n=1), regex("[0-9]+$")))),
                               Leaf_Pair = str_extract(LP$Sample, regex("[0-9]+$")),
                               Means = LP$`Dawn-Dusk (µmol H+ g-1 fwt)`))
  
}

# Test for normality of means across leaves for all plants
hist(plant_means$Mean)
ad.test(plant_means$Mean)
shapiro_test(plant_means$Mean)

# Test for significant differences between plants
oneway.test(Means~Plants, data = combined_graph, var.equal = FALSE)
games_howell_test(combined_graph, Means~Plants, conf.level = 0.95)

# Generate a list of all the leaf numbers
# [2, 4, 6, ..., maxLeaf]
leafages <- map_chr(seq(2, maxLeaf, by=2), paste)

# Make combined plot
graph_combined <- ggplot(combined_graph, aes(x=Leaf_Pair, y=Means)) +
  geom_bar(stat="identity", aes(fill=Plants), show.legend = FALSE, width = 0.5, position=position_dodge(0.9)) +
  scale_x_discrete(limits=leafages) + theme_minimal() +
  theme(strip.background = element_rect(colour = "grey", fill = "white")) + lookingfine +
  labs(x = "Leaf pair number", y = "Difference in TA between dawn and dusk (µmol H+ g-1 fwt)") +
  facet_wrap(vars(Plants), ncol = 2, scales="free_y") + theme(aspect.ratio = 0.4)

ggsave(paste("plots/combinedPlot.png"))

# Create a new data frame
meansByLeafAge <- data.frame(Leaf_Age = integer(), Means = double(), SD = double(), stringsAsFactors = FALSE)
test_data <- data.frame(Leaf_Age = integer(), TA_diff = double(), stringsAsFactors = FALSE)
  
# For each leaf age
for (age in leafages) {
  
  # Filter mean_diff to get all leaves with that age
  leaf_age <- mean_diff[str_detect(mean_diff$Sample, regex(paste("Z[A-Za-z]", age, "$", sep=""))),]
  print(leaf_age)
  
  png(paste(file="plots/betweenPlants/histograms/", age, ".png", sep=""))
  hist(leaf_age$`Dawn-Dusk (µmol H+ g-1 fwt)`)
  dev.off()
  
  print(shapiro.test(leaf_age$`Dawn-Dusk (µmol H+ g-1 fwt)`))

  # Comparing how variable the Dawn-Dusk TAs are between plants at each leaf age
  
  graph3 <- ggplot(leaf_age, aes(x=Sample, y=`Dawn-Dusk (µmol H+ g-1 fwt)`)) +
    geom_bar(stat="identity", fill="grey") + theme_minimal() + lookingfine
  
  ggsave(paste("plots/betweenPlants/", age, ".png", sep=""))
  
  # Comparing mean Dawn-Dusk TAs for each leaf age between each plant
  # Add the leaf age and the corresponding mean to meansByLeafAge
  meansByLeafAge <- rbind(meansByLeafAge, data.frame(age, 
                                                     mean(leaf_age$`Dawn-Dusk (µmol H+ g-1 fwt)`, na.rm = TRUE),
                                                     sd(leaf_age$`Dawn-Dusk (µmol H+ g-1 fwt)`, na.rm = TRUE)))
  
  test_data <-  rbind(test_data, data.frame(age,
                                            leaf_age$`Dawn-Dusk (µmol H+ g-1 fwt)`))
}

# Set the names because they go away for some reason
meansByLeafAge <- setNames(meansByLeafAge, c("Leaf_Age", "Means", "SD"))
test_data <- setNames(test_data, c("Leaf_Age", "TA_diff"))

# Do tests
png(paste(file="plots/hist.png"))
hist(meansByLeafAge$Means, breaks=10)
dev.off()

shapiro.test(meansByLeafAge$Means)
ad.test(meansByLeafAge$Means)

test_data2 <- head(test_data, -1)
graph2_aov <- aov(TA_diff ~ Leaf_Age, data=test_data2)
summary(graph2_aov)

graph2_Welch <- oneway.test(TA_diff ~ Leaf_Age, data=test_data2, var.equal = FALSE)

# Grab the ordered list of leaf ages to order the x axis
titles <- meansByLeafAge$Leaf_Age

# Make and save the graph
graph2 <- ggplot(meansByLeafAge, aes(x = Leaf_Age, y = Means)) +
  scale_x_discrete(limits = titles) +
  geom_bar(stat="identity", fill="grey") + theme_minimal() + lookingfine +
  labs(x = "Leaf pair number", y = "Difference in TA between dawn and dusk (µmol H+ g-1 fwt)") +
  geom_errorbar(aes(x = Leaf_Age, y = Means, ymin = Means-SD, ymax = Means+SD), width = 0.2)

ggsave("plots/means_by_leaf_age.png")



# SWC and leaf area comparison
SWC_data <- read_xlsx(file.choose(),sheet=2)
water <- SWC_data[c("Sample", "% water")]
area <- SWC_data[c("Sample", "Leaf Area")]
maxLeaf = 0
water_combined <- data.frame(Plants = character(), Leaf_Age = integer(), Water = double())
water_means <- data.frame(Individual = character(), Mean = double())

area_combined <- data.frame(Plants = character(), Leaf_Age = integer(), Area = double())
area_means <- data.frame(Individual = character(), Mean = double())

s.script <- expression(Leaf~Area~cm^2)

for (plant in plants) {
  
  # for SWC...
  water_content <- water[str_detect(water$Sample, plant),]
  print(water_content)
  
  water_means <- rbind(water_means, data.frame(Individual = plant,
                                               Mean = mean(water_content$`% water`, na.rm = TRUE)))
  
  titles <- water_content$Sample
  
  water_graph <- ggplot(water_content, aes(x = Sample, y = `% water`)) +
    geom_bar(stat = "identity", fill="grey") + theme_minimal() + lookinggood +
    scale_x_discrete(limits = titles) +
    labs(y = "Percentage water", x = "Leaf pair number")
  
  # Save the graph
  #ggsave(paste("plots/SWC/betweenLeaves", plant, ".png", sep=""))
  
  print(ad.test(water_content$`% water`))
  print(shapiro_test(water_content$`% water`))
  
  #png(paste(file="plots/SWC/histograms/", plant, ".png", sep=""))
  #hist(water_content$`% water`, breaks=9)
  #dev.off()
  
  water_combined <- rbind(water_combined, data.frame(Plants = rep(plant, times=strtoi(str_extract(tail(water_content$Sample, n=1), regex("[0-9]+$")))),
                                                   Leaf_Age = str_extract(water_content$Sample, regex("[0-9]+$")),
                                                   Water = water_content$`% water`))
  
  # for leaf area...
  
  leaf_area <- area[str_detect(area$Sample, plant),]
  print(leaf_area)
  
  area_means <- rbind(area_means, data.frame(Individual = plant,
                                             Mean = mean(leaf_area$`Leaf Area`, na.rm = TRUE)))

  area_graph <- ggplot(leaf_area, aes(x = Sample, y = `Leaf Area`)) +
    geom_bar(stat = "identity", fill="grey") + theme_minimal() + lookinggood +
    scale_x_discrete(limits = titles) +
    labs(y = s.script, x = "Leaf pair number")
  
  # Save the graph
  #ggsave(paste("plots/leafArea/betweenLeaves", plant, ".png", sep=""))
  
  print(ad.test(leaf_area$`Leaf Area`))
  print(shapiro_test(leaf_area$`Leaf Area`))
  
  #png(paste(file="plots/leafArea/histograms/", plant, ".png", sep=""))
  #hist(leaf_area$`Leaf Area`, breaks=9)
  #dev.off()
  
  area_combined <- rbind(area_combined, data.frame(Plants = rep(plant, times=strtoi(str_extract(tail(leaf_area$Sample, n=1), regex("[0-9]+$")))),
                                                   Leaf_Age = str_extract(leaf_area$Sample, regex("[0-9]+$")),
                                                   Area = leaf_area$`Leaf Area`))
 
  maxLeaf = max(maxLeaf, strtoi(str_extract(tail(leaf_area$Sample, n=1), regex("[0-9]+$"))))
  print(maxLeaf)
}

leafarea <- map_chr(seq(2, maxLeaf, by=2), paste)

# Test for normality of means across leaves for all plants
hist(water_means$Mean)
ad.test(water_means$Mean)
shapiro_test(water_means$Mean)

hist(area_means$Mean)
ad.test(area_means$Mean)
shapiro_test(area_means$Mean)

# Test for significant differences between plants
oneway.test(Water~Plants, data = water_combined, var.equal = FALSE)
water_posthoc <- games_howell_test(water_combined, Water~Plants, conf.level = 0.95)
write_xlsx(water_posthoc, "plots/SWC/posthoc.xlsx")

oneway.test(Area~Plants, data = area_combined, var.equal = FALSE)

# Make combined plot
graph_water <- ggplot(water_combined, aes(x=Leaf_Age, y=Water)) +
  geom_bar(stat="identity", aes(fill=Plants), show.legend = FALSE, width = 0.5, position=position_dodge(0.9)) +
  scale_x_discrete(limits=leafarea) + theme_minimal() +
  theme(strip.background = element_rect(colour = "grey", fill = "white")) + lookingfine +
  labs(x = "Leaf pair number", y = "Percentage water") +
  facet_wrap(vars(Plants), ncol = 2, scales="free_y") + theme(aspect.ratio = 0.4)

graph_area <- ggplot(area_combined, aes(x=Leaf_Age, y=Area)) +
  geom_bar(stat="identity", aes(fill=Plants), show.legend = FALSE, width = 0.5, position=position_dodge(0.9)) +
  scale_x_discrete(limits=leafarea) + theme_minimal() +
  theme(strip.background = element_rect(colour = "grey", fill = "white")) + lookingfine +
  labs(x = "Leaf pair number", y = s.script) +
  facet_wrap(vars(Plants), ncol = 2, scales="free_y") + theme(aspect.ratio = 0.4)

ggsave("plots/leafArea/areaCombined.png")

all_watermeans <- data.frame(Leaf_Age = integer(), Means = double(), SD = double(), stringsAsFactors = FALSE)
water_test <- data.frame(Leaf_Age = integer(), Area = double(), stringsAsFactors = FALSE)

all_areameans <- data.frame(Leaf_Age = integer(), Means = double(), SD = double(), stringsAsFactors = FALSE)
area_test <- data.frame(Leaf_Age = integer(), Area = double(), stringsAsFactors = FALSE)

for (age in leafarea) {
  
  # for SWC...
  
  water_LA <- water[str_detect(water$Sample, regex(paste("Z[A-Za-z]", age, "$", sep=""))),]
  print(water_LA)
  
  all_watermeans <- rbind(all_watermeans, data.frame(Leaf_Age = age, 
                                               Means = mean(water_LA$`% water`, na.rm = TRUE),
                                               SD = sd(water_LA$`% water`, na.rm = TRUE)))
  
  water_test <- rbind(water_test, data.frame(Leaf_Age = age,
                                          Area = water_LA$`% water`))
  
  print(data.frame(Leaf_Age = age,
                   Area = water_LA$`% water`))
  
  # for leaf area...
  
  area_LA <- area[str_detect(area$Sample, regex(paste("Z[A-Za-z]", age, "$", sep=""))),]
  print(area_LA)
  
  all_areameans <- rbind(all_areameans, data.frame(Leaf_Age = age, 
                                           Means = mean(area_LA$`Leaf Area`, na.rm = TRUE),
                                           SD = sd(area_LA$`Leaf Area`, na.rm = TRUE)))
  
  area_test <- rbind(area_test, data.frame(Leaf_Age = age,
                                         Area = area_LA$`Leaf Area`))
  
  print(data.frame(Leaf_Age = age,
                   Area = area_LA$`Leaf Area`))
}

all_watermeans <- setNames(all_means, c("Leaf_Age", "Means", "SD"))
all_areameans <- setNames(all_means, c("Leaf_Age", "Means", "SD"))
water_test <- setNames(water_test, c("Leaf_Age", "Percentage water"))
area_test <- setNames(area_test, c("Leaf_Age", "Leaf Area"))


png(paste(file="plots/SWC/hist.png"))
hist(all_watermeans$Means, breaks=10)
dev.off()

shapiro.test(all_watermeans$Means)
ad.test(all_watermeans$Means)

oneway.test(`Percentage water`~ Leaf_Age, data=water_test, var.equal = FALSE)

png(paste(file="plots/leafArea/hist.png"))
hist(all_areameans$Means, breaks=10)
dev.off()

shapiro.test(all_areameans$Means)
ad.test(all_areameans$Means)

oneway.test(`Leaf Area`~ Leaf_Age, data=testData, var.equal = FALSE)
posthoc <- games_howell_test(testData, `Leaf Area`~ Leaf_Age, conf.level = 0.95)

write_xlsx(posthoc, "plots/leafArea/posthoc.xlsx")

# Grab the ordered list of leaf ages to order the x axis
titles <- all_means$Leaf_Age

# Make and save the graph
graph2 <- ggplot(all_means, aes(x = Leaf_Age, y = Means)) +
  scale_x_discrete(limits = titles) +
  geom_bar(stat="identity", fill="grey") + theme_minimal() + lookingfine +
  labs(x = "Leaf pair number", y = s.script) +
  geom_errorbar(aes(x = Leaf_Age, y = Means, ymin = Means-SD, ymax = Means+SD), width = 0.2)

ggsave("plots/leafArea/means_by_leaf_age.png")



# Comparing chlorophyll content
chloro_data <- read_xlsx(file.choose(),sheet=3)
chloro <- chloro_data[c("Sample", "Chlor A", "Chlor B", "A and B", "Car")]
maxLeaf = 0
chloro_combined <- data.frame(Plants = character(),
                              Chlor_A = double(), Chlor_B = double(), 
                              A_and_B = double(), Car = double())
all_chlor <- data.frame(Plants = character(), Leaf_Age = integer(), Chlor = character(), Chlor_content = double())
chlor_means <- data.frame(Plants = character(), Chlor = character(), Chlor_content = double())
all_chlormeans <-  data.frame(Individual = character(), A_mean = double(), B_mean = double(),
                              AB_mean = double(), Car = double())

for (plant in plants) {
  # Filter mean_diff to only leaves from that plant
  chloro_content <- chloro[str_detect(chloro$Sample, plant),]
  print(chloro_content)
  
  titles <- chloro_content$Sample
  
  all_chlor <- rbind(all_chlor, data.frame(Plants = rep(plant, times=(strtoi(str_extract(tail(chloro_content$Sample, n=1), regex("[0-9]+$"))))),
                                           Leaf_Age = rep(str_extract(chloro_content$Sample, regex("[0-9]+$")), each=4),
                                           Chlor = rep(c("Chlor A", "Chlor B", "A and B", "Car"), times=(strtoi(str_extract(tail(chloro_content$Sample, n=1), regex("[0-9]+$"))))),
                                           Chlor_content = c(chloro_content$`Chlor A`, chloro_content$`Chlor B`, chloro_content$`A and B`, chloro_content$Car)))

  #graph <- ggplot(chloro_content, aes(x = Plants, y = Chlor_content) +
    #geom_bar(stat = "identity", fill="grey") + theme_minimal() + lookinggood +
    #scale_x_discrete(limits = titles) +
    #labs(y = s.script, x = "Leaf pair")
  
  #ggsave(paste("plots/leafArea/", plant, ".png", sep=""))
  
  print(c(shapiro_test(chloro_content$`Chlor A`), shapiro_test(chloro_content$`Chlor B`), shapiro_test(chloro_content$`A and B`), shapiro_test(chloro_content$Car)))
  
  #png(paste(file="plots/chloro/histograms/chlorA/", plant, ".png", sep=""))
  #hist(chloro_content$`Chlor A`)
  #dev.off()
  #png(paste(file="plots/chloro/histograms/chlorb/", plant, ".png", sep=""))
  #hist(chloro_content$`Chlor B`)
  #dev.off()
  #png(paste(file="plots/chloro/histograms/A_and_B/", plant, ".png", sep=""))
  #hist(chloro_content$`A and B`)
  #dev.off()
  #png(paste(file="plots/chloro/histograms/car/", plant, ".png", sep=""))
  #hist(chloro_content$Car)
  #dev.off()
  
  maxLeaf = max(maxLeaf, strtoi(str_extract(tail(chloro_content$Sample, n=1), regex("[0-9]+$"))))
  print(maxLeaf)
  
  chloro_combined <- rbind(chloro_combined, data.frame(Plants = plant, 
                                                       Chlor_A = chloro_content$`Chlor A`,
                                                       Chlor_B = chloro_content$`Chlor B`, 
                                                       A_and_B = chloro_content$`A and B`,
                                                       Car = chloro_content$Car))

  all_chlormeans <-  rbind(all_chlormeans, data.frame(Individual = plant,
                                                     A_mean = mean(chloro_content$`Chlor A`),
                                                     B_mean = mean(chloro_content$`Chlor B`),
                                                     AB_mean = mean(chloro_content$`A and B`),
                                                     Car = mean(chloro_content$Car)))
}

chloro_ages <- map_chr(seq(2, maxLeaf, by=2), paste)
all_chlormeans <- setNames(all_chlormeans, c("Individual", "Chlor A", "Chlor B", "A and B", "Car"))
chloro_combined <- setNames(chloro_combined, c("Plants", "Chlor A", "Chlor B", "A and B", "Car"))


# Make combined plot

lookinggood <- theme(axis.title = element_text(size = 6, color = "black"),
                     axis.text.x=element_text(colour="gray30", size = 4, vjust = 4),
                     axis.text.y=element_text(colour="gray30", size = 4, hjust = 1),
                     strip.text = element_text(size = 4),
                     strip.text.x = element_text(margin = margin(0.1)),
                     legend.text = element_text(size = 4),
                     legend.title = element_text(size=6),
                     legend.key.size = unit(0.1, "cm"))

graph_area <- ggplot(all_chlor, aes(x=Leaf_Age, y=Chlor_content)) +
  geom_bar(stat="identity", aes(fill=Chlor), show.legend = TRUE, width=0.8, position=position_dodge(0.9)) +
  scale_x_discrete(limits=chloro_ages) + theme_minimal() +
  theme(strip.background = element_rect(colour = "grey", fill = "grey")) + lookinggood +
  labs(x = "Leaf pair number", y = "Chlor") +
  facet_wrap(vars(Plants), ncol = 2, scales="free_y")+
  theme(aspect.ratio = 0.2) + force_panelsizes(rows = 0.5)

ggsave("plots/chloro/all_chlor.png")

chlor_type <- list(
  "Chlor A"
  ,"Chlor B"
  ,"A and B"
  ,"Car"
)

chlor_means <- data.frame(Leaf_Age = integer(), Chlorophyll = character(), Means = double(), SD = double(), stringsAsFactors = FALSE)
testData <- data.frame(Sample = character(), `Chlor A` = double(), `Chlor B` = double(), `A and B` = double(), `Car` = double(), stringsAsFactors = FALSE)

for (age in chloro_ages) {

  for (chl in chlor_type) {
    chl_LA <- data.frame(Sample = chloro$Sample,
                         chl = chloro[chl])
    chl_LA <- setNames(chl_LA, c("Sample", chl))
    chloro_LA <- chl_LA[str_detect(chl_LA$Sample, regex(paste("Z[A-Za-z]", age, "$", sep=""))),]
    chloro_LA <- setNames(chloro_LA, c("Sample", chl))
    
    png(paste(file="plots/chloro/histograms/hist", chl, ".png"))
    hist(as.numeric(unlist(chloro_LA[chl])), breaks=10, xlab = chl)
    dev.off()
    
    chlor_means <- rbind(chlor_means, data.frame(Leaf_Age = age, 
                                                 Chlorophyll = chl,
                                                 Means = mean(as.numeric(unlist(chloro_LA[chl])), na.rm = TRUE),
                                                 SD = sd(as.numeric(unlist(chloro_LA[chl])), na.rm = TRUE)))

    chl_means <- chlor_means[str_detect(chlor_means$Chlorophyll, chl),]
    png(paste(file="plots/chloro/hist", chl, ".png"))
    hist(chl_means$Means, breaks=10, xlab = chl)
    dev.off()
    
    
  }
  
  
  ageRows <- chloro[str_detect(chloro$Sample, regex(paste(age, "$", sep=""))),]
  ageRows <- subset(ageRows, select = -Sample)
  ageRows$leafAge <- age

  testData <- rbind(testData, ageRows)

}


chlor_means <- setNames(chlor_means, c("Leaf_Age", "Chlorophyll", "Means", "SD"))
testData <- setNames(testData, c("Chlor A", "Chlor B", "A and B", "Car", "leafAge"))
lookingfine <- theme(axis.title = element_text(size = 8, color = "black"), 
                     axis.text.x=element_text(colour="gray30", size = 6), 
                     axis.text.y=element_text(colour="gray30", size = 6))

lookingDave <- theme(axis.title = element_blank(),
                     axis.text.x=element_text(colour="gray30", size = 6), 
                     axis.text.y=element_text(colour="gray30", size = 6))

for (type in chlor_type) {
  
  x <- as.numeric(unlist(testData[type]))
  y <- testData$leafAge
  
  chlor_Welch <- oneway.test(x ~ y, data=testData, var.equal = FALSE)
  print(chlor_Welch)
  
  Dave <- chlor_means[str_detect(chlor_means$Chlorophyll, type),]
  
  graph2 <- ggplot(Dave, aes(x = Leaf_Age, y = Means)) + 
    scale_x_discrete(limits = Dave$Leaf_Age) +
    geom_bar(stat="identity", fill="grey") + theme_minimal() + lookingDave +
    geom_errorbar(aes(x = Leaf_Age, y = Means, ymin = Means-SD, ymax = Means+SD), width = 0.2)
  
  eval(parse(text = paste("graph_", gsub(" ", "_", type), " <- ggplot(Dave, aes(x = Leaf_Age, y = Means)) + 
    scale_x_discrete(limits = Dave$Leaf_Age) +
    geom_bar(stat=\"identity\", fill=\"grey\") + theme_minimal() + lookingDave +
    geom_errorbar(aes(x = Leaf_Age, y = Means, ymin = Means-SD, ymax = Means+SD), width = 0.2)", sep="")))
  
  #ggsave(paste("plots/chloro/means", type, ".png"))
  
  print("betweenPlants")
  png(paste(file="plots/chloro/histograms/betweenPlants/", type, ".png"))
  hist(as.numeric(unlist(all_chlormeans[type])))
  dev.off()
  
  print(ad.test(as.numeric(unlist(all_chlormeans[type]))))
  print(shapiro_test(as.numeric(unlist(all_chlormeans[type]))))
  
  print(oneway.test(as.numeric(unlist(chloro_combined[type]))~Plants, data = chloro_combined, var.equal = FALSE))
  #games_howell_test(combined_graph, Means~Plants, conf.level = 0.95)
  
}

install.packages("tidyverse")
library(tidyverse)
library(reshape2)
big_boy = ggarrange(graph_Chlor_A, graph_Chlor_B, graph_A_and_B, graph_Car)
annotate_figure(big_boy, left = text_grob("Chlorophyll content", rot = 90, vjust = 1, size = 8),
                bottom = text_grob("Leaf pair number", size = 8))

ggsave("plots/chloro/bigboy.png")


#write_xlsx(posthoc, "plots/leafArea/posthoc.xlsx")

titles <- chlor_means$Leaf_Age

graph2 <- ggplot(chlor_means, aes(x = Leaf_Age, y = Means)) +
  scale_x_discrete(limits = titles) +
  geom_bar(stat="identity") + theme_minimal() + lookingfine +
  labs(x = "Leaf pair number", y = "Chlorophyll content") +
  geom_errorbar(aes(x = Leaf_Age, y = Means, ymin = Means-SD, ymax = Means+SD), width = 0.2)

ggsave("plots/chloro/means_by_leaf_age.png")

