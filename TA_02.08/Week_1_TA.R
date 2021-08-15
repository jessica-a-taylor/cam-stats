library(ggplot2)
library(readxl)
attach(TA_data)
library(stringr)
library(nortest)
library(rstatix)
library(purrr)
library(ggforce)

wd <- getwd()
setwd(wd)
TA_data <- read_xlsx(file.choose())

MD <- TA_data[c("Leaf", "Dawn-Dusk (µmol H+ g-1 fwt)")]
lookinggood <- theme(axis.title = element_text(size = 10, color = "black"),
                     axis.text.x=element_text(colour="gray30", size = 8),
                     axis.text.y=element_text(colour="gray30", size = 8, hjust = 1),
                     strip.text = element_text(size = 8))

graph_data <- data.frame(Age = c(rep(c("Y", "M"), times=10), "ODD"),
                         Plant = c(rep(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), each=2), "ODD"),
                         Means = MD$`Dawn-Dusk (µmol H+ g-1 fwt)`)

graph_data$Age <- factor(graph_data$Age, levels = c("Y", "M", "ODD"))

TA_graph <- ggplot(graph_data, aes(x = Plant, y = Means)) +
  geom_bar(stat="identity", aes(fill=Age), position=position_dodge(0.9)) +
  theme_minimal() + scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "ODD")) +
  labs(x = "Plant ID", y = "Difference in TA between dawn and dusk (µmol H+ g-1 fwt)") +
  lookinggood + scale_fill_brewer(palette = "Blues")

ggsave(paste("TA_02.08/plots/allTA.png"))

Y_or_M <- list("Y", "M")
TA_test <- data.frame(Age = character(), TA = double())
TA_df <- data.frame(Age = character(), Mean = double(), SD = double())

for (age in Y_or_M) {
  YM_age <- MD[str_detect(MD$Leaf, age),]
  print(YM_age)
  TA_df <- rbind(TA_df, data.frame(Age = age,
                                   Mean = mean(YM_age$`Dawn-Dusk (µmol H+ g-1 fwt)`),
                                   SD = sd(YM_age$`Dawn-Dusk (µmol H+ g-1 fwt)`)))
  
  print(shapiro.test(YM_age$`Dawn-Dusk (µmol H+ g-1 fwt)`))
  print(ad.test(YM_age$`Dawn-Dusk (µmol H+ g-1 fwt)`))
  
  png(paste(file="TA_02.08/plots/hist", age, ".png"))
  hist(YM_age$`Dawn-Dusk (µmol H+ g-1 fwt)`)
  dev.off()
  
  TA_test <- rbind(TA_test, data.frame(Age = age,
                                       TA = YM_age$`Dawn-Dusk (µmol H+ g-1 fwt)`))
}


ggplot(TA_df, aes(x=Age, y=Mean)) + geom_bar(stat="identity") +
  geom_errorbar(aes(x = Age, y = Mean, ymin = Mean-SD, ymax = Mean+SD), width = 0.2) +
  labs(x = "Leaf Age", y = "Means difference in TA between 
       dawn and dusk (µmol H+ g-1 fwt)") +
  theme_minimal() + lookinggood

ggsave(paste("TA_02.08/plots/compareMeans.png"))

# Normality tests show data are not normal

oneway.test(TA~Age, data = TA_test, var.equal = FALSE)
