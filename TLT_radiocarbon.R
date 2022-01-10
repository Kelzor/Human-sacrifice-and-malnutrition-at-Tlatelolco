library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)

# Color blind-friendly palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load in data:
TLTrc <- read.csv("radiocarbon_data.csv")

#-------------------------------------------------------------------------------
# Figure 12.
# Ridgeline plot of radiocarbon dating probability distributions colored by 
# context.
#-------------------------------------------------------------------------------

library(Bchron)
#https://cran.r-project.org/web/packages/Bchron/vignettes/Bchron.html

TLTrcbchron = BchronCalibrate(ages=TLTrc$age_uncal,
                            ageSds=TLTrc$std, 
                            calCurves=TLTrc$calCurves, #must specify calibration curve for each date!
                            ids=TLTrc$context_ID)

#TLTrcbchron is type list of lists containing calibrated ages of sample

all_data = data.frame() #create empty dataframe

for (i in 1:length(TLTrcbchron)) {  #for each list (sample) in TLTrcbchron
  
  sample = TLTrcbchron[[i]] #sample = the ith element
  name = names(TLTrcbchron)[i] #name = name of ith object
  ageGrid = sample$ageGrid
  densities = sample[[5]] #densities = 5th item in list "sample"
  sample_data = data.frame(name, ageGrid, densities) #all values in dataframe
  all_data = bind_rows(all_data, sample_data) #append dataframe to all_data (rbind also works)
  
}

# Adding two columns to dataframe: (1) calibrated age in CE and (2) context
plotdf <- all_data %>%
  mutate(
    calage = 1950 - (ageGrid),
    context = ifelse(name == "Templo Norte 56 14D", "Grupo Norte",
                     ifelse(name == "Templo Norte 193 112", "Grupo Norte",
                            ifelse(name == "Templo Norte 189 107", "Grupo Norte",
                                   ifelse(name == "Paso a Desnivel 143 70", "Paso a Desnivel",
                                          ifelse(name == "Paso a Desnivel 137 58", "Paso a Desnivel",
                                                 ifelse(name == "Paso a Desnivel 134 53", "Paso a Desnivel", "Atenantitech"))))
                     )))


library(ggridges)

#factor by context
plotdf$context <- factor(plotdf$context, levels = c("Grupo Norte", "Paso a Desnivel", "Atenantitech"))

#plot density distributions
tiff("Fig12.tiff", units="in", width = 9, height = 5, res = 300)
ggplot(plotdf,
       aes(x = calage, y = name, height = densities, group = name, fill = context)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  scale_x_continuous(name = "Calibrated radiocarbon date range (CE)") +
  scale_y_discrete(name = "Context and skeleton ID") +
  scale_fill_manual(name = "Context", values = cbp1) +
  theme(axis.text.x = element_text(size = 15, vjust = 1),
        axis.text.y = element_text(size = 12, vjust = 1),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
dev.off()



