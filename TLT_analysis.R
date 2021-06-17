#read in data table
TLT<-read.csv("TLT_analysis_ready.csv", header = T)

#load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbp3 <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
          "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888","#000000")

#-------------------------------------------------
#Age-at-death distribution
#-------------------------------------------------

fig3 <- TLT %>% 
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,1,5,10,15,20,30,Inf), 
                        labels = c("0-1","1-5","5-10","10-15","15-20","20-30","30+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Ehecatl-Quetzalcoatl", "Grupo_Norte","Paso_a_desnivel", "Atenantitech"))) %>%
  group_by(Context, AgeGroup) %>% #unique combos of arguments
    summarize(count = n()) %>% #creates column AND condenses data into smallest units
  group_by(Context) %>%
    mutate(
      total = sum(count),
      perc = count/total
    ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Context, y=perc, x=AgeGroup)) + 
            geom_bar(position=position_dodge(preserve = "single"), stat="identity") + 
            xlab("Age Group (years)") + ylab("Frequency (by Context)") +
            scale_fill_manual(labels = c("Templo R", "Grupo Norte", "Paso a Desnivel", "Atenantitech"),
                    values = cbp1)



#-------------------------------------------------
#Pathology frequency by context and age categories 0-5, 5-10, 10-15, 15-20, 20+
#-------------------------------------------------
labX = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")
labY = 0.95
labText = c("n = 11","n = 17","n = 7","n = 7","n = 6","n = 4","n = 8", "n = 7", "n = 1", "n = 9","n = 9","n = 4", "n=5", "n=2", "n=16")
labAgeGroup = c("0-5","0-5","0-5","5-10","5-10","5-10","10-15","10-15","10-15","15-20","15-20","15-20","20+","20+","20+")
labStatus = "Unaffected"

labels = data.frame(cbind(labX, labY, labText, labAgeGroup, labStatus))
names(labels) = c("Context", "perc", "lab", "AgeGroup", "Status")
labels$perc = as.numeric(as.character(labels$perc))

age_variable_names <- c(
  "0-5" = "0-5 Years",
  "5-10" = "5-10 Years",
  "10-15" = "10-15 Years",
  "15-20" = "15-20 Years",
  "20+" = "20+ Years"
)


fig8 <- TLT %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,5,10,15,20,Inf), 
                        labels = c("0-5","5-10","10-15","15-20","20+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  group_by(Context, AgeGroup) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, AgeGroup, Status) %>%
  summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
  mutate(
    total = sum(count),
    perc = count/total
  ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Status, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context") +
  ylab("Frequency") +
  facet_grid(. ~ AgeGroup, labeller = labeller(
    AgeGroup = age_variable_names
  )) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1)) +
  scale_x_discrete(labels = c("Paso a Desnivel", "Grupo Norte", "Atenantitech")) +
  geom_text(data = labels[1,], label = labels[1,3], size = 3) +
  geom_text(data = labels[2,], label = labels[2,3], size = 3) +
  geom_text(data = labels[3,], label = labels[3,3], size = 3) +
  geom_text(data = labels[4,], label = labels[4,3], size = 3) +
  geom_text(data = labels[5,], label = labels[5,3], size = 3) +
  geom_text(data = labels[6,], label = labels[6,3], size = 3) +
  geom_text(data = labels[7,], label = labels[7,3], size = 3) +
  geom_text(data = labels[8,], label = labels[8,3], size = 3) +
  geom_text(data = labels[9,], label = labels[9,3], size = 3) +
  geom_text(data = labels[10,], label = labels[10,3], size = 3) +
  geom_text(data = labels[11,], label = labels[11,3], size = 3) +
  geom_text(data = labels[12,], label = labels[12,3], size = 3) +
  geom_text(data = labels[13,], label = labels[13,3], size = 3) +
  geom_text(data = labels[14,], label = labels[14,3], size = 3) +
  geom_text(data = labels[15,], label = labels[15,3], size = 3) +
  scale_fill_manual(values = cbp1)


#-------------------------------------------------
#Pathology frequency by context and age categories 0-5, 5-10, 10-20 - not used in text
#-------------------------------------------------

##### long and confusing ass code to add sample size numbers for age categories under 20

labX = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")
labY = 0.95
labText = c("n = 11","n = 17","n = 7","n = 7","n = 6","n = 4","n = 16","n = 14","n = 3")
labAgeGroup = c("0-5","0-5","0-5","5-10","5-10","5-10","10-20","10-20","10-20")
labStatus = "Unaffected"

labels = data.frame(cbind(labX, labY, labText, labAgeGroup, labStatus))
names(labels) = c("Context", "perc", "lab", "AgeGroup", "Status")
labels$perc = as.numeric(as.character(labels$perc))

age_variable_names <- c(
  "0-5" = "0-5 Years",
  "5-10" = "5-10 Years",
  "10-20" = "10-20 Years"
)


figX <- TLT %>%
  filter(Age < 20) %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,5,10,20), 
                        labels = c("0-5","5-10","10-20"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  group_by(Context, AgeGroup) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                  ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                  ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, AgeGroup, Status) %>%
    summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
    mutate(
      total = sum(count),
      perc = count/total
  ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Status, y=perc, x=Context)) + 
    geom_bar(stat="identity") + 
    xlab("Context") +
    ylab("Frequency") +
    facet_grid(. ~ AgeGroup, labeller = labeller(
      AgeGroup = age_variable_names
    )) +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.text.x = element_text(vjust=1, hjust = 1)) +
    scale_x_discrete(labels = c("Paso a Desnivel", "Grupo Norte", "Atenantitech")) +
    geom_text(data = labels[1,], label = labels[1,3], size = 3) +
    geom_text(data = labels[2,], label = labels[2,3], size = 3) +
    geom_text(data = labels[3,], label = labels[3,3], size = 3) +
    geom_text(data = labels[4,], label = labels[4,3], size = 3) +
    geom_text(data = labels[5,], label = labels[5,3], size = 3) +
    geom_text(data = labels[6,], label = labels[6,3], size = 3) +
    geom_text(data = labels[7,], label = labels[7,3], size = 3) +
    geom_text(data = labels[8,], label = labels[8,3], size = 3) +
    geom_text(data = labels[9,], label = labels[9,3], size = 3) +
    scale_fill_manual(values = cbp1)

#-------------------------------------------------
#Pathology frequency by context and age categories 0-5, 5-10, 10-20, 30+ - not used in text
#-------------------------------------------------

##### long and confusing ass code to add sample size numbers for age categories 30+

labX = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")
labY = 0.95
labText = c("n = 11","n = 17","n = 7","n = 7","n = 6","n = 4","n = 17","n = 16","n = 5", "n=5", "n=2", "n=16")
labAgeGroup = c("0-5","0-5","0-5","5-10","5-10","5-10","10-20","10-20","10-20","30+", "30+", "30+")
labStatus = "Unaffected"

labels = data.frame(cbind(labX, labY, labText, labAgeGroup, labStatus))
names(labels) = c("Context", "perc", "lab", "AgeGroup", "Status")
labels$perc = as.numeric(as.character(labels$perc))

age_variable_names <- c(
  "0-5" = "0-5 Years",
  "5-10" = "5-10 Years",
  "10-20" = "10-20 Years",
  "30+" = "30+ Years"
)


figY <- TLT %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,5,10,20,Inf), 
                        labels = c("0-5","5-10","10-20","30+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  group_by(Context, AgeGroup) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, AgeGroup, Status) %>%
  summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
  mutate(
    total = sum(count),
    perc = count/total
  ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Status, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context") +
  ylab("Frequency") +
  facet_grid(. ~ AgeGroup, labeller = labeller(
    AgeGroup = age_variable_names
  )) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1)) +
  scale_x_discrete(labels = c("Paso a Desnivel", "Grupo Norte", "Atenantitech")) +
  geom_text(data = labels[1,], label = labels[1,3], size = 3) +
  geom_text(data = labels[2,], label = labels[2,3], size = 3) +
  geom_text(data = labels[3,], label = labels[3,3], size = 3) +
  geom_text(data = labels[4,], label = labels[4,3], size = 3) +
  geom_text(data = labels[5,], label = labels[5,3], size = 3) +
  geom_text(data = labels[6,], label = labels[6,3], size = 3) +
  geom_text(data = labels[7,], label = labels[7,3], size = 3) +
  geom_text(data = labels[8,], label = labels[8,3], size = 3) +
  geom_text(data = labels[9,], label = labels[9,3], size = 3) +
  geom_text(data = labels[10,], label = labels[10,3], size = 3) +
  geom_text(data = labels[11,], label = labels[11,3], size = 3) +
  geom_text(data = labels[12,], label = labels[12,3], size = 3)

###########################



#stat_summary(fun.y=min,aes(label=paste0('N=',total)),geom='text',col='blue',cex=5)
#scale_x_discrete(labels = c("Paso a Desnivel = 11", "Grupo Norte = 17", "Atenantitech = 7","Paso a Desnivel = 21", "Grupo Norte = 5", "Atenantitech = 5", "Paso a Desnivel = 12", "Grupo Norte = 7", "Atenantitech = 6"))

#-------------------------------------------------
#Pathology frequency in individuals younger than 20 years by context, age collapsed
#-------------------------------------------------

fig9 <- TLT %>%
  filter(Age < 20) %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  mutate(
    total = sum(count),
    perc = count/total
  ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Status, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context (sample size)") +
  ylab("Frequency") +
  scale_x_discrete(labels = c("Paso a Desnivel (n = 34)", "Grupo Norte (n = 37)", "Atenantitech (n = 14)")) +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_text(vjust = -.5)) +
  scale_fill_manual(values = cbp1)


fig9data <- TLT %>%
  filter(Age < 20) %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,5,10,20), 
                        labels = c("0-5","5-10","10-20"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, AgeGroup, Status) %>%
  summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
  mutate(
    total = sum(count),
    perc = count/total)


#not in text
fig9scurvy <- TLT %>%
  filter(Age < 20) %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Scurvy = ifelse(Scurvy == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Scurvy), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Scurvy == 0, "Infection", 
                         ifelse(Scurvy == 1 & Infection == 0, "Scurvy",
                                ifelse(Infection == 1 & Scurvy == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  mutate(
    total = sum(count),
    perc = count/total
  ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Status, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context (sample size)") +
  ylab("Frequency") +
  scale_x_discrete(labels = c("Paso a Desnivel (n = 34)", "Grupo Norte (n = 37)", "Atenantitech (n = 14)")) +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_text(vjust = -.5))

#not in text
fig9anemia <- TLT %>%
  filter(Age < 20) %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Scurvy = ifelse(Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Anemia), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Anemia == 0, "Infection", 
                         ifelse(Anemia == 1 & Infection == 0, "Anemia",
                                ifelse(Infection == 1 & Anemia == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  mutate(
    total = sum(count),
    perc = count/total
  ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Status, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context (sample size)") +
  ylab("Frequency") +
  scale_x_discrete(labels = c("Paso a Desnivel (n = 34)", "Grupo Norte (n = 37)", "Atenantitech (n = 14)")) +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_text(vjust = -.5))

#-------------------------------------------------
#Pathology frequency in all individuals, age collapsed - not in text
#-------------------------------------------------
figZdata <- TLT %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,5,10,20,Inf), 
                        labels = c("0-5","5-10","10-20","30+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, AgeGroup, Status) %>%
  summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
  mutate(
    total = sum(count),
    perc = count/total
  )

figZ <- TLT %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  mutate(
    total = sum(count),
    perc = count/total
  ) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Status, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  ylab("Frequency") +
  xlab("Context") +
  theme(axis.text.x = element_text(angle = 0))

#-------------------------------------------------
#Perimortem trauma count by context
#-------------------------------------------------

fig11 <- TLT %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech"))) %>%
  filter(Context != "Ehecatl-Quetzalcoatl") %>%
  group_by(Context) %>%
  summarize_at(vars(Scalping, Muscle_removal, Heart_extraction, Dismemberment), list(~ sum(., na.rm = TRUE))) %>%
  pivot_longer(!Context, names_to = "Trauma", values_to = "Count") %>%
  mutate(Trauma = factor(Trauma, levels = c("Muscle_removal", "Scalping","Dismemberment", "Heart_extraction"))) %>%
  ggplot(., mapping = aes(fill=Trauma, y=Count, x=Context)) + 
  geom_bar(stat="identity") + # position = position_dodge()) remove position argument to stack data
  xlab("Context") +
  ylab("Count") +
  # scale_x_discrete(labels = c("Paso a Desnivel (n = 34)", "Grupo Norte (n = 37)", "Atenantitech (n = 14)")) +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_text(vjust = -.5)) +
  scale_fill_manual(name = "Perimortem trauma", labels = c("Muscle removal", "Scalping", "Dismemberment", "Heart extraction"),
                    values = cbp1) +
  scale_x_discrete(labels = c("Paso a Desnivel", "Grupo Norte", "Atenantitech"))


#-------------------------------------------------
#Sex by age and context
#-------------------------------------------------


figX <- TLT %>% #sex by age with NAs - not used in text
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,10,20,Inf), 
                        labels = c("0-10","10-20","20+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Ehecatl-Quetzalcoatl", "Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Sex = factor(TLT$Sex,
                      levels = c(0,1,NA),
                      labels = c("Male", "Female"))) %>%
  group_by(Context, AgeGroup, Sex) %>%
  summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
  mutate(
    total = sum(count),
    perc = count/total) %>%
    ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Sex, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context") +
  ylab("Frequency") +
  facet_grid(. ~ AgeGroup, labeller = labeller(
    AgeGroup = age_variable_names
  )) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1))

###fig7variant sampele size added
age_variable_names <- c(
  "0-10" = "0-10 Years",
  "10-20" = "10-20 Years",
  "20+" = "20+ Years")

labX = c("Ehecatl-Quetzalcoatl", "Paso_a_desnivel", "Grupo_Norte", "Atenantitech")
labY = 0.95
labText = c("n = 19","n = 3","n = 6","n = 0","n = 2","n = 12","n = 11","n = 5","n = 4", "n=8", "n=5", "n=20")
labAgeGroup = c("0-10","0-10","0-10", "0-10", "10-20", "10-20","10-20","10-20","20+","20+","20+", "20+")
labSex = "Female"

labels = data.frame(cbind(labX, labY, labText, labAgeGroup, labSex))
names(labels) = c("Context", "perc", "lab", "AgeGroup", "Sex")
labels$perc = as.numeric(as.character(labels$perc))


figY<- TLT %>% #sex by age and context 0-10, 10-20, 20+ - not used in text
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,10,20,Inf), 
                        labels = c("0-10","10-20","20+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Ehecatl-Quetzalcoatl", "Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Sex = factor(TLT$Sex,
                      levels = c(0,1,NA),
                      labels = c("Male", "Female"))) %>%
         filter(!is.na(Sex)) %>%
  group_by(Context, AgeGroup, Sex) %>%
  summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
  mutate(
    total = sum(count),
    perc = count/total) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Sex, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context") +
  ylab("Frequency") +
  facet_grid(. ~ AgeGroup, labeller = labeller(
    AgeGroup = age_variable_names
  )) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1)) +
  scale_x_discrete(labels = c("Ehecatl-Quetzalcoatl", "Paso a Desnivel", "Grupo Norte", "Atenantitech")) +
  geom_text(data = labels[1,], label = labels[1,3], size = 3) +
  geom_text(data = labels[2,], label = labels[2,3], size = 3) +
  geom_text(data = labels[3,], label = labels[3,3], size = 3) +
  geom_text(data = labels[4,], label = labels[4,3], size = 3) +
  geom_text(data = labels[5,], label = labels[5,3], size = 3) +
  geom_text(data = labels[6,], label = labels[6,3], size = 3) +
  geom_text(data = labels[7,], label = labels[7,3], size = 3) +
  geom_text(data = labels[8,], label = labels[8,3], size = 3) +
  geom_text(data = labels[9,], label = labels[9,3], size = 3) +
  geom_text(data = labels[10,], label = labels[10,3], size = 3) +
  geom_text(data = labels[11,], label = labels[11,3], size = 3) +
  geom_text(data = labels[12,], label = labels[12,3], size = 3)


##Figure 7
age_variable_names <- c(
  "0-16" = "0-16 Years",
  "16+" = "16+ Years")

labX = c("Ehecatl-Quetzalcoatl", "Paso_a_desnivel", "Grupo_Norte", "Atenantitech")
labY = 0.95
labText = c("n = 21","n = 7","n = 10","n = 0","n = 4","n = 16","n = 12","n = 25")
labAgeGroup = c("0-16","0-16","0-16", "0-16", "16+", "16+","16+","16+")
labSex = "Female"

labels = data.frame(cbind(labX, labY, labText, labAgeGroup, labSex))
names(labels) = c("Context", "perc", "lab", "AgeGroup", "Sex")
labels$perc = as.numeric(as.character(labels$perc))

fig7 <- TLT %>% #sex by age and context, 0-16, 16+
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,16,Inf), 
                        labels = c("0-16","16+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Ehecatl-Quetzalcoatl", "Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Sex = factor(TLT$Sex,
                      levels = c(0,1,NA),
                      labels = c("Male", "Female"))) %>%
  filter(!is.na(Sex)) %>%
  group_by(Context, AgeGroup, Sex) %>%
  summarize(count = n()) %>%
  group_by(Context, AgeGroup) %>%
  mutate(
    total = sum(count),
    perc = count/total) %>%
  ungroup(.) %>%
  ggplot(data, mapping = aes(fill=Sex, y=perc, x=Context)) + 
  geom_bar(stat="identity") + 
  xlab("Context") +
  ylab("Frequency") +
  facet_grid(. ~ AgeGroup, labeller = labeller(
    AgeGroup = age_variable_names
  )) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1)) +
  scale_x_discrete(labels = c("Templo R", "Paso a Desnivel", "Grupo Norte", "Atenantitech")) +
  geom_text(data = labels[1,], label = labels[1,3], size = 3) +
  geom_text(data = labels[2,], label = labels[2,3], size = 3) +
  geom_text(data = labels[3,], label = labels[3,3], size = 3) +
  geom_text(data = labels[4,], label = labels[4,3], size = 3) +
  geom_text(data = labels[5,], label = labels[5,3], size = 3) +
  geom_text(data = labels[6,], label = labels[6,3], size = 3) +
  geom_text(data = labels[7,], label = labels[7,3], size = 3) +
  geom_text(data = labels[8,], label = labels[8,3], size = 3) +
  scale_fill_manual(values = cbp1) 
 

#-------------------------------------------------
#Summary stats manual and automated
#-------------------------------------------------
#Table of under 20s
Co-morbid;Infection;Metabolic;Unaffected
Paso;22;4;8;0
Grupo;16;1;12;8
Atenantitech;2;1;3;8
###############

#Summary tables


SumAll <- TLT %>%
  group_by(Context) %>%
  summarise_at(vars(Age), list( ~ n(), ~ median(., na.rm = TRUE), ~ mean(., na.rm = TRUE),
                                    ~ sd(., na.rm = TRUE), ~ min(., na.rm = TRUE), 
                                    ~ max(., na.rm = TRUE)))

Sum20 <- TLT %>%
  filter(Age < 20) %>%
  group_by(Context) %>%
  summarise_at(vars(Age), list( ~ n(), ~ median(., na.rm = TRUE), ~ mean(., na.rm = TRUE),
                                ~ sd(., na.rm = TRUE), ~ min(., na.rm = TRUE), 
                                ~ max(., na.rm = TRUE)))

SumPath20 <- TLT %>%
  filter(Age < 20) %>%
  mutate(Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(!is.na(Metabolic), !is.na(Infection)) %>%
  group_by(Context) %>%
  summarise_at(vars(Age), list( ~ n(), ~ median(., na.rm = TRUE), ~ mean(., na.rm = TRUE),
                                ~ sd(., na.rm = TRUE), ~ min(., na.rm = TRUE), 
                                ~ max(., na.rm = TRUE)))

SumPath <- TLT %>%
  mutate(Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(!is.na(Metabolic), !is.na(Infection)) %>%
  group_by(Context) %>%
  summarise_at(vars(Age), list( ~ n(), ~ median(., na.rm = TRUE), ~ mean(., na.rm = TRUE),
                                ~ sd(., na.rm = TRUE), ~ min(., na.rm = TRUE), 
                                ~ max(., na.rm = TRUE)))

#-------------------------------------------------
#Statistical analyses - Kruskal-Wallis
#-------------------------------------------------

#Kruskal-Wallis
#http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
#http://courses.atlas.illinois.edu/spring2016/STAT/STAT200/RProgramming/NonParametricStats.html

shapiro.test(TLT$Age)

kruskal.test(Age ~ Context, data = TLT)
pairwise.wilcox.test(TLT$Age, TLT$Context, p.adjust.method = "BH")
pairwise.wilcox.test(TLT$Age, TLT$Context, p.adjust.method = "BH", conf.int = TRUE)
pairwise.wilcox.test(TLT$Age, TLT$Context, p.adjust.method = "BH", correct = FALSE)

dunn.test(TLT$Age, TLT$Context, method= "bh", altp = TRUE)
dunn.test(TLT$Age, TLT$Context, altp = TRUE)


#Kruskal-Wallis may not be appropriate here bc there are some ties in rank (same values)


#-------------------------------------------------
#Statistical analyses - Survival curves
#-------------------------------------------------

#Survivial curves http://www.sthda.com/english/wiki/survminer-r-package-survival-data-analysis-and-visualization
#http://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html
#load required libraries
library(survminer)
library(survival)

#the following tutorial from https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
#The survfit function creates survival curves based on a formula. 
#Let’s generate the overall survival curve for the entire cohort, 
#assign it to object fit, and look at the names of that object:

fit <- survfit(Surv(Age) ~ Context,
               data = TLT)
ggsurvplot(fit, risk.table = TRUE)
names(fit)

#Some key components of this survfit object that will be used to create survival curves include:
#time, which contains the start and endpoints of each time interval
#surv, which contains the survival probability corresponding to each time
#We can conduct between-group significance tests using a log-rank test
#The log-rank test equally weights observations over the entire follow-up time 
#and is the most common way to compare survival times between groups
#There are versions that more heavily weight the early or late follow-up 
#that could be more appropriate depending on the research question 
#(see ?survdiff for different test options)
#We get the log-rank p-value using the survdiff function. 
#For example, we can test whether there was a difference in survival 
#time according to sex in the lung data
#survdiff(Surv(time, status) ~ sex, data = lung)

survdiff(formula = Surv(Age) ~ Context, data = TLT)

sig <- survdiff(Surv(Age) ~ Context, data = TLT)
1 - pchisq(sig$chisq, length(sig$n) - 1)

############### End tutorial ############

##Fig. 4 - survivial curve of all contexts

fit <- survfit(Surv(Age) ~ Context, data = TLT)

ggsurvplot(fit,  size = 1,  # change line size
           linetype = "strata", # change line type by groups,
           legend.title = "Context",
           legend.labs = c("Atenantitech", "Templo R", "Grupo Norte", "Paso a Desnivel"),
           xlab = "Years",
           ylab = "Proportion alive",
           pval.size = 3.5,
           pval.coord = c(0,.10),
           break.time.by = 10,
           palette = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = FALSE
)

sig <- survdiff(Surv(Age) ~ Context, data = TLT)
1 - pchisq(sig$chisq, length(sig$n) - 1)

##Fig. 5 - survivial curve of ceremonial center contexts 

TLTceremonial <- TLT %>%
  filter(Context != "Atenantitech")

fitC <- survfit(Surv(Age) ~ Context, data = TLTceremonial)

ggsurvplot(fitC,  size = 1,  # change line size
           linetype = "strata", # change line type by groups,
           legend.title = "Context",
           legend.labs = c("Templo R", "Grupo Norte", "Paso a Desnivel"),
           xlab = "Years",
           ylab = "Proportion alive",
           pval.size = 3.5,
           pval.coord = c(0,.10),
           break.time.by = 10,
           palette = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = FALSE
)


survdiff(formula = Surv(Age) ~ Context, 
         data = subset(TLT, Context != "Atenantitech"))

sig <- survdiff(Surv(Age) ~ Context, 
                data = subset(TLT, Context != "Atenantitech"))
1 - pchisq(sig$chisq, length(sig$n) - 1)

##Fig. 6 - survivial curve of residential and most closely related ceremonial context

fitExp <- survfit(Surv(Age) ~ Context,
                data = subset(TLT, Context != "Ehecatl-Quetzalcoatl" & Context != "Grupo_Norte"))

ggsurvplot(fitExp,  size = 1,  # change line size
           linetype = "strata", # change line type by groups,
           legend.title = "Context",
           legend.labs = c("Atenantitech", "Paso a Desnivel"),
           xlab = "Years",
           ylab = "Proportion alive",
           pval.size = 3.5,
           pval.coord = c(0,.10),
           break.time.by = 10,
           palette = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = FALSE
)


survdiff(formula = Surv(Age) ~ Context, 
         data = subset(TLT, Context != "Ehecatl-Quetzalcoatl" & Context != "Grupo_Norte"))

sig <- survdiff(Surv(Age) ~ Context, 
                data = subset(TLT, Context != "Ehecatl-Quetzalcoatl" & Context != "Grupo_Norte"))
1 - pchisq(sig$chisq, length(sig$n) - 1)

###unused plot for Atenantitech and Grupo Norte

fitExp <- survfit(Surv(Age) ~ Context,
                  data = subset(TLT, Context != "Paso_a_desnivel" & Context != "Ehecatl-Quetzalcoatl"))
ggsurvplot(fitExp, risk.table = TRUE, pval = TRUE)

#-------------------------------------------------
#Statistical analyses - Contingency tables, chi-sq, Fisher's exact
#-------------------------------------------------

#create table for ChiSq and Fisher's exact test
continTable <- TLT %>% 
  filter(Age < 20) %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Status, values_from = count) %>%
  mutate(Unaffected = ifelse( is.na(Unaffected), 0 , Unaffected)) %>%
  column_to_rownames(., var = "Context")



continTableallAges <- TLT %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Status, values_from = count) %>%
  mutate(Unaffected = ifelse( is.na(Unaffected), 0 , Unaffected)) %>%
  column_to_rownames(., var = "Context")


#plotting residuals  http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

library(corrplot)

#####
#Yates continuity correction doesnt work for >2x2 tables
#If the chi-squared test concludes that there is significant association, 
#we may want to know if there is any significant difference in three compared pairs, 
#between control and experiment 1, between control and experiment 2, 
#and between experiment 1 and experiment 2. We can reduce the table into 
#multiple 2 × 2 contingency tables and perform the chi-squared test 
#with applying the Bonferroni corrected alpha level 
#(corrected α = 0.05/3 compared pairs = 0.017).
#####

#chisq for all contexts under 20 year old
chiU20 <- chisq.test(continTable)
chiU20$observed
round(chiU20$expected, 2)
round(chiU20$residuals, 2)
corrplot(chiU20$residuals, is.cor=FALSE)
contrib <- 100*chiU20$residuals^2/chiU20$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

#chisq for all contexts all ages
chiAll <- chisq.test(continTableallAges)
chiAll$observed
round(chiAll$expected, 2)
round(chiAll$residuals, 2)
corrplot(chiAll$residuals, is.cor=FALSE)
contrib <- 100*chiAll$residuals^2/chiAll$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

#fisher test comparison
fisher.test(continTableallAges)

#chisq for ceremonial center contexts, less than 20
Ceremon <- continTable[-3,]
fishCeremon <- fisher.test(Ceremon) #fisher comparison
chiCeremon <- chisq.test(Ceremon) 
chiCeremon$observed
round(chiCeremon$expected,2)
round(chiCeremon$residuals, 2)
corrplot(chiCeremon$residuals, is.cor=FALSE)
contrib <- 100*chiCeremon$residuals^2/chiCeremon$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

#chisq for ceremonial center contexts, all ages
CeremonAll <- continTableallAges[-3,]
#Ceremon <- continTableallAges[-3,]
fishCeremonAll <- fisher.test(CeremonAll)
chiCeremonAll <- chisq.test(CeremonAll) #
chiCeremonAll$observed
round(chiCeremonAll$expected,2)
round(chiCeremonAll$residuals, 2)
corrplot(chiCeremonAll$residuals, is.cor=FALSE)
contrib <- 100*chiCeremonAll$residuals^2/chiCeremonAll$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

#chisq for Atenantitech and Grupo Norte less than 20 (more similar)
AtenGr <- continTable[-1,]
chiAtenGr <- chisq.test(AtenGr) 
fishAtenGr <- fisher.test(AtenGr)
chiAtenGr$observed
round(chiAtenGr$expected,2)
round(chiAtenGr$residuals, 2)
corrplot(chiAtenGr$residuals, is.cor=FALSE)
contrib <- 100*chiAtenGr$residuals^2/chiAtenGr$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

#chisq for Atenantitech and Grupo Norte all ages (more similar)
AtenGrAll <- continTableallAges[-1,]
chiAtenGrAll <- chisq.test(AtenGrAll) 
fishAtenGrAll <- fisher.test(chiAtenGrAll)
chiAtenGrAll$observed
round(chiAtenGrAll$expected,2)
round(chiAtenGrAll$residuals, 2)
corrplot(chiAtenGrAll$residuals, is.cor=FALSE)
contrib <- 100*chiAtenGrAll$residuals^2/chiAtenGrAll$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

###



