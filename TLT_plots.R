#read in data table
TLT<-read.csv("TLT_analysis_ready.csv", header = T)

#load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

# Color blind-friendly palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#-------------------------------------------------------------------------------
#Figure 3.
#Age-at-death distribution by context. Individuals from Templo R in the 30+ age 
#category were arbitrarily assigned to this age category because their published 
#age estimates are “adult.” It is possible that some of these individuals belong 
#in the 20-30 age category, so the frequency of 30+ category may be artificially 
#inflated for Templo R. 
#-------------------------------------------------------------------------------

fig3 <- TLT %>% 
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,1,5,10,15,20,30,Inf), 
                        labels = c("0-1","1-5","5-10","10-15","15-20","20-30","30+"), 
                        right = TRUE, ordered_result = TRUE),
         Context = factor(Context, levels = c("Ehecatl-Quetzalcoatl", "Grupo_Norte","Paso_a_desnivel", "Atenantitech"))) %>%
  group_by(Context, AgeGroup) %>% #unique combinations of arguments
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

#-------------------------------------------------------------------------------
#Figure 7.
#Sex prevalence by context and age category. Sample sizes indicate the number of 
#individuals for which sex assessments were possible. Genetic sex determination 
#of individuals in the 0-16 age category was performed by De La Cruz et al. 
#(2008) (Templo R) and Morales-Arce et al. (2019) (Grupo Norte and Paso a Desnivel). 
#-------------------------------------------------------------------------------

age_variable_names <- c(
  "0-16" = "0-16 Years",
  "16+" = "16+ Years")


fig7SampleSizes <-TLT %>% #sex by age and context, 0-16, 16+
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
  summarize(total = sum(count))


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
  ggplot(., aes(x=Context)) + 
  geom_bar(stat="identity", aes(fill = Sex, y = perc)) + 
  xlab("Context") +
  ylab("Frequency") +
  facet_grid(. ~ AgeGroup, labeller = labeller(
    AgeGroup = age_variable_names
  )) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1)) +
  scale_x_discrete(labels = c("Templo R", "Paso a Desnivel", "Grupo Norte", "Atenantitech")) +
  geom_text(data = fig7SampleSizes, size = 3, aes(y = .965, label = paste(total))) +
  scale_fill_manual(values = cbp1) 

#------------------------------------------------------------------------------
#Figure 8.
#Prevalence of pathology by context across age groups. Sample size is indicated 
#for each context for each age category. Note that there is only one observable 
#individual from Atenantitech for the 10-15 year age category.
#------------------------------------------------------------------------------

#age group labels that will be printed on plot 
age_variable_names <- c(
  "0-5" = "0-5 Years",
  "5-10" = "5-10 Years",
  "10-15" = "10-15 Years",
  "15-20" = "15-20 Years",
  "20+" = "20+ Years"
)

sample_sizes <- TLT %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,5,10,15,20,Inf), 
                        labels = c("0-5","5-10","10-15","15-20","20+"),
                        right = TRUE, ordered = TRUE),
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
  summarize(total = sum(count))

fig8 <- TLT %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf,5,10,15,20,Inf), 
                        labels = c("0-5","5-10","10-15","15-20","20+")),
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
  ggplot(., aes(x=Context)) + 
  geom_bar(stat = "identity", aes(fill=Status, y=perc)) + 
  xlab("Context") +
  ylab("Frequency") +
  facet_grid(.~ AgeGroup, labeller = labeller(
    AgeGroup = age_variable_names
  )) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1)) +
  scale_x_discrete(labels = c("Paso a Desnivel", "Grupo Norte", "Atenantitech")) +
  geom_text(data = sample_sizes, size = 3, aes(y = .965, label = paste(total))) +
  scale_fill_manual(values = cbp1)

#------------------------------------------------------------------------------
#Figure 9.
#Prevalence of pathology in individuals younger than 20 years by context
#------------------------------------------------------------------------------

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

#-------------------------------------------------
#Figure 10
#Count of each type of perimortem trauma by context. Some individuals are 
#represented more than once because they had more than one type of 
#peri-postmortem trauma.
#-------------------------------------------------

fig10 <- TLT %>%
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
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_text(vjust = -.5)) +
  scale_fill_manual(name = "Perimortem trauma", labels = c("Muscle removal", "Scalping", "Dismemberment", "Heart extraction"),
                    values = cbp1) +
  scale_x_discrete(labels = c("Paso a Desnivel", "Grupo Norte", "Atenantitech"))
