#read in data table
TLT<-read.csv("TLT_analysis_ready.csv", header = T)

#load required packages
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(survminer)
library(survival)
library(corrplot)

#-------------------------------------------------------------------------------
#Figure 6a.
#Survival curves of all contexts
#-------------------------------------------------------------------------------

fit <- survfit(Surv(Age) ~ Context, data = TLT)

ggsurvplot(fit,  size = 1,  # change line size
           linetype = c(1,2,3,4), # change line type
           legend.title = "Context",
           legend.labs = c("Atenantitech", "Grupo Norte", "Paso a Desnivel", "Templo R"),
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

sig <- survdiff(Surv(Age) ~ Context, data = TLT) #code to caclulate p value - not necessary
1 - pchisq(sig$chisq, length(sig$n) - 1)

#-------------------------------------------------------------------------------
#Figure 6b.
#Survival curves of ceremonial center contexts
#-------------------------------------------------------------------------------

TLTceremonial <- TLT %>%
  filter(Context != "Atenantitech")

fitC <- survfit(Surv(Age) ~ Context, data = TLTceremonial)

ggsurvplot(fitC,  size = 1,  # change line size
           linetype = c(2,3,4), # change line type
           legend.title = "Context",
           legend.labs = c("Grupo Norte", "Paso a Desnivel", "Templo-R"),
           xlab = "Years",
           ylab = "Proportion alive",
           pval.size = 3.5,
           pval.coord = c(0,.10),
           break.time.by = 10,
           palette = c("#E69F00", "#56B4E9", "#009E73"),
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = FALSE
)

sig <- survdiff(Surv(Age) ~ Context, #code to caclulate p value - not necessary
                data = subset(TLT, Context != "Atenantitech"))
1 - pchisq(sig$chisq, length(sig$n) - 1)

#-------------------------------------------------------------------------------
#Figure 6c
#Survival curves of residential and most closely related ceremonial center context
#-------------------------------------------------------------------------------

fitExp <- survfit(Surv(Age) ~ Context,
                  data = subset(TLT, Context != "Templo-R" & Context != "Grupo_Norte"))

ggsurvplot(fitExp,  size = 1,  # change line size
           linetype = c(1,3), # change line type
           legend.title = "Context",
           legend.labs = c("Atenantitech", "Paso a Desnivel"),
           xlab = "Years",
           ylab = "Proportion alive",
           pval.size = 3.5,
           pval.coord = c(0,.10),
           break.time.by = 10,
           palette = c("#999999", "#56B4E9"),
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = FALSE
)

sig <- survdiff(Surv(Age) ~ Context, #code to caclulate p value - not necessary
                data = subset(TLT, Context != "Templo-R" & Context != "Grupo_Norte"))
1 - pchisq(sig$chisq, length(sig$n) - 1)


#----------------------------------------------------------------------------
#Table 2
#Contingency tables and chi-square test results for each combination of contexts
#info on plotting residuals in R usinf corrplot
##plotting residuals  http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
#----------------------------------------------------------------------------

#--------
#create table for ChiSq and Fisher's exact test
#chisq test and residuals
#for all contexts under 20yrs
#--------
continTableAll <- TLT %>% 
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
#chisq for all contexts under under 20yrs
chiU20 <- chisq.test(continTableAll)
chiU20$observed
round(chiU20$expected, 2)
round(chiU20$residuals, 2)
corrplot(chiU20$residuals, tl.cex = 1.5, tl.col = 'black', is.cor=FALSE)
contrib <- 100*chiU20$residuals^2/chiU20$statistic
round(contrib, 3)
corrplot(contrib, col = COL1('Purples'), tl.cex = 1.5, tl.col = 'black', is.cor = FALSE)

fishCeremon <- fisher.test(continTableAll) #fisher comparison

#--------
#create table for ChiSq and Fisher's exact test
#chisq test and residuals
#for ceremonial center contexts under 20yrs
#--------
continTableCerem <- TLT %>% 
  filter(Age < 20) %>%
  mutate(Context = factor(Context, levels = c("Paso_a_desnivel", "Grupo_Norte")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", Context != "Atenantitech", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Status, values_from = count) %>%
  mutate(Unaffected = ifelse( is.na(Unaffected), 0 , Unaffected)) %>%
  column_to_rownames(., var = "Context")
#chisq for ceremonial contexts under 20 year old
chiCeremon <- chisq.test(continTableCerem) 
chiCeremon$observed
round(chiCeremon$expected,2)
round(chiCeremon$residuals, 2)
corrplot(chiCeremon$residuals, tl.cex = 1.5, tl.col = 'black', is.cor=FALSE)
contrib <- 100*chiCeremon$residuals^2/chiCeremon$statistic
round(contrib, 3)
corrplot(contrib, col = COL1('Purples'), tl.cex = 1.5, tl.col = 'black', is.cor = FALSE)

fishCeremon <- fisher.test(continTableCerem) #fisher comparison

#--------
#create table for ChiSq and Fisher's exact test
#chisq test and residuals
#for Grupo Norte and Atenantitech contexts under 20yrs
#--------
continTableGNAten <- TLT %>% 
  filter(Age < 20) %>%
  mutate(Context = factor(Context, levels = c("Grupo_Norte", "Atenantitech")),
         Metabolic = ifelse(Scurvy == 1 | Anemia == 1, 1, 0)) %>%
  filter(Context != "Ehecatl-Quetzalcoatl", Context != "Paso_a_desnivel", !is.na(Metabolic), !is.na(Infection)) %>%
  mutate(Status = ifelse(Infection == 1 & Metabolic == 0, "Infection", 
                         ifelse(Metabolic == 1 & Infection == 0, "Metabolic",
                                ifelse(Infection == 1 & Metabolic == 1, "Co-morbid","Unaffected")))) %>%
  group_by(Context, Status) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Status, values_from = count) %>%
  mutate(Unaffected = ifelse( is.na(Unaffected), 0 , Unaffected)) %>%
  column_to_rownames(., var = "Context")

#chisq for Grupo Norte and Atenantitech contexts under 20 year old
chiGNAten <- chisq.test(continTableGNAten) 
chiGNAten$observed
round(chiGNAten$expected,2)
round(chiGNAten$residuals, 2)
corrplot(chiGNAten$residuals, tl.cex = 1.5, tl.col = 'black', is.cor=FALSE)
contrib <- 100*chiGNAten$residuals^2/chiGNAten$statistic
round(contrib, 3)
corrplot(contrib, col = COL1('Purples'), tl.cex = 1.5, tl.col = 'black', is.cor = FALSE)

fishCeremon <- fisher.test(continTableGNAten) #fisher comparison
