library(readxl)
Df <- read_excel("DPHT_Dataset.xlsx", 
                 skip = 1)
View(Df)
attach(Df)

library(tidyverse) ## Data manipulation and visualization
library(ggpubr) ##p values on plots
library(psych) ## Quick descriptive data summary
library(lme4) ## ANOVAS or linear mixed effects models
library(lmerTest) ## Anovas within LME
library(rstatix) ## Shapiro Wilk and Cohen's D

summary(Df)

Df$Period <- as.factor(Df$Period)
Df$Sex <- as.factor(Df$Sex)
Df$Class <- as.factor(Df$Class)

######################### ABSTRACT 1 #######################

## Comparing 2024s 1st year to 2023s 2nd year

## reduce to 1st year of 24s
Df_24_1st <- Df %>% filter(Class == "24")

## Re-code to PRE and POST
Df_24_1st$Period    <- recode_factor(Df_24_1st$Period   , a = "Pre", 
                                b = "Post")
## reduce to 2nd year of 23s
Df_23_2nd <- Df %>% filter(Class == "23") %>% 
  filter(Period == "b" | Period == "c")
## Re-code to PRE and POST
Df_23_2nd$Period    <- recode_factor(Df_23_2nd$Period   , b = "Pre", 
                                     c = "Post")
#combine data sets
Df_abstract1 <- merge(Df_24_1st, Df_23_2nd, all = TRUE) 

############VO2 Max

## Quick visualization
Df_abstract1 %>% ggplot(aes(x = Class, y=VO2)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract1 %>% anova_test(dv = VO2, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_1 <- Df_abstract1 %>%  group_by(Class)%>%  
  pairwise_t_test(VO2 ~ Period, paired = T,
                               p.adjust.method	= "none")

# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Df_abstract1 %>% group_by(Class)%>% cohens_d(VO2 ~ Period,
                          paired = TRUE, hedges.correction = TRUE) 

##Plot
pwc_1 <- pwc_1 %>% add_xy_position(x = "Class")
VO2_plot_A1 <- Df_abstract1 %>% ggplot(aes(x = Class, y=VO2)) + 
  labs(y="VO2max (mL/kg/min)") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_1, hide.ns = T)
VO2_plot_A1
ggsave("VO2_plot_A1.png")

 
################# FAT MASS

## Quick visualization
Df_abstract1 %>% ggplot(aes(x = Class, y=Fat_mass)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova fat mass
Df_abstract1 %>% anova_test(dv = Fat_mass, wid = ID,
                            within = c(Period), between=c(Class))

############# Lean MASS
## Quick visualization
Df_abstract1 %>% ggplot(aes(x = Class, y=Lean_mass)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard Anova 
Df_abstract1 %>% anova_test(dv = Lean_mass, wid = ID,
                            within = c(Period), between=c(Class))

################# VOP endothelial  Function
## Quick visualization
Df_abstract1 %>% ggplot(aes(x = Class, y=Endo_function)) + 
  labs(y="Endothelial Function") +
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova
Df_abstract1 %>% anova_test(dv = Endo_function, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract1 %>%  group_by(Class)%>%  
  pairwise_t_test(Endo_function ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract1 %>% group_by(Class)%>% 
  cohens_d(Endo_function ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Class")
Endo_function_plot_A1 <- Df_abstract1 %>%
  ggplot(aes(x = Class, y=Endo_function))  + 
  labs(y="Endothelial Function") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
Endo_function_plot_A1
ggsave("Endo_function_plot_A1.png")

################ VOP average
## Quick visualization
Df_abstract1 %>% ggplot(aes(x = Class, y=VOP_Average)) + 
  labs(y="VOP_Average") +
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova
Df_abstract1 %>% anova_test(dv = VOP_Average, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract1 %>%  group_by(Class)%>%  
  pairwise_t_test(VOP_Average ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract1 %>% group_by(Class)%>% 
  cohens_d(VOP_Average ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Class")
VOP_average_plot_A1 <- Df_abstract1 %>%
  ggplot(aes(x = Class, y=VOP_Average))  + 
  labs(y="VOP average") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
VOP_average_plot_A1
ggsave("VOP_average_plot_A1.png")

################ VOP peak
## Quick visualization
Df_abstract1 %>% ggplot(aes(x = Class, y=VOP_Peak)) + 
  labs(y="VOP_Peak") +
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova
Df_abstract1 %>% anova_test(dv = VOP_Peak, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract1 %>%  group_by(Class)%>%  
  pairwise_t_test(VOP_Peak ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract1 %>% group_by(Class)%>% 
  cohens_d(VOP_Peak ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Class")
VOP_peak_plot_A1 <- Df_abstract1 %>%
  ggplot(aes(x = Class, y=VOP_Peak))  + 
  labs(y="VOP Peak") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
VOP_peak_plot_A1
ggsave("VOP_peak_plot_A1.png")


# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_3 <- Df_abstract1 %>%  group_by(Class)%>%  
  pairwise_t_test(VOP_Peak ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_3
# Effect size Cohen's D with Hedge's g correction for small sample size
effect3 <- Df_abstract1 %>% group_by(Class)%>% 
  cohens_d(VOP_Peak ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect3

##  visualization
pwc_3 <- pwc_3 %>% add_xy_position(x = "Class")
VOP_Peak_plot_A1 <- Df_abstract1 %>% ggplot(aes(x = Class, y=VOP_Peak)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_3, hide.ns = T)
VOP_Peak_plot_A1
ggsave("VOP_Peak_plot_A1.png")

############## PSS_Score 
Df_abstract1 %>% ggplot(aes(x = Class, y=PSS_Score )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = PSS_Score, wid = ID,
                            within = c(Period), between=c(Class))

############ IPAQ_Score 
Df_abstract1 %>% ggplot(aes(x = Class, y= Total_Mets )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Total_Mets, wid = ID,
                            within = c(Period), between=c(Class))


############ IPAQ_Score 
Df_abstract1 %>% ggplot(aes(x = Class, y=Sitting_Mins )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Sitting_Mins, wid = ID,
                            within = c(Period), between=c(Class))

############PWV
Df_abstract1 %>% ggplot(aes(x = Class, y=PWV_Average )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = PWV_Average, wid = ID,
                            within = c(Period), between=c(Class))


############PWV
Df_abstract1 %>% ggplot(aes(x = Class, y=PWV_Average )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = PWV_Average, wid = ID,
                            within = c(Period), between=c(Class))


############PWA Aux 
Df_abstract1 %>% ggplot(aes(x = Class, y=C_Aix_avg )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = C_Aix_avg, wid = ID,
                            within = c(Period), between=c(Class))

############PWV
Df_abstract1 %>% ggplot(aes(x = Class, y=C_Aix75_avg )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = C_Aix75_avg, wid = ID,
                            within = c(Period), between=c(Class))

############Dairy
Df_abstract1 %>% ggplot(aes(x = Class, y=Dairy )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Dairy, wid = ID,
                            within = c(Period), between=c(Class))


############FruitsVeg_w_legumesFF 
Df_abstract1 %>% ggplot(aes(x = Class, y=FruitsVeg_w_legumesFF  )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = FruitsVeg_w_legumesFF , wid = ID,
                            within = c(Period), between=c(Class))

############FruitsVeg_w_legumes_w_o_FF  
Df_abstract1 %>% ggplot(aes(x = Class, y=FruitsVeg_w_legumes_w_o_FF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = FruitsVeg_w_legumes_w_o_FF  , wid = ID,
                            within = c(Period), between=c(Class))

############Veg_w_legumesFF  
Df_abstract1 %>% ggplot(aes(x = Class, y=Veg_w_legumesFF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Veg_w_legumesFF  , wid = ID,
                            within = c(Period), between=c(Class))


############Veg_w_legumes_w_o_FF  
Df_abstract1 %>% ggplot(aes(x = Class, y=Veg_w_legumes_w_o_FF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Veg_w_legumes_w_o_FF  , wid = ID,
                            within = c(Period), between=c(Class))


############Fruit  
Df_abstract1 %>% ggplot(aes(x = Class, y=Fruit   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Fruit  , wid = ID,
                            within = c(Period), between=c(Class))

############Sweetened_Beverages   
Df_abstract1 %>% ggplot(aes(x = Class, y=Sweetened_Beverages    )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Sweetened_Beverages   , wid = ID,
                            within = c(Period), between=c(Class))


############Whole_Grains   
Df_abstract1 %>% ggplot(aes(x = Class, y=Whole_Grains)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Whole_Grains, wid = ID,
                            within = c(Period), between=c(Class))


############Added_Sugar   
Df_abstract1 %>% ggplot(aes(x = Class, y=Added_Sugar)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Added_Sugar, wid = ID,
                            within = c(Period), between=c(Class))


############Fiber   
Df_abstract1 %>% ggplot(aes(x = Class, y=Fiber)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Fiber, wid = ID,
                            within = c(Period), between=c(Class))

############Calcium   
Df_abstract1 %>% ggplot(aes(x = Class, y=Calcium)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract1 %>% anova_test(dv = Calcium, wid = ID,
                            within = c(Period), between=c(Class))


############################### ABSTRACT 2##############################
## Comparing 2023s data only

## reduce to 2nd year of 23s
Df_abstract2 <- Df %>% filter(Class == "23") 

############VO2 Max

## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=VO2)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = VO2, wid = ID,
                            within = c(Period))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_1 <- Df_abstract2 %>% 
  pairwise_t_test(VO2 ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_1
# Effect size Cohen's D with Hedge's g correction for small sample size
effect_1 <- Df_abstract2 %>% group_by(Class)%>% cohens_d(VO2 ~ Period,
                                  paired = TRUE, hedges.correction = TRUE) 
effect_1
##Plot
pwc_1 <- pwc_1 %>% add_xy_position(x = "Period")
VO2_plot_A2 <- Df_abstract2 %>% ggplot(aes(x = Period, y=VO2)) + 
  labs(y="VO2max (mL/kg/min)") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_1, hide.ns = T)
VO2_plot_A2
ggsave("VO2_plot_A2.png")

############DEXA
###Fat mass
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=Fat_mass)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = Fat_mass, wid = ID,
                            within = c(Period))

###lean mass
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=Lean_mass)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = Lean_mass, wid = ID,
                            within = c(Period))

## Endothelial 
Df_abstract2 %>% ggplot(aes(x = Period, y=Endo_function)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = Endo_function, wid = ID,
                            within = c(Period))
# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract2 %>% 
  pairwise_t_test(Endo_function ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect_2 <- Df_abstract2 %>% group_by(Class)%>% 
  cohens_d(Endo_function ~ Period,
    paired = TRUE, hedges.correction = TRUE) 
effect_2
##Plot
pwc_2 <- pwc_2 %>% add_xy_position(x = "Period")
Endo_function_plot_A2 <- Df_abstract2 %>%
  ggplot(aes(x = Period, y=Endo_function)) + 
  labs(y="Endothelial Function") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
Endo_function_plot_A2
ggsave("Endo_function_plot_A2.png")


################ VOP average
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=VOP_Average)) + 
  labs(y="VOP_Average") +
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova
Df_abstract2 %>% anova_test(dv = VOP_Average, wid = ID,
                            within = c(Period))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract2 %>% 
  pairwise_t_test(VOP_Average ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract2 %>% 
  cohens_d(VOP_Average ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Period")
VOP_average_plot_A2 <- Df_abstract2 %>%
  ggplot(aes(x = Period, y=VOP_Average))  + 
  labs(y="VOP average") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
VOP_average_plot_A2
ggsave("VOP_average_plot_A2.png")

################ VOP peak
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=VOP_Peak)) + 
  labs(y="VOP_Peak") +
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova
Df_abstract2 %>% anova_test(dv = VOP_Peak, wid = ID,
                            within = c(Period))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract2 %>%   
  pairwise_t_test(VOP_Peak ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract2 %>% 
  cohens_d(VOP_Peak ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Period")
VOP_peak_plot_A2 <- Df_abstract2 %>%
  ggplot(aes(x = Period, y=VOP_Peak))  + 
  labs(y="VOP Peak") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
VOP_peak_plot_A2
ggsave("VOP_peak_plot_A2.png")


# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_3 <- Df_abstract2 %>%  group_by(Class)%>%  
  pairwise_t_test(VOP_Peak ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_3
# Effect size Cohen's D with Hedge's g correction for small sample size
effect3 <- Df_abstract2 %>% group_by(Class)%>% 
  cohens_d(VOP_Peak ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect3

##  visualization
pwc_3 <- pwc_3 %>% add_xy_position(x = "Class")
VOP_Peak_plot_A1 <- Df_abstract2 %>% ggplot(aes(x = Class, y=VOP_Peak)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_3, hide.ns = T)
VOP_Peak_plot_A1
ggsave("VOP_Peak_plot_A1.png")

##MENTAL PSS Score
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=PSS_Score)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = PSS_Score, wid = ID,
                            within = c(Period))

##0 Total mets
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=Total_Mets)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = Sitting_Mins, wid = ID,
                            within = c(Period))

##0 Total mets
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=Sitting_Mins)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = Total_Mets, wid = ID,
                            within = c(Period))


## Total sitting
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=Sitting_Mins)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = Sitting_Mins, wid = ID,
                            within = c(Period))

## PWV
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=PWV_Average)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = PWV_Average, wid = ID,
                            within = c(Period))

## PWA aix
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=C_Aix_avg)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = C_Aix_avg, wid = ID,
                            within = c(Period))

##PWA aix75
## Quick visualization
Df_abstract2 %>% ggplot(aes(x = Period, y=C_Aix75_avg)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract2 %>% anova_test(dv = C_Aix75_avg, wid = ID,
                            within = c(Period))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_3 <- Df_abstract2 %>% 
  pairwise_t_test(C_Aix75_avg ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_3
# Effect size Cohen's D with Hedge's g correction for small sample size
effect_3 <- Df_abstract2 %>% group_by(Class)%>% 
  cohens_d(C_Aix75_avg ~ Period,
           paired = TRUE, hedges.correction = TRUE) 
effect_3
##Plot
pwc_3 <- pwc_3 %>% add_xy_position(x = "Period")
Aix75_plot_A2 <- Df_abstract2 %>%
  ggplot(aes(x = Period, y=C_Aix75_avg)) + 
  labs(y="Augmentation Index 75") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_3, hide.ns = T)
Aix75_plot_A2
ggsave("Aix75_plot_A2.png")

############Dairy
Df_abstract2 %>% ggplot(aes(x = Period, y=Dairy )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Dairy, wid = ID,
                            within = c(Period))


############FruitsVeg_w_legumesFF 
Df_abstract2 %>% ggplot(aes(x = Period, y=FruitsVeg_w_legumesFF  )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = FruitsVeg_w_legumesFF , wid = ID,
                            within = c(Period))

############FruitsVeg_w_legumes_w_o_FF  
Df_abstract2 %>% ggplot(aes(x = Period, y=FruitsVeg_w_legumes_w_o_FF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = FruitsVeg_w_legumes_w_o_FF  , wid = ID,
                            within = c(Period))

############Veg_w_legumesFF  
Df_abstract2 %>% ggplot(aes(x = Period, y=Veg_w_legumesFF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Veg_w_legumesFF  , wid = ID,
                            within = c(Period))


############Veg_w_legumes_w_o_FF  
Df_abstract2 %>% ggplot(aes(x = Period, y=Veg_w_legumes_w_o_FF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Veg_w_legumes_w_o_FF  , wid = ID,
                            within = c(Period))


############Fruit  
Df_abstract2 %>% ggplot(aes(x = Period, y=Fruit   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Fruit  , wid = ID,
                            within = c(Period))

############Sweetened_Beverages   
Df_abstract2 %>% ggplot(aes(x = Period, y=Sweetened_Beverages    )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Sweetened_Beverages   , wid = ID,
                            within = c(Period))


############Whole_Grains   
Df_abstract2 %>% ggplot(aes(x = Period, y=Whole_Grains)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Whole_Grains, wid = ID,
                            within = c(Period))

############Added_Sugar   
Df_abstract2 %>% ggplot(aes(x = Period, y=Added_Sugar)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Added_Sugar, wid = ID,
                            within = c(Period))

############Fiber   
Df_abstract2 %>% ggplot(aes(x = Period, y=Fiber)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Fiber, wid = ID,
                            within = c(Period))

############Calcium   
Df_abstract2 %>% ggplot(aes(x = Period, y=Calcium)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract2 %>% anova_test(dv = Calcium, wid = ID,
                            within = c(Period))
################################ ABSTRACT 3######################################
## Comparing 2024s 1st year to 2023s 2nd year

## reduce to 1st year of 24s
Df_24_1st <- Df %>% filter(Class == "24")

## reduce to 2nd year of 23s
Df_23_1st <- Df %>% filter(Class == "23") %>% 
  filter(Period == "a" | Period == "b")

#combine data sets
Df_abstract3 <- merge(Df_24_1st, Df_23_1st, all = TRUE) 


############VO2 Max

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=VO2)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = VO2, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_1 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(VO2 ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_1
# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Df_abstract3 %>% group_by(Class)%>% cohens_d(VO2 ~ Period,
                             paired = TRUE, hedges.correction = TRUE) 
effect

##Plot
pwc_1 <- pwc_1 %>% add_xy_position(x = "Class")
VO2_plot_A3 <- Df_abstract3 %>% ggplot(aes(x = Class, y=VO2)) + 
  labs(y="VO2max (mL/kg/min)") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_1, hide.ns = T)
VO2_plot_A3
ggsave("VO2_plot_A3.png")

############ Fat mass

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=Fat_mass)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = Fat_mass, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(Fat_mass ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2

#####LEAN MASS
# Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=Lean_mass)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = Lean_mass, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_3 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(Lean_mass ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_3


############PSS

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=PSS_Score)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = PSS_Score, wid = ID,
                            within = c(Period), between=c(Class))

## TOtal METS

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=Total_Mets)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = Total_Mets, wid = ID,
                            within = c(Period), between=c(Class))


## Total Sitting time

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=Sitting_Mins)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = Sitting_Mins, wid = ID,
                            within = c(Period), between=c(Class))


## Total PWV

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=PWV_Average)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = PWV_Average, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_4 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(PWV_Average ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_4
# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Df_abstract3 %>% group_by(Class)%>% cohens_d(PWV_Average ~ Period,
                                                       paired = TRUE, hedges.correction = TRUE) 
effect

##Plot
pwc_4 <- pwc_4 %>% add_xy_position(x = "Class")
PWV_plot_A3 <- Df_abstract3 %>% ggplot(aes(x = Class, y=PWV_Average)) + 
  labs(y="Pulse Wave Velocity") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_4, hide.ns = T)
PWV_plot_A3
ggsave("PWV_plot_A3.png")


##arterial stiffness

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=C_Aix75_avg)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = C_Aix75_avg, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_6 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(C_Aix75_avg ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_6
# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Df_abstract3 %>% group_by(Class)%>% cohens_d(Endo_function ~ Period,
                                                       paired = TRUE, hedges.correction = TRUE) 
effect

##Plot
pwc_6 <- pwc_6 %>% add_xy_position(x = "Class")
Aix75_plot_A3 <- Df_abstract3 %>% 
  ggplot(aes(x = Class, y=C_Aix75_avg)) + 
  labs(y="Augmentation Index 75") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_6, hide.ns = T)
Aix75_plot_A3 
ggsave("Aix75_plot_A3.png")

##  Endothelial Function

## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=Endo_function)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

# Standard 2x2 ANOVA for VO2 Max
Df_abstract3 %>% anova_test(dv = Endo_function, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_5 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(Endo_function ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_5
# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Df_abstract3 %>% group_by(Class)%>% cohens_d(Endo_function ~ Period,
                                                       paired = TRUE, hedges.correction = TRUE) 
effect

##Plot
pwc_5 <- pwc_5 %>% add_xy_position(x = "Class")
Endo_function_plot_A3 <- Df_abstract3 %>% 
  ggplot(aes(x = Class, y=Endo_function)) + 
  labs(y="Endothelial Function") +   
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_5, hide.ns = T)
Endo_function_plot_A3
ggsave("Endo_function_plot_A3.png")


################ VOP average
## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=VOP_Average)) + 
  labs(y="VOP_Average") +
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova
Df_abstract3 %>% anova_test(dv = VOP_Average, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(VOP_Average ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract3 %>% group_by(Class)%>% 
  cohens_d(VOP_Average ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Class")
VOP_average_plot_A3 <- Df_abstract3 %>%
  ggplot(aes(x = Class, y=VOP_Average))  + 
  labs(y="VOP average") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
VOP_average_plot_A3
ggsave("VOP_average_plot_A3.png")

################ VOP peak
## Quick visualization
Df_abstract3 %>% ggplot(aes(x = Class, y=VOP_Peak)) + 
  labs(y="VOP_Peak") +
  geom_boxplot(aes(fill=Period)) + theme_bw()

## Standard Anova
Df_abstract3 %>% anova_test(dv = VOP_Peak, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(VOP_Peak ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract3 %>% group_by(Class)%>% 
  cohens_d(VOP_Peak ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Class")
VOP_peak_plot_A3 <- Df_abstract3 %>%
  ggplot(aes(x = Class, y=VOP_Peak))  + 
  labs(y="VOP Peak") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
VOP_peak_plot_A3
ggsave("VOP_peak_plot_A3.png")


############Dairy
Df_abstract3 %>% ggplot(aes(x = Class, y=Dairy )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Dairy, wid = ID,
                            within = c(Period), between=c(Class))


############FruitsVeg_w_legumesFF 
Df_abstract3 %>% ggplot(aes(x = Class, y=FruitsVeg_w_legumesFF  )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = FruitsVeg_w_legumesFF , wid = ID,
                            within = c(Period), between=c(Class))

############FruitsVeg_w_legumes_w_o_FF  
Df_abstract3 %>% ggplot(aes(x = Class, y=FruitsVeg_w_legumes_w_o_FF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = FruitsVeg_w_legumes_w_o_FF  , wid = ID,
                            within = c(Period), between=c(Class))

############Veg_w_legumesFF  
Df_abstract3 %>% ggplot(aes(x = Class, y=Veg_w_legumesFF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Veg_w_legumesFF  , wid = ID,
                            within = c(Period), between=c(Class))


############Veg_w_legumes_w_o_FF  
Df_abstract3 %>% ggplot(aes(x = Class, y=Veg_w_legumes_w_o_FF   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Veg_w_legumes_w_o_FF  , wid = ID,
                            within = c(Period), between=c(Class))


############Fruit  
Df_abstract3 %>% ggplot(aes(x = Class, y=Fruit   )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Fruit  , wid = ID,
                            within = c(Period), between=c(Class))

############Sweetened_Beverages   
Df_abstract3 %>% ggplot(aes(x = Class, y=Sweetened_Beverages    )) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Sweetened_Beverages   , wid = ID,
                            within = c(Period), between=c(Class))

############Whole_Grains   
Df_abstract3 %>% ggplot(aes(x = Class, y=Whole_Grains)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Whole_Grains, wid = ID,
                            within = c(Period), between=c(Class))

############Added_Sugar   
Df_abstract3 %>% ggplot(aes(x = Class, y=Added_Sugar)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Added_Sugar, wid = ID,
                            within = c(Period), between=c(Class))

############Fiber   
Df_abstract3 %>% ggplot(aes(x = Class, y=Fiber)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Fiber, wid = ID,
                            within = c(Period), between=c(Class))

# POST HOC TEST ONLY IF ANOVA IS SIGNIFICANT
pwc_2 <- Df_abstract3 %>%  group_by(Class)%>%  
  pairwise_t_test(Fiber ~ Period, paired = T,
                  p.adjust.method	= "none")
pwc_2
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <- Df_abstract3 %>% group_by(Class)%>% 
  cohens_d(Fiber ~ Period,paired = TRUE, hedges.correction = TRUE) 
effect2

##  visualization 
pwc_2 <- pwc_2 %>% add_xy_position(x = "Class")
Fiber_plot_A3 <- Df_abstract3 %>%
  ggplot(aes(x = Class, y=Fiber))  + 
  labs(y="Fiber") + 
  geom_boxplot(aes(fill=Period)) + theme_bw() + 
  stat_pvalue_manual(pwc_2, hide.ns = T)
Fiber_plot_A3
ggsave("Fiber_plot_A3.png")

############Calcium   
Df_abstract3 %>% ggplot(aes(x = Class, y=Calcium)) + 
  geom_boxplot(aes(fill=Period)) + theme_bw()

Df_abstract3 %>% anova_test(dv = Calcium, wid = ID,
                            within = c(Period), between=c(Class))
