# Load packages
library(lme4)
library(tidyverse)
options(scipen=999)
library(readxl)
library(mgcv)
library(ggplot2)

# Set wd
setwd("C:/Users/neven/Dropbox/Phd project/Experiment/Hungarian_study")

# Import data
Data <- read_xlsx(path = "HUresults.xlsx")

# Run model with no predictors
m0 <- glmer(accuracy ~ 1 +
              (1|subjects) + (1|item),
            family = "binomial",
            data = Data)

summary(m0)

# There is variability between subjects
# but not much variation between items

# ..............................................................................
# Run model with age as fixed effect ----
# ..............................................................................

# Standardize age (z-score transformation)
Data$age_scaled <- scale(Data$age, center = TRUE, scale = TRUE)

# Run model with a fixed effect of age
m <- glmer(accuracy ~ age_scaled +
             (1|subjects) + (1|item),
           family = binomial(),
           data = Data
           )

summary(m)

# Significant effect of age (p = .002)
# Accuracy across subjects varies
# Variation across items is small

# Compare null model with age model
anova(m0, m, test = "Chisq")

# Adding age significantly improves the model
# AIC and Deviance are lower in m

# ..............................................................................
# Run model with pronoun and stimuli types as fixed effects ----
# ..............................................................................

m1 <- glmer(accuracy ~ pronoun_type +
              (1|subjects) + (1|item),
            family = binomial(),
            data = Data
              )

summary(m1)

# ..............................................................................
# Run model with age, pronoun and stimuli types as fixed effects ----
# ..............................................................................

m2 <- glmer(accuracy ~ pronoun_type * age_scaled  +
              (1|subjects) + (1|item),
            family = binomial(),
            data = Data
)

summary(m2)


# No interaction between age and pronoun type

# ..............................................................................
# Plot stimuli type accuracy  ----
# ..............................................................................

averages_by_subject <- Data %>%
  group_by(subjects, pronoun_type, stimuli_type) %>%
  summarize(average = mean(accuracy, na.rm = TRUE), .groups = "drop")

averages <- averages_by_subject %>%
  group_by(pronoun_type, stimuli_type) %>%
  summarize(
    mean = mean(average, na.rm = TRUE),
    se = sd(average, na.rm = TRUE) / sqrt(n()),  
    .groups = "drop"
  )

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_name = ggplot(averages, aes(x=pronoun_type, y=mean, fill=stimuli_type)) + 
  geom_bar(position=position_dodge(), stat="identity",
           color="black", # Use black outlines,
           linewidth=.3) +    # Thinner lines     
  geom_dotplot(data=averages_by_subject, aes(y=average),
               binaxis='y', stackdir='center', dotsize=.5,position=position_dodge())+
  geom_errorbar(aes(y=mean, ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ylab("accuracy") +
  scale_x_discrete(limits=c("pronoun", "anaphor"),
                   labels=c("pronoun", "anaphor"))+  
  scale_fill_manual(breaks=c("true", "false"),
                    values=c("#F0E442", "#009E73")) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw(base_size=20) 

plot(plot_name) 

# ..............................................................................
# Plot pronoun type accuracy  ----
# ..............................................................................

subject_averages <- Data %>%
  group_by(subjects, pronoun_type) %>%
  summarize(average = mean(accuracy, na.rm = TRUE), .groups = "drop")

averages2 <- subject_averages %>%
  group_by(pronoun_type) %>%
  summarize(
    mean = mean(average, na.rm = TRUE),
    se = sd(average, na.rm = TRUE) / sqrt(n()),  
    .groups = "drop"
  )

# Create plot

plot_name = ggplot(averages2, aes(x=pronoun_type, y=mean, fill=pronoun_type)) + 
  geom_bar(position=position_dodge(), stat="identity",
           color="black", # Use black outlines,
           linewidth=.3) +    # Thinner lines     
  geom_dotplot(data=subject_averages, aes(y=average),
               binaxis='y', stackdir='center', dotsize=.5,position=position_dodge())+
  geom_errorbar(aes(y=mean, ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_text(aes(label = paste0(round(mean * 100), "%"), y = mean + 0.05),
            position = position_dodge(0.9),
            vjust = 10,
            size = 6) +
  ylab("accuracy") +
  scale_x_discrete(limits=c("pronoun", "anaphor"),
                   labels=c("pronoun", "reflexive"))+  
  scale_fill_manual(breaks=c("pronoun", "anaphor"),
                    labels=c("pronoun", "reflexive"),
                    values=c( "grey",  "white")) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw(base_size=20) 

plot(plot_name) 


# ..............................................................................
# Plot data with age and accuracy ----
# ..............................................................................

# Aggregate accuracy by age
Data_summary <- Data %>%
  group_by(age) %>%
  summarise(mean_accuracy = mean(accuracy), 
            n = n())

# Plot using GAM
ggplot(Data_summary, aes(x = age, y = mean_accuracy)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = TRUE, color = "blue") +
  ylim(0, 1.05) +
  labs(title = "Effect of Age on Accuracy",
       x = "Age (Days)",
       y = "Mean Accuracy",
       caption = "Smoothed GAM curve with 95% CI") +
  theme_minimal()          
