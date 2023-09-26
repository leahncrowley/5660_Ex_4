# GLMs with Binary Data - Advanced Data Analytics 
# Leah N. Crowley - Fall 2023 

# Call requried packages to workspace: 
  library(ggplot2)
  library(arm)
  library(ggfortify)
  library(performance)
  library(AICcmodavg)
  library(grid)
  
# Read in data you are using for this assignment: 
  bats <- read.csv("bats.csv")

# Plot binary data - Presence of P. destructans on bat as a function of bat age in years
  ggplot(bats, aes(age.years, pd)) +
    geom_point(size=5, color = "goldenrod1") +
    geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), color = "deeppink") +
    labs(title="Bat Age and Fungal Presence") +
    ylab ("Probability of Fungus Presence") +
    xlab ("Bat Age (Years)") +
    theme_light()

# Model this relationship: 
  model_bats_age <- glm(pd ~ age.years, data=bats, family=binomial)
  model_bats_age  
  coef(model_bats_age)
  confint(model_bats_age)
  summary(model_bats_age)

# Plot binary data - Presence of P. destructans on bat as a function of bat mass in grams
  ggplot(bats, aes(mass.grams, pd)) +
    geom_point(size=5, color = "palegreen3") +
    geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), color = "royalblue1") +
    labs(title="Bat Mass and Fungal Presence") +
    ylab ("Probability of Fungus Presence") +
    xlab ("Bat Mass (g)") +
    theme_light()

# Model this relationship: 
  model_bats_mass <- glm(pd ~ mass.grams, data=bats, family=binomial)
  model_bats_mass  
  coef(model_bats_mass)
  confint(model_bats_mass)
  summary(model_bats_mass)
  