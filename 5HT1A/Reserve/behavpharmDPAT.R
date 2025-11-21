#Analysis scripts for data on the effects of 8-OH-DPAT on time-dependent sensitization in zebrafish
#Project: "Role of serotonin in behavioral processes relevant to modeling mental disorders"
#Project coordinator: Caio Maximino (ORCID: https://orcid.org/0000-0002-3261-9196)
#Project grant: CNPq/Brazil (Process no. 302998/2019-5)

#Install and load libraries
if(!require(readr)){
  install.packages("readr")
  library(readr)
}

if(!require(RCurl)){
  install.packages("RCurl")
  library(RCurl)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(dabestr)){
  devtools::install_github(repo = "ACCLAB/dabestr", ref = "dev")
  library(dabestr)
}

#Load dataset for light/dark test
DPAT_LDT <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/5HT/main/5HT1A/DPAT_LDT.csv", col_types = cols(Group = col_factor(levels = c("CTRL", "DPAT"))))
View(DPAT_LDT)

#Run t-test on "Time on white"
t.test(TimeOnWhite ~ Group, data = DPAT_LDT)

#Create object for plotting
dabest_DPAT_LDT_tw <- load(data = DPAT_LDT, x = Group, y = TimeOnWhite, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_LDT_tw, TRUE, swarm_label = "Time on white (s)", swarm_ylim = c(0, 900))

#Run t-test on "Risk assessment"
t.test(RiskAssessment ~ Group, data = DPAT_LDT)

#Create object for plotting
dabest_DPAT_LDT_ra <- load(data = DPAT_LDT, x = Group, y = RiskAssessment, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_LDT_ra, TRUE, swarm_label = "Risk assessment (N)")

#Run t-test on "Erratic swimming"
t.test(ErraticSwimming ~ Group, data = DPAT_LDT)

#Create object for plotting
dabest_DPAT_LDT_es <- load(data = DPAT_LDT, x = Group, y = ErraticSwimming, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_LDT_es, TRUE, swarm_label = "Erratic swimming (Abs. turn angle)", swarm_ylim = c(0, 45))

#Run t-test on "Freezing"
t.test(Freezing ~ Group, data = DPAT_LDT)

#Create object for plotting
dabest_DPAT_LDT_fr <- load(data = DPAT_LDT, x = Group, y = Freezing, idx = c("CTRL", "DPAT")) %>% mean_diff()

#Plot
dabest_plot(dabest_DPAT_LDT_fr, TRUE, swarm_label = "Freezing (s)")

#Run t-test on "Thigmotaxis"
t.test(Thigmotaxis ~ Group, data = DPAT_LDT)

#Create object for plotting
dabest_DPAT_LDT_th <- load(data = DPAT_LDT, x = Group, y = Thigmotaxis, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_LDT_th, TRUE, swarm_label = "Thigmotaxis (%)")

#Run t-test on "Swimming speed"
t.test(Speed ~ Group, data = DPAT_LDT)

#Create object for plotting
dabest_DPAT_LDT_sp <- load(data = DPAT_LDT, x = Group, y = Speed, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_LDT_sp, TRUE, swarm_label = "Swimming speed (cm/s)")

#Load dataset for novel tank test
DPAT_NTT <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/5HT/main/5HT1A/DPAT_NTT.csv", col_types = cols(Group = col_factor(levels = c("CTRL", "DPAT"))))
View(DPAT_NTT)

#Run t-test on "Time on bottom"
t.test(TimeBottom ~ Group, data = DPAT_NTT)

#Create object for plotting
dabest_DPAT_NTT_tb <- load(data = DPAT_NTT, x = Group, y = TimeBottom, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_NTT_tb, TRUE, swarm_label = "Time on bottom third of the tank (s)", swarm_ylim = c(0, 360))

#Run t-test on "Time on top"
t.test(TimeTop ~ Group, data = DPAT_NTT)

#Create object for plotting
dabest_DPAT_NTT_tt <- load(data = DPAT_NTT, x = Group, y = TimeTop, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_NTT_tt, TRUE, swarm_label = "Time on top third of the tank (s)", swarm_ylim = c(0, 360))

#Run t-test on "Erratic swimming"
t.test(ErraticSwimming ~ Group, data = DPAT_NTT)

#Create object for plotting
dabest_DPAT_NTT_es <- load(data = DPAT_NTT, x = Group, y = ErraticSwimming, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_NTT_es, TRUE, swarm_label = "Erratic swimming (Abs. turn angle)", swarm_ylim = c(0, 45))

#Run t-test on "Freezing"
t.test(Freezing ~ Group, data = DPAT_NTT)

#Create object for plotting
dabest_DPAT_NTT_fr <- load(data = DPAT_NTT, x = Group, y = Freezing, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_NTT_fr, TRUE, swarm_label = "Freezing (s)")

#Run t-test on "Swimming speed"
t.test(Speed ~ Group, data = DPAT_NTT)

#Create object for plotting
dabest_DPAT_NTT_sp <- load(data = DPAT_NTT, x = Group, y = Speed, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_NTT_sp, TRUE, swarm_label = "Swimming speed (cm/s)")

#Load dataset for social investigation test
DPAT_SI <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/5HT/main/5HT1A/DPAT_SI.csv", col_types = cols(Group = col_factor(levels = c("CTRL", "DPAT"))))
View(DPAT_SI)

#Run t-test on "Preference score"
t.test(PreferenceScore ~ Group, data = DPAT_SI)

#Create object for plotting
dabest_DPAT_SI_ps <- load(data = DPAT_SI, x = Group, y = PreferenceScore, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SI_ps, TRUE, swarm_label = "Preference score", swarm_ylim = c(-1, 1))

#Run t-test on "Social avoidance"
t.test(FarFromStimulus ~ Group, data = DPAT_SI)

#Create object for plotting
dabest_DPAT_SI_sa <- load(data = DPAT_SI, x = Group, y = FarFromStimulus, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SI_sa, TRUE, swarm_label = "Time near empty tank (s)", swarm_ylim = c(0, 360))

#Run t-test on "Erratic swimming"
t.test(ErraticSwimming ~ Group, data = DPAT_SI)

#Create object for plotting
dabest_DPAT_SI_es <- load(data = DPAT_SI, x = Group, y = ErraticSwimming, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SI_es, TRUE, swarm_label = "Erratic swimming (Abs. turn angle)", swarm_ylim = c(0, 45))

#Run t-test on "Swimming speed"
t.test(Speed ~ Group, data = DPAT_SI)

#Create object for plotting
dabest_DPAT_SI_sp <- load(data = DPAT_SI, x = Group, y = Speed, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SI_sp, TRUE, swarm_label = "Swimming speed (cm/s)", swarm_ylim = c(0, 12))

#Load dataset for social novelty test
DPAT_SN <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/5HT/main/5HT1A/DPAT_SN.csv", col_types = cols(Group = col_factor(levels = c("CTRL", "DPAT"))))
View(DPAT_SN)

#Run t-test on "Preference score"
t.test(PreferenceScore ~ Group, data = DPAT_SN)

#Create object for plotting
dabest_DPAT_SN_ps <- load(data = DPAT_SN, x = Group, y = PreferenceScore, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SN_ps, TRUE, swarm_label = "Preference score", swarm_ylim = c(-1, 1))

#Run t-test on "Social avoidance"
t.test(OldStranger ~ Group, data = DPAT_SN)

#Create object for plotting
dabest_DPAT_SN_sa <- load(data = DPAT_SN, x = Group, y = OldStranger, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SN_sa, TRUE, swarm_label = "Time near 'old stranger' (s)", swarm_ylim = c(0, 360))

#Run t-test on "Erratic swimming"
t.test(ErraticSwimming ~ Group, data = DPAT_SN)

#Create object for plotting
dabest_DPAT_SN_es <- load(data = DPAT_SN, x = Group, y = ErraticSwimming, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SN_es, TRUE, swarm_label = "Erratic swimming (Abs. turn angle)", swarm_ylim = c(0, 45))

#Run t-test on "Swimming speed"
t.test(Speed ~ Group, data = DPAT_SN)

#Create object for plotting
dabest_DPAT_SN_sp <- load(data = DPAT_SN, x = Group, y = Speed, idx = c("CTRL", "DPAT")) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_SN_sp, TRUE, swarm_label = "Swimming speed (cm/s)", swarm_ylim = c(0, 12))

#Load dataset for receptor reserve
DPAT_reserve <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/5HT/refs/heads/main/5HT1A/DPAT_reserve.csv", col_types = cols(`8-OH-DPAT dose (mg/kg)` = col_factor(levels = c("0.00", "0.03", "0.30", "3.00")), `WAY 100,635 dose (mg/kg)` = col_factor(levels = c("0.000", "0.001"))))
View(DPAT_reserve)

#Run two-way ANOVA on "Geotaxis"
anova_reserve <- aov(DPAT_reserve$`Geotaxis (s)` ~ DPAT_reserve$`8-OH-DPAT dose (mg/kg)`:DPAT_reserve$`WAY 100,635 dose (mg/kg)`, data = DPAT_reserve)
summary(anova_reserve)
TukeyHSD(anova_reserve)

#Create object for plotting
dabest_DPAT_reserve_ps <- load(data = DPAT_reserve_longform, x = Group, y = `Geotaxis (s)`, idx = list(c("0.00_0.000", "0.00_0.001"), c("0.03_0.000", "0.03_0.001"), c("0.30_0.000", "0.30_0.001"), c("3.00_0.000", "3.00_0.001"))) %>% cohens_d()

#Plot
dabest_plot(dabest_DPAT_reserve_ps, TRUE, swarm_label = "Geotaxis (s)", swarm_ylim = c(0, 360), contrast_ylim = c(-2, 7))
