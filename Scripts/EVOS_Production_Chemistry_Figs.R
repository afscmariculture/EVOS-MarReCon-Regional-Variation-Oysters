# Code for generating figures for EVOS Annual Report

# Created By: Mariela Brooks, edited by Becca
# Created On: Dec 3, 2025

# NOTES:
# 12/03/25 - create energy density figure, focusing on T1

# Libraries
library(tidyverse)
library(lubridate)
library(broom)
library(ggpubr)


# Import data file for oyster chem data
EVOS_oysters <- read.csv("Data/EVOS_Production_dataIsoED_T0_T1.csv")

# Data summary by age class and sample period
dataCount_summary <-
  EVOS_oysters %>%
  group_by(SpeciesBio,SampleCat,LifeStage)%>%
  dplyr::summarize(n = n())

knitr::kable(dataCount_summary)

# Relevel farm names to order by region 
EVOS_oysters$Location = factor(EVOS_oysters$Location, 
                               levels =c("BLST","AOF1","KIS1","KOB1",
                                         "BCF1","MIO1","SSF1",
                                         "ROK1","SBO1","SBR1"))

####################################################

# ENERGY DENSITY
# Energy Density Figure - Subadults including all farms for T1
ED_fig <-
  EVOS_oysters %>%
  filter(LifeStage %in% c("subadult")) %>%
  filter(SampleCat %in% c("T1")) %>%
  ggplot(aes(x = Location, y = BombED_Dry, fill = SampleCat)) +
  geom_boxplot(position = position_dodge(0.85)) +
  labs(title="", x = "", y = "Energy Density (kJ/g)") +
  scale_fill_manual(values = c("#FC8D62FF")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none",
        text = element_text(size = 16),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA))


## Save figure
# ggsave("evos_oyster_ED_fig1.png",plot = ED_fig, 
#        height = 7,
#        width = 12,
#        dpi = 300,
#        units = "in",
#        path = "C:/Users/mariela.brooks/Work/MyProjects/EVOS_Mariculture/")


# Energy Density Figure - Subadults including all farms and both T0 (Jinhi Bay) and T1 (Farm sites)
EVOS_oysters %>%
  filter(LifeStage %in% c("subadult")) %>%
  ggplot(aes(x = Location, y = BombED_Dry, fill = SampleCat)) +
  geom_boxplot(position = position_dodge(0.85)) +
  labs(title="", x = "", y = "Energy Density (kJ/g)") +
  scale_fill_manual(values = c("#66C2A5FF", "#FC8D62FF")) +
  theme_bw(base_size = 10) #+

# Energy Density Figure - Juveniles including all farms for T1
EVOS_oysters %>%
  filter(LifeStage %in% c("juvenile")) %>%
  filter(SampleCat %in% c("T1")) %>%
  ggplot(aes(x = Location, y = BombED_Dry, fill = SampleCat)) +
  geom_boxplot(position = position_dodge(0.85)) +
  labs(title="Juvenile composites", x = "", y = "Energy Density (kJ/g)") +
  scale_fill_manual(values = c("#FC8D62FF")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none",
        text = element_text(size = 16),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA))

####################################################

# NITROGEN ISOTOPES subadults T1
EVOS_oysters %>%
  filter(d15N > -400) %>%
  filter(LifeStage %in% c("subadult")) %>%
  filter(SampleCat %in% c("T1")) %>%
  ggplot(aes(x = Location, y = d15N, fill = SampleCat)) +
  geom_boxplot(position = position_dodge(0.85)) +
  labs(title="", x = "", y = expression(paste(delta^{15},"N (‰)"))) +
  scale_fill_manual(values = c("#FC8D62FF")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none",
        text = element_text(size = 16),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA))

#NITROGEN ISOTOPES juveniles T1
EVOS_oysters %>%
  filter(d15N > -400) %>%
  filter(LifeStage %in% c("juvenile")) %>%
  filter(SampleCat %in% c("T1")) %>%
  ggplot(aes(x = Location, y = d15N, fill = SampleCat)) +
  geom_boxplot(position = position_dodge(0.85)) +
  labs(title="Juvenile composites", x = "", y = expression(paste(delta^{15},"N (‰)"))) +
  scale_fill_manual(values = c("#FC8D62FF")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none",
        text = element_text(size = 16),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA))

####################################################

# CARBON ISOTOPES Subadults T1
EVOS_oysters %>%
  filter(d13C > -400) %>%
  filter(LifeStage %in% c("subadult")) %>%
  filter(SampleCat %in% c("T1")) %>%
  ggplot(aes(x = Location, y = d13C, fill = SampleCat)) +
  geom_boxplot(position = position_dodge(0.85)) +
  labs(title="", x = "", y = expression(paste(delta^{13},"C"["corr"]," (‰)"))) +
  scale_fill_manual(values = c("#FC8D62FF")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none",
        text = element_text(size = 16),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA))

#Carbon Isotopes Juveniles T1
EVOS_oysters %>%
  filter(d13C > -400) %>%
  filter(LifeStage %in% c("juvenile")) %>%
  filter(SampleCat %in% c("T1")) %>%
  ggplot(aes(x = Location, y = d13C, fill = SampleCat)) +
  geom_boxplot(position = position_dodge(0.85)) +
  labs(title="Juvenile composites", x = "", y = expression(paste(delta^{13},"C"["corr"]," (‰)"))) +
  scale_fill_manual(values = c("#FC8D62FF")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none",
        text = element_text(size = 16),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA))
