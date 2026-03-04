# Import Libraries 
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(readxl)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(scatterplot3d)
library(rgl)
library(plotly)
library(htmlwidgets)
library(rstatix)
library(kableExtra)
library(dplyr)
library(tidyr)
library(knitr)
library(DT)


# Import File - Maigreur Jeunes Garçons
MaigreurGarçons <- read_excel("Proportion de jeunes GARCONS de 17 ans ayant un IMC correspondant à une situation de maigreur.xlsx")
print(MaigreurGarçons)

summary(MaigreurGarçons)

#Normality tests
## Shapiro test 
MaigreurGarçons %>% shapiro_test(2008, 2011, 2014, 2017, 2022)
## Diagramme de densité
ggdensity(MaigreurGarçons$'2022', fill = "lightgray")
## QQ plot
ggqqplot(MaigreurGarçons$'2014')


MaigreurGarçons <- MaigreurGarçons %>%
  gather(key = "Years", value = "Year", '2008', '2011', '2014', '2017', '2022') %>%
  convert_as_factor(ZoneGéo, Years)
MaigreurGarçons 

# Analyse par région - Graphique interactif 
p_interactive <- ggplot(MaigreurGarçons,
                        aes(x = Years,
                            y = Year,
                            color = ZoneGéo,
                            group = ZoneGéo,
                            text = paste(
                              "Région :", ZoneGéo,
                              "<br>Année :", Years,
                              "<br>Valeur :", Year
                            ))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proportion de jeunes garçons de 17 ans en situation de maigreur (IMC < 18.5) par région",
    x = "Année",
    y = "Pour 100",
    color = "Régions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "red", size = 12, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12,face = "bold")
  )
fig <- ggplotly(p_interactive, tooltip = "text")
fig


# Import File - Maigreur jeunes filles 
MaigreurFilles <- read_excel("Proportion de jeunes FILLES de 17 ans ayant un IMC correspondant à une situation de maigreur.xlsx")
print(MaigreurFilles)

summary(MaigreurFilles)

#Normality tests
## Shapiro test 
MaigreurFilles %>% shapiro_test(2008, 2011, 2014, 2017, 2022)
## Diagramme de densité
ggdensity(MaigreurFilles$'2014', fill = "lightgray")
## QQ plot
ggqqplot(MaigreurFilles$'2014')

MaigreurFilles <- MaigreurFilles %>%
  gather(key = "Years", value = "Year", '2008', '2011', '2014', '2017', '2022') %>%
  convert_as_factor(ZoneGéo, Years)
MaigreurFilles 

# Analyse par région - Graphique interactif 
p_interactive <- ggplot(MaigreurFilles,
                        aes(x = Years,
                            y = Year,
                            color = ZoneGéo,
                            group = ZoneGéo,
                            text = paste(
                              "Région :", ZoneGéo,
                              "<br>Année :", Years,
                              "<br>Valeur :", Year
                            ))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proportion de jeunes filles de 17 ans en situation de maigreur (IMC < 18.5) par région",
    x = "Année",
    y = "Pour 100",
    ylim=c(0,14),
    color = "Régions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "red", size = 12, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12,face = "bold")
  )
fig <- ggplotly(p_interactive, tooltip = "text")
fig


# Boxplot per year UNIQUEMENT POUR GARCONS 
## Statistical test
res.fried <- MaigreurGarçons %>% friedman_test(Year ~ Years |ZoneGéo)
res.fried

pwc <- MaigreurGarçons %>%
  wilcox_test(Year ~ Years, paired = TRUE,p.adjust.method = "holm")
pwc

#boxplot chart
pwc <- pwc %>% add_xy_position(x = "Years", step.increase = 0.05)
ggboxplot(MaigreurGarçons,
          x = "Years",
          y = "Year",
          ylab = "Pour 100",
          palette = c("#00AFBB", "#E7B800", "#FC4E07","#1B9E77","#ABC333"),
          fill = "Years", 
          add = "jitter",
          title = "Proportion de jeunes garçons de 17 ans en situation de maigreur (IMC < 18.5) en France",
          bxp.errorbar = TRUE) +
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = 0) +
  theme(
    legend.position ='none',
    plot.title = element_text(color = "red", size = 12, face = "bold", hjust = 0.5)) +
  labs(caption =get_pwc_label(pwc))


# Boxplot per year UNIQUEMENT POUR FILLES 
## Statistical test
res.fried <- MaigreurFilles %>% friedman_test(Year ~ Years |ZoneGéo)
res.fried

pwc <- MaigreurFilles %>%
  wilcox_test(Year ~ Years, paired = TRUE,p.adjust.method = "holm")
pwc


pwc <- pwc %>% add_xy_position(x = "Years", step.increase = 0.05)
ggboxplot(MaigreurFilles,
          x = "Years",
          y = "Year",
          ylab = "Pour 100",
          ylim = c(0,16),
          palette = c("#00AFBB", "#E7B800", "#FC4E07","#1B9E77","#ABC333"),
          fill = "Years", 
          add = "jitter",
          title = "Proportion de jeunes filles de 17 ans en situation de maigreur (IMC < 18.5) en France",
          bxp.errorbar = TRUE) +
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = 0) +
  theme(
    legend.position ='none',
    plot.title = element_text(color = "red", size = 12, face = "bold", hjust = 0.5)) +
  labs(caption = get_test_label(res.fried, detailed = FALSE), get_pwc_label(pwc))




# Boxplot final
## Import File 
MaigreurAdos <- read_excel("Proportion de jeunes garçons et filles de 17 ans ayant un IMC correspondant à une situation de maigreur.xlsx")
print(MaigreurAdos)

## Préparation des données 
MaigreurAdos_long <- MaigreurAdos %>%
  pivot_longer(
    cols = c(`2008`, `2011`, `2014`, `2017`, `2022`),
    names_to = "Annee",
    values_to = "Obesite"
  ) %>%
  mutate(
    ZoneGéo = factor(Zone_Géo),
    Sexe_jeunes = factor(Sexe_jeunes),
    Annee = factor(Annee, levels = c("2008","2011","2014","2017","2022"))
  )
MaigreurAdos_long

## Test de Friedman
res.fried <- MaigreurAdos_long %>%
  group_by(Sexe_jeunes) %>%
  friedman_test(Obesite ~ Annee | Zone_Géo)
res.fried

## Post-hoc Test de Wilcoxon
### Comparaison entre année pour chaque sexe
pwc_years <- MaigreurAdos_long %>%
  group_by(Sexe_jeunes) %>%
  wilcox_test(
    Obesite ~ Annee,
    paired = TRUE,
    p.adjust.method = "holm"
  )
pwc_years

### Comparaison entre sexe pour chaque année 
pwc_sexe <- MaigreurAdos_long %>%
  group_by(Annee) %>%
  wilcox_test(
    Obesite ~ Sexe_jeunes,
    paired = TRUE,
    p.adjust.method = "holm"
  )
pwc_sexe

## Position p-value sur le graphique
pwc_years <- pwc_years %>%
  add_xy_position(x = "Annee", dodge = 0.8)

pwc_sexe <- pwc_sexe %>%
  add_xy_position(x = "Annee", dodge = 0.8)


## Graphique
ggbarplot(
  MaigreurAdos_long,
  x = "Annee",
  y = "Obesite",
  ylim = c(0,10),
  fill = "Sexe_jeunes",
  palette = c("#00AFBB", "#FC4E07"),
  add = "mean_se",
  position = position_dodge(0.8)
) +
  
  stat_pvalue_manual(
    pwc_sexe,
    hide.ns = TRUE,
    tip.length = 0
  ) +
  
  labs(
    title = "Comparaison filles/garçons de 17 ans en situation de maigreur (IMC < 18.5) en France",
    x = "Année",
    y = "Pour 100",
    fill = "Sexe"
  ) +
  
  theme_classic() +
  theme(
    plot.title = element_text(
      color = "red",
      size = 14,
      face = "bold",
      hjust = 0.5
    )
  )