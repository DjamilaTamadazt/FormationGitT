rm(list = ls())
setwd("/home/onyxia/formation-bonnes-pratiques-R")

# if (!require("ggplot2"))
#   install.packages("ggplot2")
# if (!require("stringr"))
#   install.packages("stringr")
# if (!require("dplyr"))
#   install.packages("dplyr")
# if (!require("tidyverse"))
#   install.packages("tidyverse")
# if (!require("MASS"))
#   install.packages("MASS")


library(tidyverse)
library(forcats)

source("scripts/functions.R", encoding = "UTF-8")

# Import des données
df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select = c(
    "region",
    "aemm",
    "aged",
    "anai",
    "catl",
    "cs1",
    "cs2",
    "cs3",
    "couple",
    "na38",
    "naf08",
    "pnai12",
    "sexe",
    "surf",
    "tp",
    "trans",
    "ur"
  )
)

# Retraitement des données

df <- df %>%
  dplyr::mutate(aged = as.numeric(aged))

dplyr::summarise(group_by(df, aged), n())

ggplot2::ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

# stats trans par statut
df3 <- df %>%
  dplyr::group_by(couple, trans) %>%
  dplyr::summarise(x = n()) %>%
  dplyr::group_by(couple) %>%
  dplyr::mutate(y = 100 * x / sum(x))

p <- # part d'homme dans chaque cohort
  df %>%
  dplyr::group_by(aged, sexe) %>%
  dplyr::summarise(SH_sexe = n()) %>%
  dplyr::group_by(aged) %>%
  dplyr::mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  dplyr::filter(sexe == 1) %>%
  ggplot2::ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


ggsave("p.png", p)


df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")


calcul_stat_agregee (rnorm(10))
calcul_stat_agregee (rnorm(10), "ecart-type")
calcul_stat_agregee (rnorm(10), "variance")

calcul_stat_agregee (df %>% dplyr::filter(sexe == "Homme") %>% pull(aged))
calcul_stat_agregee (df %>% dplyr::filter(sexe == "Femme") %>% pull(aged))

api_token <- "trotskitueleski$1917"

# modelisation

df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  dplyr::filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  dplyr::filter(couple == "2" & aged > 40 & aged < 60)
polr(surf ~ cs1 + factor(ur), df3)
