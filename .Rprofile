source("rv/scripts/rvr.R")
source("rv/scripts/activate.R")
source("~/.Rprofile")

library(tidyverse)
library(rlang, warn.conflicts = FALSE)
library(glue)
library(labelled)
library(gtsummary)
library(gt)
library(scales, warn.conflicts = FALSE)
library(broom.mixed)
library(survival)
library(ggsurvfit)
library(hebstr)

update_geom_defaults("text", list(family = "Luciole"))

lang_fr()

conflicted::conflicts_prefer(dplyr::filter, .quiet = FALSE)
