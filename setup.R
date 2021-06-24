## ---- setup --------
library(flexdashboard)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(DT)
library(plotly)
library(highcharter)
#options(highcharter.theme = hc_theme_hcrt(), highcharter.debug = TRUE)

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warnings = FALSE)
#knitr::opts_chunk$set(fig.width = 13, fig.height = 7, fig.align = "center")

  ########## read data ############
root_dir <- "~/Documents/hetao/data/"
dat <- readRDS(paste0(root_dir, "dat_dash_oops.rds"))
headers <- readRDS(paste0(root_dir, "headers.rds"))
#furion <- readRDS("/Users/hetao/Documents/hetao/data/furion.rds")
dat_ct_term <- readRDS(paste0(root_dir, "dat_ct_term.rds"))
rm(root_dir)

  ########## ggplot theme ############
theme_set(
  theme_hc() +
    theme(text = element_text(size = 15, family = "STFangsong"),
          axis.text.x = element_text(angle = 45, size = 10, vjust = 0.5, hjust=1),
          legend.text = element_text(size = 10),
          legend.position = "bottom")
)

source("helper.R")
