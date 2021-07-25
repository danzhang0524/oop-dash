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
dat <- readRDS(paste0("~/Documents/hetao/data/dat_dash_oops.rds"))
root_dir <- "~/Documents/hetao/data/"
headers <- readRDS("~/Documents/hetao/data/headers.rds")
rm(root_dir)

  ########## ggplot theme ############
theme_set(
  theme_hc() +
    theme(text = element_text(size = 15, family = "STFangsong"),
          axis.text.x = element_text(angle = 45, size = 10, vjust = 0.5, hjust=1),
          legend.text = element_text(size = 10),
          legend.position = "bottom")
)
  ########## color list ############

color = list(
  "channel" = c("black","#53b400","#00b6eb","lightgrey","#c9a215","#a58aff","#ee6bda"),
  "group_city" = c("#00b6eb","#53b400","#c9a215","#a58aff","#ee6bda","lightgrey")
)


source("~/Documents/hetao/oop-dash/helper.R")
