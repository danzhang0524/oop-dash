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
setwd("~/Documents/hetao/oop-dash")
dat <- readRDS("../data/dat_dash_oops.rds")
headers <- readRDS("headers.rds")

  ########## prep data ############
dat <- dat %>% 
  mutate(
    across(ends_with("time"), ymd_hms),
    term_name = fct_reorder(term_name, classopen_date),
    term_label = paste0(term_name,"-",term_remarks,"-",term_id),
    term_label = fct_reorder(term_label, classopen_date),
    l1_order_channel = factor(fct_other(l1_order_channel,
                                        keep = c("自然增长","效果类广告","BD","CPA","CPS","转介绍"), 
                                        other_level = "其他"),
                              levels = c("自然增长","BD","效果类广告","其他","CPA","CPS","转介绍")),
    group_city = factor(group_city, levels = c("西安","北京","成都","济南","武汉")),
    phone_city_level = factor(phone_city_level, 
                              levels = c("一线","新一线","二线","三线","四线","五线")),
    is_apply = ifelse(!is.na(renewal_pay_time) & renewal_order_course_type == "ANNUAL", 1, 0),
    is_renewal = ifelse(!is.na(renewal_pay_time) & renewal_order_course_type == "ANNUAL", 1, 0),
    is_renewal_2 = ifelse(!is.na(renewal_pay_time), 1, 0)
  )

dat_class <- dat %>% 
  filter(
    classopen_date >= "2021-06-18"
  ) %>% 
  mutate(
    term_name = ifelse(term_id == 1888, "2021暑假-第1期-早鸟", as.character(term_name)),
    group_class = case_when(
      str_detect(term_remarks, "转介绍") ~ "转介绍",
      str_detect(term_remarks, "召回") ~ "老用户召回",
      str_detect(term_remarks, "编数") ~ "编数同带",
      str_detect(term_tag_name, "ABCD") & user_level == "D" ~ "温度编程",
      str_detect(term_tag_name, "ABCD") & user_level != "D" ~ "专业编程",
      str_detect(term_tag_name, "ABCD") & is.na(user_level) ~ "专业编程"
    ),
    group_class = factor(group_class, levels = c("专业编程", "温度编程", "转介绍", "老用户召回"))
  ) 

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

  ########## functions ############
source("helper.R")
