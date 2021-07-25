########## functions ############
showDT <- function(data, pageLength = 20, dom = 'Blfrtip',
                   names.conv = TRUE) {
  
  options(DT.options = list(
    pageLength = pageLength,
    dom = dom,
    buttons = list('copy', "csv"))
  )
  
  cn = names(data)
  groupVars = group_vars(data)
  
  DT::datatable(
    data,
    class = 'cell-border stripe display compact',
    colnames = headers[cn,],
    rownames = FALSE,
    extensions = 'Buttons'
  ) %>% 
    formatRound(grep("rate", cn), 1) %>% 
    formatStyle(grep("rate|prop", cn), fontStyle = "italic") %>% 
    formatStyle(groupVars, fontSize = '80%') %>% 
    formatStyle(grep("rate", cn),
                background = styleColorBar(range(data[grep("rate", cn)], na.rm = TRUE), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center') 
  # formatStyle(ifelse(length(grep("prop", cn)) == 0, 1, grep("prop", cn)),
  #             background = styleColorBar(range(data[grep("prop", cn)], na.rm = TRUE), 'lightgrey'),
  #             backgroundSize = '98% 88%',
  #             backgroundRepeat = 'no-repeat',
  #             backgroundPosition = 'center')
}

# plot_comp <- function(data, x, y, group, grid) {
#   gg =  data %>%
#     mutate(label = factor(ifelse(term_name == "2021春季-第16期", pay_grade, NA))) %>% 
#     ggplot(aes_string(x = x, y = y)) +
#     geom_line(aes_string(group = group, color = group)) +
#     geom_text(aes(label = label, color = label),
#               hjust = 0, nudge_x = 0.1) +
#     labs( x = "", y = "") +
#     theme(legend.position = 'none') 
#   
#   if (!missing(grid)) gg = gg + facet_grid(vars(package_grade))
#   
#   ggplotly(gg)
#   }

getRates <- function(grp_data, .vars, ...) {
  grp_data %>% 
    summarise(
      enroll_total = n_distinct(user_id, na.rm = TRUE),
      apply_total = n_distinct(ifelse(!is.na(renewal_pay_time) & 
                                      renewal_order_course_type == "ANNUAL",
                                      user_id, NA), na.rm = TRUE),
      apply_rate = apply_total/enroll_total,
      apply_2_rate = n_distinct(ifelse(!is.na(renewal_pay_time), user_id, NA), 
                                na.rm = TRUE)/enroll_total,
      ct_total = n_distinct(counselor_id, na.rm = TRUE),
      ct_enroll_total = round(enroll_total/ct_total),
      add_wx_rate = mean(ifelse(is.na(add_wx_time), 0, 1)),
      subscribe_rate = mean(is_subscribe, na.rm = TRUE),
      unit0_attend_rate = n_distinct(ifelse(is.na(unit0_attend_time), NA, user_id), na.rm = TRUE)/enroll_total,
      unit0_finish_rate = n_distinct(ifelse(is.na(unit0_finish_time), NA, user_id), na.rm = TRUE)/enroll_total,
      #.unit1_attend_dayn = difftime(min(unit1_attend_time, na.rm = TRUE), unit1_attend_time, units = "days"),
      #unit1_d1_attend_rate = n_distinct(ifelse((!is.na(unit1_attend_time)) & (.unit1_attend_dayn == 0), user_id, NA), na.rm = TRUE)/enroll_total,
      unit1_attend_rate = n_distinct(ifelse(is.na(unit1_attend_time), NA, user_id), na.rm = TRUE)/enroll_total,
      unit5_finish_rate = n_distinct(ifelse(is.na(unit5_finish_time), NA, user_id), na.rm = TRUE)/enroll_total,
      retent_rate = unit5_finish_rate/unit1_attend_rate,
      unit5_apply_rate = apply_rate/unit5_finish_rate,
      .groups = "drop_last"
    ) %>% 
    mutate(
      across(ends_with("rate"), ~ round(.x *100, 1)),
      enroll_rate = round(enroll_total/sum(enroll_total)*100),
      across(ends_with("rate"), ~ ifelse(.x < 1, NA, .x))
    ) %>% 
    select(-starts_with("."))
}

getRates_detail <- function(grp_data, .vars, ...) {
  grp_data %>% 
    summarise(
      across(c(paste0("unit", rep(0:5, each = 2), c("_attend","_finish"), "_time")),
             ~ mean(ifelse(is.na(.x), 0, 1)),
             .names = "{.col}_rate"),
      .groups = "drop_last"
    ) %>% 
    mutate(
      across(ends_with("rate"), ~ round(.x *100, 0)),
      across(ends_with("rate"), ~ ifelse(.x < 1, NA, .x))
    )
}

getRenewalRates <- function(grp_data, .vars, ...) {
  grp_data %>% 
    summarise(
      enroll_total = n_distinct(user_id, na.rm = TRUE),
      apply_d1_rate = n_distinct(ifelse(renewal_dayn <= 0, user_id, NA), 
                                 na.rm = TRUE)/enroll_total,
      apply_d2_rate = n_distinct(ifelse(renewal_dayn <= 1, user_id, NA), 
                                 na.rm = TRUE)/enroll_total,
      apply_d3_rate = n_distinct(ifelse(renewal_dayn <= 2, user_id, NA), 
                                 na.rm = TRUE)/enroll_total,
      apply_d4_rate = n_distinct(ifelse(renewal_dayn <= 3, user_id, NA), 
                                 na.rm = TRUE)/enroll_total,
      .groups = "drop_last"
    ) %>% 
    select(-enroll_total) %>%
    mutate(
      across(ends_with("rate"), ~ round(.x *100, 1)),
      across(ends_with("rate"), ~ ifelse(.x < 1, NA, .x))
    ) 
}

getAttendRates <- function(grp_data, ...) {
  grp_data %>% 
    mutate(
      .min = as.Date(min(unit1_attend_time, na.rm = TRUE)),
      .dayn = as.Date(unit1_attend_time) - .min
    ) %>% 
    summarise(
      unit1_attend_d1_rate = mean(ifelse((!is.na(unit1_attend_time)) & .dayn == 0,
                                         1, 0))
    ) %>% 
    select(-starts_with(".")) %>% 
    mutate(
      across(ends_with("rate"), ~ round(.x *100, 0)),
      across(ends_with("rate"), ~ ifelse(.x < 1, NA, .x))
    ) 
}

getProps <- function(grp_data, .vars, ...) {
  grp_data %>% 
    summarise(
      grade_low_rate = mean(package_grade == "LOW", na.rm = TRUE),
      referral_rate = mean(l1_order_channel == "转介绍", na.rm = TRUE),
      grade_8_rate = mean(pay_grade == 8, na.rm = TRUE),
      .groups = "drop_last"
    ) %>% 
    mutate(
      across(ends_with("rate"), ~ round(.x *100, 0))
    )
}

getSegs <- function(data) {
  
  
}




