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

getRates <- function(data) {
  data %>% 
    summarise(
      enroll_total = n_distinct(user_id, na.rm = TRUE),
      apply_total = n_distinct(ifelse(is_apply == 1, user_id, NA), na.rm = TRUE),
      apply_rate = apply_total/enroll_total,
      add_wx_rate = mean(ifelse(is.na(add_wx_time), 0, 1)),
      across(c(unit1_attend_time, unit5_finish_time),
             ~ mean(ifelse(is.na(.x), 0, 1)),
             .names = "{.col}_rate"),
      unit5_apply_rate = apply_rate/unit5_finish_time_rate,
      retent_rate = unit5_finish_time_rate/unit1_attend_time_rate,
      .groups = "drop_last"
    ) %>% 
    mutate(
      across(ends_with("rate"), ~ round(.x *100, 1)),
      across(ends_with("prop"), ~ round(.x *100, 0))
    )
}

getProps <- function(data) {
  data %>% 
    summarise(
      grade_low_prop = mean(package_grade == "LOW", na.rm = TRUE),
      referral_prop = mean(l1_order_channel == "转介绍", na.rm = TRUE),
      grade_8_prop = mean(pay_grade == 8, na.rm = TRUE),
      .groups = "drop_last"
    ) %>% 
    mutate(
      across(ends_with("rate"), ~ round(.x *100, 1)),
      across(ends_with("prop"), ~ round(.x *100, 0))
    )
}
