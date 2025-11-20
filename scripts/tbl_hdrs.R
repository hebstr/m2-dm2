hdrs$sum$wide |>
  keep(is.numeric) |>
  tbl_wide_summary(statistic = opts$qt_stat_wide,
                   digits = ~ 1) |>
  modify_header(label = glue("**Visite**"),
                stat_6 ~ glue("**{names(opts$qt_stat$mean)}**")) |>
  gt_format(width = 500) |>
  easy_out("tbl_hdrs")
