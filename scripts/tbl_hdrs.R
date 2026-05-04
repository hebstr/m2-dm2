tbl_hdrs <- hdrs$sum$wide |>
  keep(is.numeric) |>
  tbl_wide_summary(statistic = opts$qt_stat_wide, digits = ~1) |>
  modify_header(
    label = str_glue("**Visite**"),
    stat_6 ~ str_glue("**{names(opts$qt_stat$mean)}**")
  ) |>
  gt_format(width = 500)

easy_out(tbl_hdrs)
