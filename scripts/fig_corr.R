fig_corr <- hdrs$visit$str |>
  map(
    ~ scl$join |>
      filter(visit == .) |>
      corrr::correlate(quiet = TRUE) |>
      select(term, {{ . }} := hdrs) |>
      filter(term != "hdrs") |>
      column_to_rownames("term") |>
      set_names(toupper)
  ) |>
  list_cbind() |>
  rownames_to_column("term") |>
  gt_heatmap(
    rowname_col = "term",
    digit = 2,
    width = 400,
    palette = with(opts$color, c(warm[2], cold[2])),
    arrange = TRUE
  )

easy_out(fig_corr)
