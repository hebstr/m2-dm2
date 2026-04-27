.hdrs_data <- set_hdrs()

hdrs$pca <- easy_pca(.hdrs_data)

pc_pct <- \(data, x) {
  data <- data |>
    tidy("eigenvalues") |>
    mutate(p = label_p()(percent))

  lst(p = glue("{data$p[{x}]}"), str = glue("PC{x} ({p})"))
}

### FIG VAR --------------------------------------------------------------------

.var <- lst(
  data = pca_var_extract(hdrs$pca),
  line = list(linetype = "dashed", alpha = 0.3),
  arrow = arrow(
    angle = 20,
    ends = "first",
    length = unit(0.01 + data$weight * 0.05, "npc")
  )
)

.fig_var <- .var$data$coord |>
  ggplot() +
  aes(x = PC1, y = PC2) +
  list(
    geom_vline(xintercept = 0, !!!.var$line),
    geom_hline(yintercept = 0, !!!.var$line)
  ) |>
    inject() +
  geom_segment(
    mapping = aes(color = .var$data$weight),
    yend = 0,
    xend = 0,
    alpha = 0.2,
    arrow = .var$arrow
  ) +
  ggrepel::geom_text_repel(
    mapping = aes(label = column, color = .var$data$weight),
    hjust = 1,
    size = 0.7 + .var$data$weight * 3,
    box.padding = 0.15,
    nudge_x = 0.01,
    nudge_y = 0.01,
    min.segment.length = Inf,
    family = opts$font
  ) +
  scale_color_continuous(low = "grey90", high = opts$color$warm[2]) +
  theme_pca()

### FIG IND --------------------------------------------------------------------

.ind <- lst(
  data = hdrs$pca |>
    augment() |>
    separate_wider_delim(
      .rownames,
      delim = "_",
      names = c("numero", "visit")
    ) |>
    mutate(visit = str_remove(visit, "j") |> as.integer()) |>
    rename_with(~ str_remove_all(., ".fitted")),
  line = list(linetype = "dashed", alpha = 0.3),
  axis = list(
    geom = "text",
    hjust = 0,
    size = 2.5,
    alpha = 0.4,
    family = opts$font
  ),
  arrow = list(
    linewidth = 0.3,
    color = colorspace::lighten(opts$color$base, 0.3),
    arrow = arrow(length = unit(0.035, "npc"))
  ),
  j_str = unique(.hdrs_data$visit) |>
    toupper() |>
    str_flatten_comma(last = " et "),
  color = list(
    low = opts$color$c[1],
    high = opts$color$c[2],
    vctr = opts$color$w[2]
  )
)

fig_pca <- .ind$data |>
  ggplot() +
  aes(y = PC2, x = PC1, color = visit) +
  geom_point(size = 1) +
  list(
    geom_vline(xintercept = 0, !!!.ind$line),
    geom_hline(yintercept = 0, !!!.ind$line),
    geom_segment(y = 3.9, yend = 4.1, x = 0, xend = 0, !!!.ind$arrow),
    geom_segment(x = 5.15, xend = 5.35, y = 0, yend = 0, !!!.ind$arrow),
    annotate(label = pc_pct(hdrs$pca, 1)$str, x = -7.5, y = 0.25, !!!.ind$axis),
    annotate(
      label = pc_pct(hdrs$pca, 2)$str,
      x = -0.25,
      y = -4.5,
      angle = 90,
      !!!.ind$axis
    )
  ) |>
    inject() +
  annotation_custom(
    ggplotGrob(.fig_var),
    xmin = -8.25,
    xmax = -4.7,
    ymin = -0.75,
    ymax = -5
  ) +
  scale_color_continuous(low = .ind$color$low, high = .ind$color$high) +
  theme_pca()

easy_out(fig_pca, width = 8)
