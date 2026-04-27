.bar <- list(
  text = aes(
    y = after_stat(count) + 0.05 * max(count),
    label = after_stat(count),
    color = after_scale(fill)
  ),
  position = position_dodge2(preserve = "single")
)

### GLOBAL ---------------------------------------------------------------------

fig_hdrs_global <- hdrs$sum$long |>
  filter(visit %in% c("j0", "j56")) |>
  ggplot() +
  aes(x = hdrs, fill = visit) +
  geom_histogram(binwidth = 0.25, position = .bar$position) +
  geom_text(
    mapping = .bar$text,
    position = position_dodge(width = 1),
    !!!opts$bar
  ) |>
    inject() +
  labs(x = "Valeur du score global", y = opts$y) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  scale_fill_manual(values = opts$palette) +
  theme_bar(grid = FALSE)

easy_out(fig_hdrs_global, width = 5.25)

### ITEM -----------------------------------------------------------------------

fig_hdrs_items <- lst(
  n2 = c(4, 5, 6, 12, 13, 14, 16, 17),
  n4 = c(1:17)[-n2]
) |>
  map(
    ~ set_hdrs(.visit = c("j0", "j56")) |>
      pivot_longer(cols = matches("hamd"), names_to = "item") |>
      mutate(item = str_extract(item, "\\d+")) |>
      filter(item %in% .) |>
      ggplot() +
      aes(x = value, fill = visit) +
      facet_wrap(~ as.numeric(item), ncol = 3) +
      geom_bar(alpha = 0.75, position = .bar$position) +
      geom_text(
        mapping = .bar$text,
        position = position_dodge(width = 0.9),
        !!!opts$bar
      ) |>
        inject() +
      labs(x = "Valeur", y = opts$y) +
      scale_y_continuous(n.breaks = 7) +
      scale_fill_manual(values = opts$palette) +
      theme_bar(
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold")
      )
  )

easy_out_map(fig_hdrs_items, height = 6.2, width = 6)
