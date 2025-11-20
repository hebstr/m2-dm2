fig_scree <-
c("j0", "j56") |> 
  map(~ hdrs$item[[.x]] |> 
        easy_pca() |> 
        tidy("eigenvalues") |> 
        mutate(visit = .)) |>
  list_rbind() |> 
  ggplot() +
  aes(y = percent,
      x = factor(PC),
      fill = visit) +
  facet_wrap(~ visit, ncol = 1) +
  geom_col(alpha = 0.75) +
  geom_text(mapping = 
              aes(y = percent + 0.06 * max(percent),
                  label = label_p(suffix = "")(percent),
                  color = after_scale(fill)),
            !!!opts$bar[-1]) |>
  inject() +
  labs(x = "Composante principale",
       y = "Variance capturée (%)") +
  scale_y_continuous(labels = label_percent(suffix = "")) +
  scale_fill_manual(values = opts$palette) +
  theme_bar(panel.background = element_rect(fill = "grey95"),
            strip.text = element_blank(),
            axis.text = element_text(size = 7))

easy_out(fig_scree, size = c(4.5, 5.5))
