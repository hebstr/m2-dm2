### DATA -----------------------------------------------------------------------

hdrs$surv <- lst(
  data =
    hdrs$sum$long |>
      mutate(visit = str_remove(visit, "j") |> as.integer()) |>
      group_by(numero) |>
        mutate(
          bin = if_else(hdrs <= 0.5 * hdrs[visit == 0], 1, 0),
          event = if_else(sum(bin) >= 1, 1, 0),
          time = if_else(
            event == 1,
            visit[bin == 1][1],
            max(visit[bin == 0])
          )
        ) |>
        ungroup() |>
      select(numero, groupe, event, time) |>
      distinct(),
  model = lst(
    fit = exprs(Surv(time, event) ~ groupe, data),
    tte = lst(
      obj = do.call("survfit2", fit),
      tidy =
        tidy_survfit(obj) |>
          split(~ strata) |>
          map(
            ~ . |>
              unite(
                "n.event/censor", n.event:n.censor,
                sep = "/",
                remove = TRUE
              ) |>
              unite(
                "cum.event/censor", cum.event:cum.censor,
                sep = "/",
                remove = TRUE
              ) |>
              merge_estim_ci() |>
              select(time, starts_with(c("n", "cum")), estimate_ci)
          )
    ),
    cox = lst(
      obj = do.call("coxph", fit),
      tidy =
        tidy(obj, exp = TRUE, conf.int = TRUE) |>
          merge_estim_ci() |>
          mutate(
            str = "Hazard ratio pour l'absence de réponse",
            p.value = style_pvalue(
              p.value,
              digits = 1,
              prepend_p = TRUE
            )
          )
    )
  )
)

### FIG ------------------------------------------------------------------------

fig_surv <-
hdrs$surv$model$tte$obj |>
  ggsurvfit() +
  add_quantile(
    y_value = 0.5,
    color = opts$color$w[2]
  ) +
  add_risktable(
    risktable_stats = c("{n.risk} ({cum.event})"),
    stats_label = list("{n.risk} ({cum.event})" = "No. à risque (évènements)"),
    risktable_group = "risktable_stats",
    size = 2,
    theme = theme_risktable(plot_margin = 5)
  ) +
  add_risktable_strata_symbol(symbol = "\U2014", size = 8) +
  geom_text(
    data = hdrs$surv$model$cox$tidy,
    mapping = aes(
      label = glue(
        "{str} {opts$ci$label}{opts$sep$int}{estimate_ci}
        Log rank {p.value}"
      )
    ),
    x = 56,
    y = 1,
    hjust = 1,
    vjust = 1,
    size = 2.5
  ) +
  scale_ggsurvfit(
    x_scales = list(
      name = "Durée depuis la première visite (jours)",
      breaks = hdrs$visit$num,
      limits = c(0, 57)
    ),
    y_scales = list(
      name = "Probabilité de non-réponse (%)",
      breaks = seq(0, 1, by = 0.2),
      labels = label_percent(suffix = "")
    )
  ) +
  scale_color_manual(values = opts$palette) +
  theme_tte()

easy_out(fig_surv, size = c(3.25, 6.5))
