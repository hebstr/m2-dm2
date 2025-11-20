### DATA -----------------------------------------------------------------------

.value <-
c("ab", "na") |> 
  set_names() |> 
  map(~ lst(n = scl$value[[.]],
            p = label_p()(nrow(n) / scl$n$rc),
            obs = 
              lst(n = n_distinct(n$numero),
                  p = label_p()(n / scl$n$row)),
            item = 
              lst(n = n_distinct(n$name),
                  p = label_p()(n / scl$n$col))))

### FIG ------------------------------------------------------------------------

list(ab = list("Valeur", expr(fct_infreq(value))),
     na = list("Jours", expr(fct_inseq(visit)))) |> 
  imap(~ .value[[.y]]$n |>
         ggplot() +
         aes(x = eval(.x[[2]])) +
         geom_bar(alpha = 0.8) +
         geom_text(mapping =
                     aes(y = after_stat(count) + 0.05 * max(count),
                         label = after_stat(count)),
                   !!!opts$bar) |> inject() +
         geom_text(mapping = 
                     aes(y = after_stat(count) - 0.05 * max(count),
                         label = label_p()(after_stat(count) / sum(after_stat(count)))),
                   color = "grey95",
                   !!!opts$bar) |> inject() +
         labs(x = .x[[1]],
              y = opts$y) +
         theme_bar()) |> 
  easy_out_map(filename = "fig_value",
               size = c(3, 5.5))
