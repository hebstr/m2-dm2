### DATA -----------------------------------------------------------------------

.contrib <-
lst(pc =
      list(j0 = c("PC1", "PC2", "PC3"),
           j56 = "PC1") |>
        imap(~ set_hdrs(.y) |>
               easy_pca() |>
               pca_var_extract() |>
               pluck("contrib") |>
               select(column, all_of(.x))),
    max =
      pc$j0 |>
      rowwise() |>
        mutate(column = as.integer(column),
               contrib = round(max(across(matches("PC"))) * 100, 1)) |>
        select(-matches("PC")),
    labels =
      c("humeur dépressive",
        "sentiment de culpabilité",
        "suicide",
        "insomnie début de nuit",
        "insomnie milieu de nuit",
        "insomnie du matin",
        "travail et activités",
        "ralentissement",
        "agitation",
        "anxiété psychique",
        "anxiété somatique",
        "symptômes gastro-intestinaux",
        "symptômes somatiques généraux",
        "symptômes génitaux",
        "hypocondrie",
        "perte de poids",
        "prise de conscience"),
    palette = c("white", opts$color$warm[2]),
    width = 320)

easy_sort <- \(x,
              ...,
              columns,
              target_columns = everything()) {

  columns <- enexpr(columns)
  target_columns <- enexpr(target_columns)

  x |>
    set_variable_labels("{columns}" := "contrib. (%)") |>
    gt_heatmap(...,
               width = .contrib$width) |>
    data_color(columns = !!columns,
               target_columns = !!target_columns,
               palette = .contrib$palette) |>
    tab_style(style = cell_text(align = "left"),
              locations =
                list(cells_column_labels("description"),
                     cells_body("description"))) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups())

}

### TBL ------------------------------------------------------------------------

.contrib$pc$j0 |>
  mutate(across(matches("PC"), ~ round(. * 100, 1))) |>
  gt_heatmap(rowname_col = "column",
             width = .contrib$width,
             palette = .contrib$palette) |>
  easy_out("tbl_contrib_pc")

list(j0 =
       tibble(item = 1:17,
              description = .contrib$labels,
              PC =
                case_match(item,
                           c(2, 3, 6, 9) ~ "PC1",
                           c(4, 5, 7, 12, 13, 14, 16) ~ "PC2",
                           c(1, 8, 10, 11, 15, 17) ~ "PC3")) |>
         left_join(.contrib$max,
                   by = join_by(item == column)) |>
         mutate(item = as.character(item)) |>
         relocate(PC, contrib,
                  .before = everything()) |>
         arrange(PC, desc(contrib)) |>
         easy_sort(groupname_col = "PC",
                   columns = contrib),
     j56 =
       tibble(PC1 = round(.contrib$pc$j56$PC1 * 100, 1),
              item = as.character(1:17),
              description = .contrib$labels) |>
         easy_sort(arrange = TRUE,
                   columns = PC1)) |>
  easy_out_map("tbl_contrib_sort")
