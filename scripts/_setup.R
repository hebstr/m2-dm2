### OPTS -----------------------------------------------------------------------

set_opts(
  acro = acro(PC ~ "composante principale"),
  y = "Effectif",
  bar = lst(
    stat = "count",
    size = 2.5,
    family = set_opts()$font
  )
)

### IMPORT ---------------------------------------------------------------------

easy_read <- \(x) {
  str_glue("data/{x}.xls") |>
    readxl::read_excel(na = c("", "ND")) |>
    set_names(tolower) |>
    mutate(numero = factor(numero))
}

easy_pca <- \(x) {
  x |>
    unite("numero_visit", c(numero, visit)) |>
    column_to_rownames("numero_visit") |>
    drop_na() |>
    prcomp(scale = TRUE)
}

.base <-
  c("hdrs", "scl") |>
  set_names() |>
  map(
    ~ easy_read(.) |>
      mutate(visit = tolower(visit))
  )

.groupe <- easy_read("groupe") |> mutate(groupe = factor(groupe))

### HDRS -----------------------------------------------------------------------

set_hdrs <- \(.visit = NULL) {
  .hdrs <-
    .base$hdrs |>
    mutate(
      hamd16 = coalesce(hamd16a, hamd16b),
      .keep = "unused",
      .after = hamd15
    )

  if (!is_null(.visit)) {
    .hdrs <- .hdrs |> filter(visit %in% .visit)
  }

  return(.hdrs)
}

hdrs <- lst(
  visit = lst(
    num = c(0, 4, 7, 14, 21, 28, 42, 56),
    str = paste0("j", num)
  ),
  item = map(set_names(visit$str), set_hdrs),
  pct = map_df(item, ~ label_p()(nrow(.) / nrow(item$j0))),
  sum = lst(
    long = set_hdrs() |>
      mutate(
        hdrs = rowSums(pick(matches("hamd"))),
        .after = visit,
        .keep = "unused"
      ) |>
      left_join(.groupe, by = "numero") |>
      drop_na(),
    wide = long |>
      pivot_wider(
        names_from = visit,
        values_from = hdrs
      )
  )
)

### SCL ------------------------------------------------------------------------

.dim <- lst(
  "somatisation" = c(1, 4, 12, 27, 40, 42, 48, 49, 52, 53, 56, 58),
  "traits obsessionnels" = c(3, 9, 10, 28, 38, 45, 46, 51, 55, 65),
  "vulnérabilité" = c(6, 21, 34, 36, 37, 41, 61, 69, 73),
  "dépression" = c(5, 14, 15, 20, 22, 26, 29, 30, 31, 32, 54, 71, 79),
  "anxiété" = c(2, 17, 23, 33, 39, 57, 72, 78, 80, 86),
  "hostilité" = c(11, 24, 63, 67, 74, 81),
  "phobies" = c(13, 25, 47, 50, 70, 75, 82),
  "traits paranoïaques" = c(8, 18, 43, 68, 76, 83),
  "traits psychotiques" = c(7, 16, 35, 62, 77, 84, 85, 87, 88, 90),
  "symptômes divers" = c(19, 44, 59, 60, 64, 66, 89)
) |>
  map(~ paste0("q", .))

is_norm <- \(x) x %in% c(0:4)

set_na <- \(x) {
  mutate(x, across(matches("q"), ~ if_else(!. %in% c(0:4, NA), NA, .)))
}

scl <- lst(
  base = .base$scl,
  n = lst(
    row = n_distinct(base$numero),
    col = base |> select(matches("q")) |> ncol(),
    rc = nrow(base) * col
  ),
  value = lst(
    data = base |>
      filter(!if_all(matches("q"), is_norm)) |>
      select(!where(~ unique(is_norm(.)) |> is_true())),
    total = base |>
      pivot_longer(cols = matches("q")) |>
      mutate(across(c(visit, value), ~ factor(.))) |>
      filter(!is_norm(value)),
    ab = drop_na(total),
    na = total |>
      filter(is.na(value)) |>
      mutate(
        visit = str_extract(visit, "(?<=j)\\d+"),
        value = 1
      )
  ),
  dim = names(.dim) |>
    map(
      ~ base |>
        set_na() |>
        transmute("{.}" := rowSums(across(.dim[[.]]), na.rm = TRUE))
    ) |>
    list_cbind(),
  join = base[c("numero", "visit")] |>
    bind_cols(dim) |>
    left_join(
      y = hdrs$sum$long,
      by = join_by(numero, visit)
    )
)

### AUTO EXEC ------------------------------------------------------------------

auto_exec()
