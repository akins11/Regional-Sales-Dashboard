#' <plot> number of transactions
#'
#' @param df data frame or a reactive data frame.
#' @param gp_var character variable with unique values to count.
#' @param top whether to count the top n or bottom n.
#' @param n number of observations to return.
#'
#' @return a echarts4r bar chart.
#' @export
#'
#' @examples count_chr_plt(r_df(), "product_name")
#'
count_chr_plt <- function(df, gp_var, top = TRUE, n = 10) {

  f_tbl <- count_chr(df, gp_var)

  if (isTRUE(top)) {
    f_plt <- head(f_tbl, n) |> dplyr::arrange(count)
  } else {
    f_plt <- tail(f_tbl, n)
  }

  t_b <- ifelse(isTRUE(top), "Top", "Bottom")

  var_name <- var_names[[gp_var]]

  p_tl <-
    stringr::str_glue("{t_b} {n} {var_name}s By Number Of Transaction")

  f_plt |>
    echarts4r::e_charts_(x = gp_var) |>
    echarts4r::e_bar_(serie = "count", name = var_name, legend = FALSE) |>
    echarts4r::e_color(color = pill_buttons_color_5) |>
    echarts4r::e_y_axis(show = FALSE) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_title(text = p_tl,
                       textStyle = list(color = title_color,
                                        fontWeight = "lighter")) |>
    echarts4r::e_tooltip() |>
    echarts4r::e_grid(left = "18%")
}



#' <plot> number of order for each product.
#'
#' @param df data frame or a reactive data frame.
#' @param sumy_fun aggregate function.
#' @param top whether to count the top n or bottom n.
#' @param n number of observations to return.
#'
#' @return a echarts4r bar chart.
#' @export
#'
#' @examples plt_product_quantity(r_df())
#'
plt_product_quantity <- function(df, sumy_fun = "sum", top = TRUE, n = 10) {

  f_tbl <- s_group_num_summary(df = df,
                               gp_var1 = "product_name",
                               sumy_var = "order_quantity",
                               sumy_fun = sumy_fun)

  if (isTRUE(top)) {
    f_tbl <- head(f_tbl, n) |> dplyr::arrange(.data[[sumy_fun]])
  } else {
    f_tbl <- tail(f_tbl, n)
  }

  t_b <- ifelse(isTRUE(top), "Top", "Bottom")

  fun_lab <- agg_names[[sumy_fun]] |> stringr::str_to_title()

  p_t <- stringr::str_glue("{t_b} {n} Products Based On {clean_text(fun_lab)} Number Of Order")

  f_tbl |>
    echarts4r::e_charts_(x = "product_name") |>
    echarts4r::e_bar_(serie = sumy_fun, name = "Total Order", legend = FALSE) |>
    echarts4r::e_color(color = pill_buttons_color_5) |>
    echarts4r::e_y_axis(show = FALSE) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_title(text = p_t,
                       textStyle = list(color = title_color,
                                        fontWeight = "lighter")) |>
    echarts4r::e_tooltip() |>
    echarts4r::e_grid(left = "20%")
}



#' <data frame> summarise product and quantity order.
#'
#' @param df data frame
#' @param gp_var variable to group by.
#' @param sumy_fun aggregate function.
#' @param sort whether to sort the table in ascending order.
#' @param group whether to keep the group index.
#'
#' @return a tibble.
#' @export
#'
#' @examples product_location_qty2(data, "region", "sum")
#'
product_location_qty2 <- function(df, gp_var, sumy_fun,
                                  sort = FALSE, group = FALSE) {
  fun <- rlang::as_closure(sumy_fun)

  fun_name <- agg_names[[sumy_fun]]

  col_name <- paste0(fun_name, "_quantity")

  dplyr::group_by(df, product_name, .data[[gp_var]]) |>
    dplyr::summarise("{col_name}" := fun(order_quantity)) |>
    pipe_cond(sort,
              ~dplyr::arrange(.x, product_name, .data[[gp_var]], dplyr::desc(.data[[col_name]])),
              ~.x) |>
    pipe_cond(group, ~.x, ~dplyr::ungroup(.x))
}




#' <table> showing the highest or least order quantity by product and geo location
#'
#' @param df data frame or a reactive data frame.
#' @param gp_var variable to group by.
#' @param sumy_fun aggregate function.
#' @param get_max whether to get the maximum value for each group if true or the minimum
#'
#' @return react table output.
#' @export
#'
#' @examples min_max_product_order(r_df(), gp_var = "region")
#'
min_max_product_order <- function(df,
                                  gp_var = "region", sumy_fun = "sum",
                                  get_max = TRUE) {

  f_tbl <- product_location_qty2(df = df,
                                 gp_var = gp_var,
                                 sumy_fun = sumy_fun,
                                 sort = TRUE, group = TRUE) |>
    pipe_cond(
      get_max,
      ~dplyr::slice_max(.x, order_by = .data[[gp_var]], n = 1, with_ties = FALSE),
      ~dplyr::slice_min(.x, order_by = .data[[gp_var]], n = 1, with_ties = FALSE)
    ) |>
    dplyr::rename(quantity = 3, product = product_name, gp_col = all_of(gp_var)) |>
    dplyr::mutate(quantity = round(quantity, 1)) |>
    dplyr::arrange(dplyr::desc(quantity))

  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::nytimes(font_size = 12, header_font_size = 12),
    showSortable = TRUE,

    columns = list(
      product = reactable::colDef(filterable = TRUE),
      gp_col = reactable::colDef(
        name = clean_text(gp_var),
        style = list(color = "#B3B3B3",
                     fontSize = 15),
        sortable = FALSE,
        maxWidth = 100,
      ),
      quantity = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "inside-base",
                                        fill_color = pill_buttons_color_5,
                                        text_color = text_color_white,
                                        text_size = 16,
                                        box_shadow = TRUE)
      )
    ),
    language = table_style(type = "lang", info_text = "Rows")
  )
}



#' <table> showing the order summary by product and geo location.
#'
#' @param df data frame or a reactive data frame.
#' @param gp_var2 second variable to group by with product.
#' @param sumy_fun aggregate function.
#'
#' @return a react table output.
#' @export
#'
#' @examples rtbl_product_quantity(r_Df(), "state", "sum")
#'
rtbl_product_quantity <- function(df, gp_var2 = "region", sumy_fun) {
  fun <- rlang::as_closure(sumy_fun)

  f_tbl <- df |>
    dplyr::group_by(product_name, .data[[gp_var2]]) |>
    dplyr::summarise(quantity = round(fun(order_quantity), 2)) |>
    dplyr::rename(loc_var = all_of(gp_var2)) |>
    dplyr::mutate(col_color = dplyr::case_when(quantity == max(quantity) ~ "#AFEEEE",
                                               quantity == min(quantity) ~ "#FF8C69",
                                               TRUE ~ "#F7F7F7"))

  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::nytimes(font_size = 12, header_font_size = 12),
    filterable = TRUE,
    showSortable = TRUE,
    defaultColDef = reactable::colDef(header = \(.x) clean_text(.x)),
    defaultPageSize = 8,
    columns = list(
      product_name = reactable::colDef(
        name = "Product",
        # style = reactablefmtr::group_border_sort("product_name"),
        align = "center",
        vAlign = "center"
      ),
      loc_var = reactable::colDef(
        name = clean_text(gp_var2),
        # cell = reactablefmtr::pill_buttons(data = f_tbl,
        #                                    # color_ref = "col_color",
        #                                    box_shadow = TRUE),
        align = "center",
      ),
      quantity = reactable::colDef(
        name = "Quantity",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "above",
                                        fill_color = pill_buttons_color_5),
        filterable = FALSE
      ),
      col_color = reactable::colDef(show = FALSE)
    ),
    language = table_style(type = "lang", info_text = "Groups")
  )
}



#' <table> showing a variable summary by product.
#'
#' @param df data frame or a reactive data frame.
#' @param sumy_var variable to summarise.
#'
#' @return a react table output.
#' @export
#'
#' @examples ftbl_product_summary_by(r_df(), "profit")
#'
ftbl_product_summary_by <- function(df, sumy_var) {

  f_tbl <- df |>
    dplyr::group_by(product_name) |>
    dplyr::summarise(average = mean(.data[[sumy_var]]),
                     total = sum(.data[[sumy_var]])) |>
    dplyr::mutate(average = round(average, 1)) |>
    dplyr::arrange(dplyr::desc(total))

  if (sumy_var %in% c("cost", "discount")) {
    bar_color <- neg_color
    bar_bgcolor <- neg_bgcolor

  } else {
    bar_color <- pos_color
    bar_bgcolor <- pos_bgcolor
  }

  num_lab <- scales::label_number(0.01,
                                  prefix = "$",
                                  scale_cut = scales::cut_short_scale())
  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::nytimes(), # font_size = 12, header_font_size = 12
    showSortable = TRUE,

    columns = list(
      product_name = reactable::colDef(
        name = "Product",
        filterable = TRUE
      ),
      average = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-base",
                                        fill_color = bar_color,
                                        background = bar_bgcolor,
                                        number_fmt = num_lab,
                                        text_color = text_color_black,
                                        text_size = 13,
                                        box_shadow = TRUE)
      ),
      total = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-base",
                                        fill_color = bar_color,
                                        background = bar_bgcolor,
                                        number_fmt = num_lab,
                                        text_color = text_color_black,
                                        text_size = 13,
                                        box_shadow = TRUE)
      )
    ),
    language = table_style(type = "lang", info_text = "Products")
  )
}




# ftbl_product_summary_by(s_df, "discount")

#' <table> showing the difference  between to date in days.
#'
#' @param df data frame or a reactive data frame.
#' @param date_var1 first date variable (must be the most recent date of the two date variable)
#' @param date_var2 second date variable.
#'
#' @return a react table output.
#' @export
#'
#' @examples rtbl_product_datediff(r_df(), "delivery_date", "procurement_date")
#'
rtbl_product_datediff <- function(df, date_var1, date_var2) {

  f_tbl <- date_diff_summary(df = df,
                             date_var1 = date_var1, date_var2 = date_var2,
                             sumy_var  = "date_diff",
                             gp_var    = "product_name",
                             multi_fun = TRUE) |>
    dplyr::select(product_name, minimum, average = mean, maximum) |>
    dplyr::mutate(average = round(average, 2))

  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::nytimes(header_font_color = "#A1A1A1"), #pagination_color = "#A1A1A1"
    defaultColDef = reactable::colDef(
      header = \(.x) stringr::str_to_title(.x),
    ),
    filterable = TRUE,
    defaultPageSize = 7,
    showSortable = TRUE,
    columns = list(
      product_name = reactable::colDef(
        name = "Product"
      ),
      minimum = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "above",
                                        fill_color = pill_buttons_color_1,
                                        text_size = 12,
                                        bold_text = TRUE,
                                        round_edges = TRUE,
                                        box_shadow = TRUE)
      ),
      average = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "above",
                                        fill_color = pill_buttons_color_2,
                                        text_color = "#A1A1A1",
                                        text_size = 13,
                                        bold_text = TRUE,
                                        round_edges = TRUE,
                                        box_shadow = TRUE)
      ),
      maximum = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "above",
                                        fill_color = pill_buttons_color_3,
                                        text_size = 15,
                                        bold_text = TRUE,
                                        round_edges = TRUE,
                                        box_shadow = TRUE)
      )
    ),
    language = table_style(type = "lang", info_text = "Products")
  )
}
