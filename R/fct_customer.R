#' <table> showing the number of time each customer made a purchase.
#'
#' @param df data frame of a reactive data frame.
#' @param page_size the initial number of rows to display default 11.
#'
#' @return a react table output.
#' @export
#'
#' @examples customer_purchase_freq(r_df())
#'
customer_purchase_freq <- function(df, page_size = 11) {

  f_tbl <- dplyr::count(df, customer_names, sort = TRUE, name = "count")

  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::sanfran(header_font_color = "#A1A1A1",
                                   pagination_color = "#A1A1A1",
                                   cell_color = "#FCFCFC"),
    defaultPageSize = page_size,
    showSortable = TRUE,

    columns = list(
      customer_names = reactable::colDef(
        name = "Customer",
        filterable = TRUE,
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_3,
                                           text_size = 13,
                                           text_color = text_color_white,
                                           bold_text = TRUE,
                                           box_shadow = TRUE)
      ),
      count = reactable::colDef(
        name = "Purchase Frequency",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-base",
                                        fill_color = pill_buttons_color_2,
                                        text_color = pill_buttons_color_3,
                                        text_size = 14,
                                        bold_text = TRUE,
                                        box_shadow = TRUE,
                                        round_edges = TRUE)
      )
    ),
    language = table_style(type = "lang", info_text = "Customers")
  )
}



#' <table> showing the summary of sales, cost & profit from each customer.
#'
#' @param df data frame of a reactive data frame.
#' @param s_fun the aggregate function.
#'
#' @return a react table output.
#' @export
#'
#' @examples customer_sales_profit_summary(r_df(), "sum")
#'
customer_sales_profit_summary <- function(df, s_fun) {

  fun <- rlang::as_closure(s_fun)

  f_tbl <- df |>
    dplyr::group_by(customer_names) |>
    dplyr::summarise(sales = fun(sales), cost = fun(cost), profit = fun(profit)) |>
    dplyr::arrange(dplyr::desc(profit))

  num_lab <- scales::label_number(0.01,
                                  prefix = "$",
                                  scale_cut = scales::cut_short_scale())
  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::sanfran(header_font_color = pill_buttons_color_2,
                                   cell_color = "#FCFCFC"),
    showSortable = TRUE,
    columns = list(
      customer_names = reactable::colDef(
        name = "Customer",
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_3,
                                           text_size = 13,
                                           text_color = text_color_white,
                                           box_shadow = TRUE)
      ),
      sales = reactable::colDef(
        name = "Sales",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "inside-base",
                                        text_color = text_color_white,
                                        number_fmt = num_lab,
                                        fill_color = pill_buttons_color_2)
      ),
      cost = reactable::colDef(
        name = "Cost",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "inside-base",
                                        number_fmt = num_lab,
                                        fill_color = neg_color,
                                        background = neg_bgcolor,
                                        align_bars = "right")
      ),
      profit = reactable::colDef(
        name = "Profit",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "inside-base",
                                        number_fmt = num_lab,
                                        fill_color = pos_color,
                                        background = pos_bgcolor)
      )
    ),
    columnGroups = list(
      reactable::colGroup(name = clean_text(agg_names[[s_fun]]),
                          columns = c("sales", "cost", "profit"),
                          headerStyle = list(color = pill_buttons_color_2,
                                             borderBottom = stringr::str_glue("1px solid {pill_buttons_color_2}")))
    ),
    language = table_style(type = "lang", info_text = "Customers")
  )
}



#' <plot> of variables by order date
#'
#' @param f_df data frame of a reactive data frame.
#' @param category the filtered category
#' @param by the variable to plot.
#' @param cum whether to use a cumulative sum or not.
#'
#' @return a echarts4r line and area chart.
#' @export
#'
#' @examples plt_order_date(r_df(), "aaa ltd", "profit")
#'
plt_order_date <- function(f_df, category, by, cum = FALSE) {

  if (by %in% c("discount", "cost")) {
    line_area_color <- neg_color
  } else if (by %in% c("sales", "profit")) {
    line_area_color <- pos_color
  } else {
    line_area_color <- pill_buttons_color_5
  }

  if (isFALSE(cum)) {
    c_name <- clean_text(by)

    t_t <- stringr::str_glue("{c_name} By Order Date")

    f_tbl <- f_df |>
      dplyr::group_by(orderdate) |>
      dplyr::summarise(total = sum(.data[[by]]))

    f_plt <- highcharter::highchart() |>
      highcharter::hc_add_series(data = f_tbl,
                                 type = "spline",
                                 color = line_area_color,
                                 highcharter::hcaes(x = orderdate, y = total),
                                 name = c_name) |>
      highcharter::hc_title(text = t_t,
                            align = "left",
                            style = list(color = title_color))

  } else {
    c_name <- stringr::str_glue("Commulative {clean_text(by)}")

    t_t <- stringr::str_glue("Cummulative {clean_text(by)} By Order Date")

    f_tbl <- f_df |>
      dplyr::mutate(cum = cumsum(.data[[by]])) |>
      dplyr::group_by(orderdate) |>
      dplyr::summarise(total = sum(cum))

    f_plt <- highcharter::highchart() |>
      highcharter::hc_add_series(data = f_tbl,
                                 type = "spline",
                                 color = line_area_color,
                                 highcharter::hcaes(x = orderdate, y = total),
                                 name = c_name) |>
      highcharter::hc_title(text = t_t,
                            align = "left",
                            style = list(color = title_color))
  }

  f_plt |>
    highcharter::hc_xAxis(type = "datetime",
                          dateTimeLabelFormats = list(day = "%m %d")) |>
    highcharter::hc_legend(enabled = FALSE) #|>
    # highcharter::hc_add_theme(highcharter::hc_theme_sandsignika())
}





#' <table> showing a description of the number of days since a customer last made a purchase.
#'
#' @param f_df data frame of a reactive data frame.
#' @param customer the customer name.
#'
#' @return a react table output.
#' @export
#'
#' @examples rtbl_customer_order_days(r_df(), "Ayomide Samuel")
#'
rtbl_customer_order_days <- function(f_df, customer) {

  f_tbl <- f_df |>
    dplyr::mutate(days_diff = orderdate - lag(orderdate),
                  days_diff = as.numeric(stringr::str_extract(days_diff, "[:digit:]+")),
                  days_diff = dplyr::if_else(is.na(days_diff), 0, days_diff)) |>
    numeric_summary("days_diff", FALSE) |>
    dplyr::select(-c(Q25, Q75, sum), Average = mean) |>
    dplyr::rename_with(clean_text) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "stat", values_to = "number_days") |>
    dplyr::mutate(label = stringr::str_glue("{round(number_days, 1)}  days"))

  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::sanfran(header_font_color = "#A1A1A1",
                                   pagination_color = "#A1A1A1",
                                   cell_color = "#FCFCFC"),
    columns = list(
      stat = reactable::colDef(
        name = "Aggregate",
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_3,
                                           text_size = 16,
                                           text_color = text_color_white,
                                           bold_text = TRUE,
                                           box_shadow = TRUE)
      ),
      label = reactable::colDef(
        name = "Duration",
        cell = reactablefmtr::color_tiles(data = f_tbl,
                                          c("#C7C7C7", "#ADADAD", "#949494"),
                                          color_by = "number_days",
                                          opacity = 0.9,
                                          text_size = 18,
                                          bold_text = TRUE,
                                          box_shadow = TRUE,
                                          text_color = text_color_white)
      ),
      number_days = reactable::colDef(show = FALSE)
    )
  )
}



#' <character> names
#'
#' @param cols a column names.
#'
#' @return character vector.
#' @export
#'
#' @examples
rename_cols <- function(cols) {
  if (any(cols == "city_name")) {
    cols[cols == "city_name"] <- "city"
  }
  clean_text(cols)
}



#' <table> showing a variable summary by location.
#'
#' @param f_df data frame of a reactive data frame.
#' @param gp_vars a variable to group by.
#' @param sumy_var a variable to summarise.
#' @param customer_name the name of the customer.
#' @param s_fun the aggregate function.
#'
#' @return a react table output.
#' @export
#'
#' @examples rtbl_sub_cloc_drilldown(r_df(), c("state", "county"), "profit", "Ayomide Samuel")
#'
rtbl_sub_cloc_drilldown <- function(f_df,
                                    gp_vars, sumy_var,
                                    customer_name, s_fun = "sum") {

  f_tbl <- f_df |>
    location_sub_summary(gp_vars = gp_vars, sumy_var = sumy_var, s_fun = s_fun) |>
    dplyr::rename_with(rename_cols) |>
    dplyr::arrange(dplyr::desc(Sum))

  if (min(f_tbl$Sum) < 0) {
    fill_clr <- c(neg_color, pos_color)

  } else {
    fill_clr <- pos_color
  }

  num_lab <- scales::label_number(0.01,
                                  prefix = "$",
                                  scale_cut = scales::cut_short_scale())
  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::nytimes(font_color = pill_buttons_color_2),
    filterable = TRUE,
    showSortable = TRUE,

    columns = list(
      Sum = reactable::colDef(
        name = stringr::str_glue("Total {clean_text(sumy_var)}"),
        minWidth = 150,
        align = 'center',
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        fill_color = fill_clr,
                                        brighten_text_color = "#222222",
                                        number_fmt = num_lab,
                                        text_position = 'inside-base',
                                        min_value  = min(f_tbl$Sum),
                                        max_value  = max(f_tbl$Sum),
                                        text_color = "#222222",
                                        bold_text  = TRUE,
                                        bar_height = 25,
                                        box_shadow = TRUE)
      )
    ),
    language = table_style(type = "lang", info_text = "Groups")
  )
}



