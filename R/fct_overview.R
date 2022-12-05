#' <list> of total and average summary.
#'
#' @param df data frame or a reactive data frame.
#' @param sumy_var variable to summarise.
#' @param desc_pl number of decimal points.
#'
#' @return a list of aggregations
#' @export
#'
#' @examples revenue_summary_list(r_df(), "profit" 2)
#'
revenue_summary_list <- function(df, sumy_var, desc_pl = 2) {

  var_vec <- df[[sumy_var]]

  lst_out <- list(total = scales::comma(sum(var_vec), accuracy = 0.1, prefix = "$"),
                  average = scales::comma(round(mean(var_vec), desc_pl),
                                          prefix = "$"))

  if (!sumy_var %in% c("sales", "discount")) {
    lst_out$percentage_sales <- paste0(round((sum(var_vec) / sum(df$sales))*100, desc_pl),
                                       "%")
    lst_out

  } else {
    lst_out
  }

}



#' <table> of the best category of product, customer, region, sales team
#'
#' @param df data frame or a reactive data frame.
#' @param arrange_by sort the table using what variable, default is 'sum'.
#'
#' @return a react table output.
#' @export
#'
#' @examples best_category(r_df())
#'
best_category <- function(df, arrange_by = "sum") {

  arrange_by <- match.arg(arrange_by, c("sum", "mean"))

  c_variables <- c("product_name", "customer_names", "state", "sales_team")

  c_variables2 <- c("Product", "Customer", "State", "Sales Person")

  f_tbl <- purrr::map2_dfr(c_variables, c_variables2, function(.x, .y) {
    df |>
      dplyr::group_by(.data[[.x]]) |>
      dplyr::summarise(mean = mean(profit), sum = sum(profit)) |>
      dplyr::arrange(desc(.data[[arrange_by]])) |>
      dplyr::rename(name = .data[[.x]]) |>
      dplyr::slice_head(n = 1) |>
      tibble::add_column(tibble::tibble(variable = .y), .before = 1)
  })

  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::no_lines(header_font_color = header_font_color),
    columns = list(
      variable = reactable::colDef(
        name = "Measure",
        cell = reactablefmtr::pill_buttons(data = f,
                                           colors = pill_buttons_color_2,
                                           text_size = 15,
                                           box_shadow = TRUE)
      ),
      name = reactable::colDef(
        name = "Name",
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_1,
                                           text_size = 15,
                                           box_shadow = TRUE)
      ),
      mean = reactable::colDef(
        name = "Average",
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_5,
                                           number_fmt = scales::label_comma(0.1, prefix = "$"),
                                           text_size = 18,
                                           text_color = text_color_white,
                                           box_shadow = TRUE)
      ),
      sum = reactable::colDef(
        name = "Total",
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_5,
                                           number_fmt = scales::label_comma(0.1, prefix = "$"),
                                           text_size = 18,
                                           text_color = text_color_white,
                                           box_shadow = TRUE)
      )
    )
  )
}



#' <plot> of variable summary be year.
#'
#' @param df data frame or a reactive data frame.
#' @param f_year the year to use.
#' @param sumy_var variable to summarise.
#'
#' @return a echarts4r line chart
#' @export
#'
#' @examples plt_revenue_year(r_df(), 2020, "discount")
#'
plt_revenue_year <- function(df, f_year, sumy_var) {
  if (!is.null(f_year)) {
    if (!f_year %in% c(2019, 2020, 2021)) {
      stop("`f_year` must be between 2019 and 2021")
    }

    p_name <- clean_text(sumy_var)

    if (sumy_var %in% c("discount", "cost")) {
      line_color <- neg_color

    } else {
      line_color <- pill_buttons_color_5
    }

    f_tbl <- df |>
      dplyr::filter(lubridate::year(orderdate) == f_year) |>
      dplyr::group_by(orderdate) |>
      dplyr::summarise(total = sum(.data[[sumy_var]]))

    highcharter::highchart() |>
      highcharter::hc_add_series(data = f_tbl,
                                 type = "spline",
                                 color = line_color,
                                 highcharter::hcaes(x = orderdate, y = total),
                                 name = p_name) |>
      highcharter::hc_xAxis(type = "datetime",
                            # zoomEnabled = TRUE,
                            dateTimeLabelFormats = list(day = "%m %d")) |>
      highcharter::hc_title(text = stringr::str_glue("Total {p_name} Across {f_year}"),
                            align = "left", style = list(color = title_color)) |>
      highcharter::hc_legend(enabled = FALSE)# |>
      # highcharter::hc_chart(zooming = list(type = "x"))

  }
}




ch_year <- function(y_all, y_19, y_20, y_21) {
  if (isTRUE(y_all)) {
    2019

  } else {
    yrs <- c("2019" = y_19, "2020" = y_20, "2021" = y_21)

    if (any(yrs)) {
      yrs[yrs == TRUE] |> names() |> as.numeric() |> min()

    } else {
      2019
    }
  }
}





# plt_revenue_year(s_df, 2019, "discount")


#' <plot> of cost break down.
#'
#' @param df data frame or a reactive data frame.
#'
#' @return a echarts4r doughnut chart
#' @export
#'
#' @examples plt_cost_breakdown(r_df())
#'
plt_cost_breakdown <- function(df) {

  df |>
    dplyr::summarise(Discount = sum(discount),
                     `Other Cost` = sum(order_quantity * unit_cost)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "cost", values_to = "value") |>

    echarts4r::e_charts(x = cost) |>
    echarts4r::e_pie(serie = value,
                     name = "Cost Type",
                     legend = FALSE,
                     radius = c("40%", "80%"),
                     itemStyle = list(borderRadius = 8,
                                      borderColor = '#fff',
                                      borderWidth = 2)) |>
    echarts4r::e_color(color = c("#333333", "#999999")) |>
    echarts4r::e_tooltip(
      formatter =  htmlwidgets::JS("
                                    function(params)
                                    {
                                        return `<strong>${params.name}</strong>
                                                <br/>Total: ${echarts.format.addCommas(params.value)}
                                                <br/>Percent: ${params.percent}%`
                                    }  ")
    ) |>
    echarts4r::e_title(text = "Cost Breakdown",
                       textStyle = list(color = title_color,
                                        fontWeight = "normal"),
                       top = "3%")
}




#' <plot> of profit received in each region.
#'
#' @param df data frame or a reactive data frame.
#'
#' @return a echarts4r bar chart.
#' @export
#'
#' @examples plt_profit_by_region(r_df())
#'
plt_profit_by_region <- function(df) {
  df |>
    dplyr::group_by(region) |>
    dplyr::summarise(profit = sum(profit)) |>
    dplyr::arrange(dplyr::desc(profit)) |>

    echarts4r::e_charts(x = region) |>
    echarts4r::e_bar(serie = profit,
                     name = "Profit",
                     legend = FALSE,
                     showBackground = TRUE,
                     itemStyle = list(borderRadius = 8,
                                      borderWidth = 2)) |>
    echarts4r::e_color(color = pill_buttons_color_5) |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter(style = "currency")) |>
    echarts4r::e_title(text = "Profit By Region",
                       textStyle = list(color = title_color,
                                        fontWeight = "normal")) |>
    echarts4r::e_y_axis(show = FALSE)
}





#' Add or subtract a month.
#'
#' @param selected_date date to perform operation.
#' @param add True to add a month false to subtract a month
#'
#' @return date type.
#' @export
#'
#' @examples add_sub_month("2020-05-24", FALSE)
#'
add_sub_month <- function(selected_date, add = TRUE) {
  sel_date <- as.Date(selected_date)

  if (isTRUE(add)) {
    lubridate::add_with_rollback(sel_date, months(1))

  } else {
    lubridate::`%m-%`(sel_date, months(1))
  }
}



#' Year on year
#'
#' @param df data frame or reactive data frame.
#' @param period either month or quarter.
#' @param period_value which particular month or quarter.
#' @param a_fun aggregate function.
#'
#' @return numeric value.
#' @export
#'
#' @examples get_yoy(r_df, "month", "March")
#'
get_yoy <- function(df, period, period_value, a_fun = "sum") {
  fun <- rlang::as_closure(a_fun)

  if (!missing(period_value)) {
    if (period == "month") {
      period_value <- lubridate::month(period_value, label = TRUE, abbr = FALSE)
    }
  }

  val <- lapply(
    c(2020, 2021),
    function(.x) {
      df |>
        dplyr::filter(year == .x & .data[[period]] == period_value) |>
        dplyr::summarise(value = fun(profit)) |>
        dplyr::pull()
    }
  )

  (val[[2]] - val[[1]]) / val[[1]]
}



#' Month to date.
#'
#' @param df data frame or reactive data frame.
#' @param start_date date to begin calculation.
#' @param end_date  final date of the calculation.
#' @param a_fun aggregate function.
#'
#' @return numeric value.
#' @export
#'
#' @examples get_mtd(r_df, "2020-11-22", "2020-12-21)
#'
get_mtd <- function(df, start_date, end_date, a_fun = "sum") {
  fun <- rlang::as_closure(a_fun)

  if (as.Date(end_date) > as.Date(start_date)) {
    df |>
      dplyr::filter(orderdate >= start_date & orderdate <= end_date) |>
      dplyr::summarise(value = fun(profit)) |>
      dplyr::pull()

  } else {
    0.0
  }
}





#' Month on month.
#'
#' @param df data frame or reactive data frame.
#' @param current_date current date for the calculation.
#' @param a_fun aggregate function.
#'
#' @return numeric value.
#' @export
#'
#' @examples get_mom(r_df, "2019-10-06")
#'
get_mom <- function(df, current_date, a_fun = "sum") {

  c_year  <- lubridate::year(current_date)
  c_month <- lubridate::month(current_date, label = TRUE, abbr = FALSE) |>
    as.character()

  p_period <- add_sub_month(current_date, FALSE)
  p_year <- lubridate::year(p_period)
  p_month  <- lubridate::month(p_period, label = TRUE, abbr = FALSE) |>
    as.character()

  fun <- rlang::as_closure(a_fun)

  val <- purrr::map2(
    c(p_year, c_year),
    c(p_month, c_month),

    function(.x, .y) {
      df |>
        dplyr::filter(year == .x & month == .y) |>
        dplyr::summarise(value = fun(profit)) |>
        dplyr::pull()
    }
  )

  (val[[2]] - val[[1]]) / val[[1]]
}




#' numeric label
#'
#' @param value numeric value
#' @param output_type type of output any of "percent", "dollar" or "number"
#'
#' @return character value.
#' @export
#'
#' @examples numeric_lab(2458974, "number")
#'
numeric_lab <- function(value, output_type = "dollar") {
  if (output_type == "percent") {
    scales::label_percent(0.01)(value)

  } else if (output_type == "dollar") {
    scales::label_number(accuracy = 0.01,
                         prefix = "$",
                         scale_cut = scales::cut_short_scale())(value)

  } else if (output_type == "number") {
    scales::label_number(accuracy = 0.01,
                         scale_cut = scales::cut_short_scale())(value)
  }
}
