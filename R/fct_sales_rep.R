#' Plot Sales, Cost & Profit Summary for all sales team
#'
#' @param df data frame or reactive data frame.
#'
#' @param use the variable to summarise by either sales, cost or profit.
#'
#' @param s_fun the summary function.
#'
#' @return a echart4r bar chart / stacked bar chart.
#' @export
#'
#' @examples plt_sp_sales_profit_summary(r_df(), "profit", "mean")
#'
plt_sp_sales_profit_summary <- function(df, use, s_fun) {
  fun <- rlang::as_closure(s_fun)

  fun_lab <- clean_text(agg_names[[s_fun]])

  title_style <- list(color = title_color, fontWeight = "bold")

  if (use == "cost_profit") {
    df |>
      dplyr::group_by(sales_team) |>
      dplyr::summarise(profit = fun(profit), cost = fun(cost)) |>
      dplyr::arrange(dplyr::desc(cost)) |>

      echarts4r::e_charts(x = sales_team) |>
      echarts4r::e_bar(serie = cost, name = "Cost", stack = "grp") |>
      echarts4r::e_bar(serie = profit, name = "Profit", stack = "grp") |>
      echarts4r::e_color(color = c("#FCAB64", "#7FD8BE")) |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_title(text = stringr::str_glue("{fun_lab} Cost & Profit By Sales Person"),
                         textStyle = title_style) |>
      echarts4r::e_legend(right = 1) |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "currency"))

  } else if (use == "sales") {
    df |>
      dplyr::group_by(sales_team) |>
      dplyr::summarise(sales = fun(sales)) |>
      dplyr::arrange(dplyr::desc(sales)) |>

      echarts4r::e_charts(x = sales_team) |>
      echarts4r::e_bar(serie = sales, name = "Sales", legend = FALSE) |>
      echarts4r::e_color(color = c("#CDBA96")) |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_title(text = stringr::str_glue("{fun_lab} Sales By Sales Person"),
                         textStyle = title_style) |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "currency"))
  }
}



#' <Plot> The number of transaction in each region by sales team.
#'
#' @param f_df data frame or reactive data frame.
#'
#' @return a echart4r pie chart.
#' @export
#'
#' @examples plt_sp_trans_region(r_df())
#'
plt_sp_trans_region <- function(f_df) {
  f_df |>
    dplyr::count(region) |>

    echarts4r::e_chart(x = region) |>
    echarts4r::e_pie(serie = n, legend = FALSE,
                     itemStyle = list(
                       borderRadius = 8,
                       borderColor = '#fff',
                       borderWidth = 2)) |>
    echarts4r::e_color(color = c("#F9C784", "#BBDEF0", "#CBDFBD", "#FFCDB2")) |>
    echarts4r::e_title(text = "Number Of Transactions In Each Region",
                       textStyle = list(color = title_color,
                                        fontWeight = "bold")) |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
                                                      function(params)
                                                      {
                                                          return `<strong>${params.name}</strong>
                                                                  <br/>Total: ${echarts.format.addCommas(params.value)}
                                                                  <br/>Percent: ${params.percent}%`
                                                      } "))
}



#' <plot> sales, ..  by Sales team for each region.
#'
#' @param f_df data frame or reactive data frame.
#'
#' @param variable the variable to summarise by either sales, cost or profit etc.
#'
#' @param s_fun the summary function.
#'
#' @return a echart4r bar chart.
#' @export
#'
#' @examples plt_sp_region_revenue_summary(r_df(), "profit", "sum")
#'
plt_sp_region_revenue_summary <- function(f_df, variable, s_fun) {
  fun <- rlang::as_closure(s_fun)

  t_t <- stringr::str_glue("{clean_text(agg_names[[s_fun]])} {clean_text(variable)} By Region")

  f_df |>
    dplyr::group_by(region) |>
    dplyr::summarise(var = fun(.data[[variable]])) |>
    dplyr::arrange(desc(var)) |>

    echarts4r::e_charts(x = region) |>
    echarts4r::e_bar(serie = var,
                     name = "Total",
                     legend = FALSE,
                     showBackground = TRUE,
                     itemStyle = list(
                       borderRadius = 8,
                       borderWidth = 2)) |>
    echarts4r::e_color(color = pill_buttons_color_4) |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter(style = "currency")) |>
    echarts4r::e_y_axis(show = FALSE) |>
    echarts4r::e_title(text = t_t,
                       textStyle = list(color = title_color,
                                        fontWeight = "bold"))
}

