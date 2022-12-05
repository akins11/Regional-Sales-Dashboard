#' <plot> number of transaction involved in each sales channel.
#'
#' @param df a data frame or a reactive data frame.
#'
#' @return echarts4r doughnut chart.
#' @export
#'
#' @examples plt_sales_channel_count(r_df())
#'
plt_sales_channel_count <- function(df) {

  df |>
    dplyr::count(sales_channel, sort = TRUE, name = "count") |>
    dplyr::mutate(percentage = round(proportions(count)*100, 2)) |>

    echarts4r::e_charts(x = sales_channel) |>
    echarts4r::e_pie(serie = count,
                     name = "Sales Channel",
                     legend = FALSE,
                     radius = c("40%", "80%"),
                     itemStyle = list(borderRadius = 8,
                                      borderColor = '#fff',
                                      borderWidth = 2)) |>
    echarts4r::e_color(color = c("#FCAB64", "#A1FCDF", "#FAEDCD", "#F5CAC3")) |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
                                                      function(params)
                                                      {
                                                          return `<strong>${params.name}</strong>
                                                                  <br/>Total: ${echarts.format.addCommas(params.value)}
                                                                  <br/>Percent: ${params.percent}%`
                                                      } ")) |>
    echarts4r::e_title(text = "Number Of Transactions Done Through Each Channel",
                       textStyle = list(color = title_color,
                                        fontWeight = "normal"),
                       # top = "1%", bottom = "8%",
                       padding = c(5, 50, 7, 1))
}



#' <table> showing the number of transactions involved by each sales channel.
#'
#' @param df a data frame or a reactive data frame.
#' @param variable variable to count with sales channel.
#'
#' @return a react table output.
#' @export
#'
#' @examples ftbl_sales_channel_var_count(r_df(), "customer_names")
#'
ftbl_sales_channel_var_count <- function(df, variable) {

  f_tbl <- df |>
    dplyr::group_by(sales_channel, variable = .data[[variable]]) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(count))

  top_level <- f_tbl |>
    dplyr::slice_max(order_by = count) |>
    dplyr::summarise(highest = paste(variable, collapse = ", ")) |>
    dplyr::inner_join(
      dplyr::select(dplyr::slice_min(f_tbl, order_by = count) |>
                      dplyr::summarise(lowest = paste(variable, collapse = ", ")),
                    sales_channel, lowest),
      by = "sales_channel") |>
    dplyr::inner_join(dplyr::summarise(f_tbl, average = round(mean(count), 2)),
                      by = "sales_channel") |>
    dplyr::arrange(dplyr::desc(average))

  variable_nm <- list("product_name" = "Product",
                      "customer_names" = "Customer")

  reactable::reactable(
    data = top_level,
    showSortable = TRUE,
    theme = reactablefmtr::sanfran(header_font_color = "#A1A1A1",
                                   pagination_color = "#A1A1A1",
                                   cell_color = "#FCFCFC"),

    columnGroups = list(
      reactable::colGroup(name = "Number of Transcation",
                          columns = c("highest", "lowest", "average"),
                          headerStyle = list(color = pill_buttons_color_1,
                                             borderBottom = stringr::str_glue("1px solid {pill_buttons_color_1}")))
    ),
    columns = list(
      sales_channel = reactable::colDef(
        name = "Sales Channel",
        sortable = FALSE,
        cell = reactablefmtr::pill_buttons(data = top_level,
                                           colors = pill_buttons_color_3,
                                           text_color = text_color_white,
                                           text_size = 16,
                                           box_shadow = TRUE)
      ),
      highest = reactable::colDef(
        name = stringr::str_glue("Top {variable_nm[[variable]]}"),
        sortable = FALSE
      ),
      lowest = reactable::colDef(
        name = stringr::str_glue("Bottom {variable_nm[[variable]]}"),
        sortable = FALSE
      ),
      average = reactable::colDef(
        name = "Average",
        cell = reactablefmtr::data_bars(data = top_level,
                                        text_position = "center",
                                        fill_color = pill_buttons_color_3,
                                        text_color = text_color_white,
                                        box_shadow = TRUE,
                                        round_edges = TRUE)
      )
    ),
    # Second Level -----------------------------------------------------------||
    details = function(index) {
      sec_level <- f_tbl[f_tbl$sales_channel == top_level$sales_channel[index], ]

      reactable::reactable(
        data = sec_level,
        theme = reactablefmtr::sanfran(header_font_color = "#A1A1A1",
                                       pagination_color = "#A1A1A1",
                                       cell_color = "#FCFCFC"),
        filterable = TRUE,
        showSortable = TRUE,
        defaultPageSize = 5,

        columns = list(
          sales_channel = reactable::colDef(show = FALSE),

          variable = reactable::colDef(
            name = variable_nm[[variable]],
            cell = reactablefmtr::pill_buttons(data = sec_level,
                                               colors = pill_buttons_color_3,
                                               text_color = text_color_white,
                                               text_size = 13,
                                               box_shadow = TRUE)
          ),
          count = reactable::colDef(
            name = "Number Of Transcation",
            cell = reactablefmtr::data_bars(data = sec_level,
                                            text_position = "outside-base",
                                            fill_color = pill_buttons_color_2,
                                            text_color = pill_buttons_color_3,
                                            box_shadow = TRUE,
                                            round_edges = TRUE)
          )
        )
      )
    }
  )
}


#' <plot> of sales and profit summary by sales channel.
#'
#' @param df a data frame or a reactive data frame.
#' @param s_fun aggregate function.
#'
#' @return echarts4r radar chart.
#' @export
#'
#' @examples plt_sc_sales_profit(r_df(), "mean")
#'
plt_sc_sales_profit <- function(df, s_fun) {

  fun <- rlang::as_closure(s_fun)

  f_tbl <- df |>
    dplyr::group_by(sales_channel) |>
    dplyr::summarise(dplyr::across(c(sales, profit), sum))

  t_t <- stringr::str_glue("{clean_text(agg_names[[s_fun]])} Sales & Profit By Sales Channel")

  f_tbl |>
    echarts4r::e_charts(x = sales_channel) |>
    echarts4r::e_radar(serie = sales, name = "Sales", max = max(f_tbl$sales)) |>
    echarts4r::e_radar(serie = profit, name = "Profit", max = max(f_tbl$profit)) |>
    echarts4r::e_radar_opts(splitNumber = 4,
                            shape = "circle",
                            axisName = list(color = "#878787",
                                            fontWeight = "bold"),
                            splitArea = list(areaStyle = list(color = c("#FFFFFF", "#FCFCFC")))) |>
    echarts4r::e_color(color = c("#CDBA96", "#7FD8BE")) |>
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_legend(right = 1) |>
    echarts4r::e_title(text = t_t,
                       textStyle = list(color = title_color,
                                        fontWeight = "normal"),
                       padding = c(0, 0, 20, 0))
}

