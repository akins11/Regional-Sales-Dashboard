#' <character> replace underscore with space and convert string to title.
#'
#' @param string a string to clean.
#'
#' @return a character vector.
#' @export
#'
#' @examples clean_text("customer_name")
#'
clean_text <- function(string) {
  stringr::str_replace_all(string, "_", " ") |>
    stringr::str_to_title()
}



#' <data frame> of numeric summary.
#'
#' @param df a data frame.
#' @param var a numeric variable to summarise.
#' @param include_count whether to include the count.
#' @param groups how to treat groupings if available.
#' @param pivot whether to convert the table to a long format (useful when there are no groupings)
#'
#' @return a tibble.
#' @export
#'
#' @examples numeric_summary(data, "profit")
#'
numeric_summary <- function(df, var,
                            include_count = TRUE, groups = "drop_last",
                            pivot = FALSE) {
  if (!is.numeric(df[[var]])) {
    stop("arguemnt `var` only allows numeric variables")
  }
  f_tbl <- df |>
    dplyr::summarise(count   = dplyr::n(),
                     minimum = min(.data[[var]], na.rm = TRUE),
                     Q25     = quantile(.data[[var]], 0.25, na.rm = TRUE),
                     mean    = mean(.data[[var]], na.rm = TRUE),
                     median  = median(.data[[var]], na.rm = TRUE),
                     Q75     = quantile(.data[[var]], 0.75, na.rm = TRUE),
                     maximum = max(.data[[var]], na.rm = TRUE),
                     sum     = sum(.data[[var]], na.rm = TRUE),
                     .groups = groups)

  if (isTRUE(include_count)) f_tbl else f_tbl <- dplyr::select(f_tbl, -count)

  if (isTRUE(pivot)) {
    f_tbl |>
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to  = "statistics",
                          values_to = "value")
  } else {
    f_tbl
  }
}


#' <data frame> Filter Date
#'
#' @param df a data frame of reactive data frame.
#' @param y_all all years
#' @param y_19 2019
#' @param y_20 2020
#' @param y_21 2021
#'
#' @return a subset of the data when a date is true and y_all is false else the
#' data
#' @export
#'
#' @examples
filter_period <- function(df,
                          y_all = TRUE,
                          y_19 = FALSE, y_20 = FALSE, y_21 = FALSE) {

  if (isTRUE(y_all) && isFALSE(y_19) && isFALSE(y_20) && isFALSE(y_21)) {
    df

  } else if (isFALSE(y_all) && (isTRUE(y_19) || isTRUE(y_20) || isTRUE(y_21))) {
    yrs <- c("2019" = y_19, "2020" = y_20, "2021" = y_21)

    if (any(yrs)) {
      ch_yrs <- yrs[yrs == TRUE] |> names() |> as.numeric()

      if (length(ch_yrs) == 1) {
        dplyr::filter(df, lubridate::year(orderdate) == ch_yrs)

      } else if (length(ch_yrs) > 1) {
        dplyr::filter(df, lubridate::year(orderdate) %in% ch_yrs)

      } else {
        df
      }
    } else {
      df
    }
  } else if (isTRUE(y_all) && (isTRUE(y_19) || isTRUE(y_20) || isTRUE(y_21))) {
    df

  } else {
    df
  }
}


#' Table theme and language
#'
#' @param type type of output, either "theme" or "lang".
#' @param cell_padding the amount of top, bottom, left & right padding for each cell.
#' @param info_text additional info for the table pagination.
#'
#' @return a react table output.
#' @export
#'
#' @examples reactable(theme = table_style(type = "theme"))
#'
table_style <- function(type = "theme", cell_padding = 6, info_text = "entries") {
  if (type == "theme") {
    reactable::reactableTheme(
      backgroundColor = "#ffffff",
      color = "#787878",
      borderWidth = "1px",
      borderColor = "#EDEDED",
      stripedColor = "#FCFCFC",
      cellPadding = cell_padding,

      cellStyle = list(display = "flex",
                       flexDirection = "column",
                       justifyContent = "center"
      ),

      tableStyle = list(fontSize = 15),
      headerStyle = list(borderWidth = "1px",
                         padding = "5px",

                         background = "#FFFFFF",
                         borderColor = "#828282",
                         fontWeight = "600",
                         fontSize = 16,
                         color = "#666666"),

      inputStyle = NULL,
      rowSelectedStyle = NULL,
      selectStyle = NULL,
      paginationStyle = NULL,

      style = list(
        fontFamily = tbl_font_family
      )
    )

  }  else if (type == "lang") {
    reactable::reactableLang(pageInfo = stringr::str_glue("{{rows}} {info_text}"),
                             pagePrevious = "\u276e",
                             pageNext = "\u276f")
  }
}



#' Title
#'
#' @param y_all
#' @param y_19
#' @param y_20
#' @param y_21
#'
#' @return
#' @export
#'
#' @examples
show_hide <- function(y_all = TRUE, y_19 = FALSE, y_20 = FALSE, y_21 = FALSE) {
  if (isTRUE(y_all)) {
    TRUE

  } else {
    yrs <- c(y_19, y_20, y_21)

    if (any(yrs)) {
      len <- length(yrs[yrs == TRUE])

      if (len == 1) {
        FALSE

      } else if (len > 1) {
        TRUE

      } else {
        FALSE
      }
    }
  }
}



#' <data frame> use if else condition with pipe operator.
#'
#' @param .data a data frame.
#' @param condition a condition that returns a Boolean.
#' @param .true the value to return if the condition is true.
#' @param .false the value to return if the condition is false.
#'
#' @return a tibble.
#' @export
#'
#' @examples pipe_cond(data, max(profit) > 1000, ~group_by(.x, sale_channel), ~.x)
#'
pipe_cond <- function(.data, condition, .true, .false = identity) {

  if (isTRUE(condition)) {
    call <- rlang::as_closure(.true)

  } else {
    call <- rlang::as_closure(.false)
  }

  do.call(call, rlang::list2(.data))
}



#' <data frame> count the number of observation in each character category.
#'
#' @param df a data frame.
#' @param count_var the character variable to count.
#' @param add_percent whether to add each category proportion of the total column.
#'
#' @return a tibble
#' @export
#'
#' @examples  count_chr(data, "sales_team")
#'
count_chr <- function(df, count_var, add_percent = TRUE) {

  s_tbl <- df |>
    dplyr::count(.data[[count_var]], sort = TRUE, name = "count")

  if (add_percent) {
    s_tbl |>
      dplyr::mutate(percentage = round(proportions(count)*100, 2))

  } else {
    s_tbl
  }
}



#' <data frame> a single description numeric summary.
#'
#' @param df a data frame.
#' @param gp_var1 first variable to group by.
#' @param gp_var2 second variable to group by.
#' @param sumy_var the variable to summarise.
#' @param sumy_fun the aggregate function.
#' @param sort whether to sort the values in ascending format.
#' @param group whether to keep the group index.
#'
#' @return a tibble
#' @export
#'
#' @examples s_group_num_summary(data, "region", "sales_channel", "profit", "mean")
#'
s_group_num_summary <- function(df,
                                gp_var1, gp_var2,
                                sumy_var, sumy_fun,
                                sort = TRUE, group = FALSE) {

  fun <- rlang::as_closure(sumy_fun)

  if (missing(gp_var2)) {
    s_tbl <- dplyr::group_by(df, .data[[gp_var1]])

  } else {
    s_tbl <- dplyr::group_by(df, .data[[gp_var1]], .data[[gp_var2]])
  }

  dplyr::summarise(s_tbl, "{sumy_fun}" := fun(.data[[sumy_var]])) |>
    pipe_cond(sort, ~dplyr::arrange(.x, dplyr::desc(.data[[sumy_fun]])), ~.x) |>
    pipe_cond(group, ~.x, ~dplyr::ungroup(.x))
}



#' <data frame> with the difference in days between to date variables.
#'
#' @param df data frame.
#' @param date_var1 first date variable (the most recent of the two date).
#' @param date_var2 second date variable.
#' @param sumy_var the variable to summarise by
#' @param sumy_fun the aggregate function.
#' @param gp_var if supplied, the variable to group by.
#' @param multi_fun whether to return multiple description or just 1 description.
#'
#' @return a tibble.
#' @export
#'
#' @examples date_diff_summary(data, "delivery_date", "order_date", "profit")
#'
date_diff_summary <- function(df,
                              date_var1, date_var2,
                              sumy_var, sumy_fun = "mean",
                              gp_var,
                              multi_fun = TRUE) {

  f_tbl <- dplyr::mutate(
    df,
    date_diff = .data[[date_var1]] - .data[[date_var2]],
    date_diff = as.numeric(stringr::str_remove_all(date_diff, "[:alpha:]"))
  ) |>
    pipe_cond(!missing(gp_var), ~dplyr::group_by(.x, .data[[gp_var]]), ~.x)

  if (!missing(sumy_var)) {
    if (multi_fun) {
      numeric_summary(f_tbl, sumy_var)

    } else {
      fun <- rlang::as_closure(sumy_fun)
      col_name <- agg_names[[sumy_fun]]

      dplyr::summarise(f_tbl, "{col_name}" := fun(.data[[sumy_var]]))
    }

  } else {
    f_tbl
  }
}



#' <character> returns the appropriate variable based on x
#'
#' @param x a variable name.
#' @param recent whether it is a recent date variable name or not.
#'
#' @return a character vector.
#' @export
#'
#' @examples dyn_date("orderdate")
#'
dyn_date <- function(x, recent = TRUE) {

  if (recent) {
    if (x == "procureddate") {
      c("Procurement Date" = "procureddate",
        "Order Date" = "orderdate",
        "Ship Date" = "shipdate")
    } else if (x == "orderdate") {
      c("Ship Date" = "shipdate", "Delivery Date" = "deliverydate")
    } else if (x == "shipdate") {
      c("Delivery Date" = "deliverydate")
    } else if (x == "deliverydate") {
      NULL
    }

  } else {
    if (x == "procureddate") {
      NULL
    } else if (x == "orderdate") {
      c("Procurement Date" = "procureddate")
    } else if (x == "shipdate") {
      c("Procurement Date" = "procureddate",  "Order Date" = "orderdate")
    } else if (x == "deliverydate") {
      c("Procurement Date" = "procureddate",
        "Order Date" = "orderdate",
        "Ship Date" = "shipdate")
    }
  }
}



#' <plot> the percentage of profit and cost.
#'
#' @param df data frame of reactive data frame.
#'
#' @return a echarts4r doughnut chart.
#' @export
#'
#' @examples plt_percent_profit_cost(r_df())
#'
plt_percent_profit_cost <- function(df) {

  df |>
    dplyr::summarise(profit = sum(profit), cost = sum(cost)) |>
    tidyr::pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>

    echarts4r::e_charts(x = variable) |>
    echarts4r::e_pie(serie = value,
                     name = "Sales",
                     legend = FALSE,
                     radius = c("40%", "80%"),
                     itemStyle = list(
                       borderRadius = 8,
                       borderColor = '#fff',
                       borderWidth = 2)) |>
    echarts4r::e_color(c("#7FD8BE", "#FCAB64")) |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
                                                      function(params)
                                                      {
                                                          return `<strong>${params.name}</strong>
                                                                  <br/>Total: ${echarts.format.addCommas(params.value)}
                                                                  <br/>Percent: ${params.percent}%`
                                                      } ")) |>
    echarts4r::e_title(text = "Percentage of Cost & Profit",
                       textStyle = list(color = title_color,
                                        fontWeight = "normal"),
                       top = "2%")
}




#' <data frame> filter a variable based on a category.
#'
#' @param df data frame.
#' @param variable the variable to filter.
#' @param category a sunset of the variable to keep.
#'
#' @return a tibble.
#' @export
#'
#' @examples filter_variable(df, "customer_names", "Ayomide Samuel")
#'
filter_variable <- function(df, variable, category) {

  if (length(category) == 1) {
    dplyr::filter(df, .data[[variable]] == category)

  } else {
    dplyr::filter(df, .data[[variable]] %in% category)
  }
}




#' <data frame> summarise a variable geographic location.
#'
#' @param f_df data frame or reactive data frame.
#' @param gp_vars geo location to group by.
#' @param sumy_var  the summary variable.
#' @param s_fun the aggregate function.
#' @param group whether to keep the group index.
#'
#' @return a tibble.
#' @export
#'
#' @examples location_sub_summary(r_df(), c("region", "state", "county"), "profit")
#'
location_sub_summary <- function(f_df,
                                 gp_vars, sumy_var,
                                 s_fun = "sum", group = FALSE) {

  lvl <- c("region", "state", "county", "city_name")
  order_vars <- gp_vars[order(match(gp_vars, lvl))]

  fun <- rlang::as_closure(s_fun)

  f_df |>
    dplyr::group_by(dplyr::across(.cols = dplyr::all_of(order_vars))) |>
    dplyr::summarise("{s_fun}" := round(fun(.data[[sumy_var]]), 2),
                     .groups = dplyr::if_else(isTRUE(group), "keep", "drop"))
}



#' <list> of variable summary.
#'
#' @param df data frame or reactive data frame.
#' @param variable the variable to summarise.
#' @param col_name name of the additional summary column.
#' @param sum_var the additional summary variable.
#' @param f_data if supplied the filtered data frame.
#' @param currency whether to format using currency.
#'
#' @return a list of aggregate summary.
#' @export
#'
#' @examples sub_summary(data, "order_quantity", "percentage_total", "order_quantity')
#'
sub_summary <- function(df, variable,
                        col_name, sum_var, f_data = NULL,
                        currency = FALSE) {

  f_lst <- df |>
    dplyr::summarise(sum_value = sum(.data[[variable]]),
                     average_value = mean(.data[[variable]])) |>
    pipe_cond(
      !is.null(f_data),
      ~dplyr::mutate(.x, "{col_name}" := (sum_value / sum(f_data[[sum_var]]))*100),
      ~dplyr::mutate(.x, "{col_name}" := (sum_value / sum(df[[sum_var]]))*100)
    ) |>
    as.list()

  if (currency) {
    f_lst[1:2] <- lapply(f_lst[1:2], \(.x) scales::dollar(.x, 0.01))
    f_lst[[3]] <- paste0(round(f_lst[[3]], 2), "%")
    f_lst

  } else {
    f_lst <- lapply(f_lst, \(.x) scales::comma(.x, 0.01))
    f_lst[[3]] <- paste0(f_lst[[3]], "%")
    f_lst
  }
}



#' <list> of variable summary.
#'
#' @param filtered_data filtered data frame or reactive data frame.
#' @param full_data the unfiltered data frame or reactive data frame.
#' @param variable variable to summarise.
#'
#' @return a list of aggregate summary.
#' @export
#'
#' @examples sub_category_summary(fr_df(), r_df(), "cost")
#'
sub_category_summary <- function(filtered_data, full_data = NULL, variable) {
  if (variable == "order_quantity") {
    sub_summary(df = filtered_data, variable = "order_quantity",
                col_name = "percentage_total", sum_var = "order_quantity",
                f_data = full_data, currency = FALSE)

  } else if (variable == "transaction") {
    dplyr::tibble(n_value = nrow(filtered_data),
                  percentage_total = round((nrow(filtered_data) / nrow(full_data))*100, 2)) |>
      dplyr::mutate(percentage_total = paste0(percentage_total, "%")) |>
      as.list()

  } else if (variable == "discount") {
    sub_summary(df = filtered_data, variable = "discount",
                col_name = "percentage_cost", sum_var = "cost",
                f_data = full_data, currency = TRUE)

  } else if (variable == "sales") {
    sub_summary(df = filtered_data, variable = "sales",
                col_name = "percentage_sales", sum_var = "sales",
                f_data = full_data, currency = TRUE)

  } else if (variable == "cost") {
    sub_summary(df = filtered_data, variable = "cost",
                col_name = "percentage_own_sales", sum_var = "sales",
                currency = TRUE)

  } else if (variable == "profit") {
    sub_summary(df = filtered_data, variable = "profit",
                col_name = "percentage_own_sales", sum_var = "sales",
                currency = TRUE)
  }
}



#' <character> a vector of unique values.
#'
#' @param f_df data frame or reactive data frame.
#' @param cat_var a character variable.
#' @param sample whether to return a sample.
#'
#' @return character vector.
#' @export
#'
#' @examples get_filter_unique_cat(r_df(), "region")
#'
get_filter_unique_cat <- function(f_df, cat_var, sample = TRUE) {

  f_tbl <- f_df |>
    dplyr::group_by(.data[[cat_var]]) |>
    dplyr::summarise(total = sum(profit))

  if (isTRUE(sample)) {
    f_tbl |>
      dplyr::slice_max(order_by = total, n = 6) |>
      dplyr::pull(cat_var)

  } else {
    dplyr::pull(f_tbl, cat_var)
  }

}



#' <plot> of sales, cost & profit by geo location.
#'
#' @param f_df a filtered data frame or reactive data frame.
#' @param fl_var the filtered variable
#' @param category the subset of the variable that was filtered.
#' @param gp_var the variable to group by.
#' @param s_fun the aggregate function.
#' @param gp_sample if the unique values of the grouped variable is greater than 5, then a sample of the values.
#'
#' @return a echarts4r bar chart.
#' @export
#'
#' @examples plt_ser_summary(r_df(), "customer_name", "Ayomide Samule", "region")
#'
plt_ser_summary <- function(f_df,
                            fl_var, category,
                            gp_var, s_fun = "sum", gp_sample) {

  if (length(unique(f_df[[gp_var]])) > 5 && missing(gp_sample)) {
    stop("Grouped variable has more than 5 categories and `gp_sample` is missing")
  }

  fun <- rlang::as_closure(s_fun)

  f_tbl <- f_df |>
    dplyr::group_by(.data[[gp_var]]) |>
    dplyr::summarise(dplyr::across(.cols = c(sales, cost, profit),
                                   .fns = fun)) |>
    pipe_cond(
      length(unique(f_df[[gp_var]])) > 5 & !missing(gp_sample),
      ~dplyr::filter(.x, .data[[gp_var]] %in% gp_sample),
      ~.x
    )

  t_t <- stringr::str_glue("{var_names[[fl_var]]} :: {clean_text(category)}")

  f_tbl |>
    echarts4r::e_charts_(x = gp_var) |>
    echarts4r::e_bar(serie = sales) |>
    echarts4r::e_bar(serie = cost) |>
    echarts4r::e_bar(serie = profit) |>
    echarts4r::e_color(color = c("#E0D5BE", "#FCAB64", "#7FD8BE")) |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter("currency")) |>
    echarts4r::e_legend(right = 0) |>
    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("currency")) |>
    echarts4r::e_title(text = t_t,
                       textStyle = list(color = title_color,
                                        fontWeight = "normal")) |>
    echarts4r::e_theme(name = "macarons2")
}



#' <table> showing the summary of quantity order by products or customer.
#'
#' @param f_df data frame or a reactive data frame.
#' @param count_var the character variable to count either product or customer name.
#' @param s_fun aggregate function.
#'
#' @return react table output.
#' @export
#'
#' @examples rtbl_sub_pc_summary(r_df(), "customer_names")
#'
rtbl_sub_pc_summary <- function(f_df, count_var, s_fun = "sum") {

  fun <- rlang::as_closure(s_fun)

  f_tbl <- f_df |>
    dplyr::group_by(.data[[count_var]]) |>
    dplyr::summarise(count = dplyr::n(),
                     quantity_order = round(fun(order_quantity), 2)) |>
    dplyr::arrange(dplyr::desc(count)) |>
    dplyr::rename(named_col = all_of(count_var))

  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::sanfran(header_font_color = "#A1A1A1",
                                   pagination_color = "#A1A1A1",
                                   cell_color = "#FCFCFC"),
    filterable = TRUE,
    showSortable = TRUE,
    columns = list(
      named_col = reactable::colDef(
        name = var_names[[count_var]],
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_3,
                                           text_color = text_color_white,
                                           text_size = 13,
                                           box_shadow = TRUE)
      ),
      count = reactable::colDef(
        name = "Number of Transactions",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-base",
                                        fill_color = pill_buttons_color_2,
                                        text_color = text_color_grey,
                                        text_size = 14,
                                        bold_text = TRUE,
                                        box_shadow = TRUE,
                                        round_edges  = TRUE)
      ),
      quantity_order = reactable::colDef(
        name = stringr::str_glue("{clean_text(agg_names[[s_fun]])} Order Quantity"),
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-base",
                                        fill_color = pill_buttons_color_2,
                                        text_color = text_color_grey,
                                        number_fmt = scales::label_comma(0.01),
                                        text_size = 14,
                                        bold_text = TRUE,
                                        box_shadow = TRUE,
                                        round_edges  = TRUE)
      )
    ),
    language = table_style(type = "lang", info_text = "Customers")
  )
}



#' <table> showing the summary based on sales, cost & profit.
#'
#' @param f_df data frame or a reactive data frame.
#' @param count_var variable to summarise.
#' @param s_fun the aggregate function.
#' @param bold_name name the first column name bold.
#'
#' @return react table output.
#' @export
#'
#' @examples rtbl_sub_sale_profit(r_Df(), "product_name")
#'
rtbl_sub_sale_profit <- function(f_df, count_var, s_fun = "sum", bold_name = FALSE) {

  fun <- rlang::as_closure(s_fun)

  f_tbl <- f_df |>
    dplyr::group_by(.data[[count_var]]) |>
    dplyr::summarise(sales = round(fun(sales), 2),
                     cost  = round(fun(cost), 2),
                     profit = round(fun(profit), 2)) |>
    dplyr::rename(named_col = all_of(count_var)) |>
    dplyr::arrange(dplyr::desc(sales))

  if (min(f_tbl$profit) < 0) {
    profit_fill_color <- c(neg_color, pos_color)
    text_pos <- "inside-base"

  } else {
    profit_fill_color <- pos_color
    text_pos <- "outside-base"
  }

  if (isTRUE(bold_name)) {
    name_color <- pill_buttons_color_6
    font_size <- 21

  } else {
    name_color <- "#A1A1A1"
    font_size <- 15
  }

  num_lab <- scales::label_number(0.01,
                                  prefix = "$",
                                  scale_cut = scales::cut_short_scale())
  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::sanfran(header_font_color = "#A1A1A1",
                                   pagination_color = "#A1A1A1",
                                   cell_color = "#FCFCFC"),
    showSortable = TRUE,
    columns = list(
      named_col = reactable::colDef(
        name = var_names[[count_var]],
        filterable = TRUE,
        headerStyle = list(color = name_color, fontSize = font_size),
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = pill_buttons_color_3,
                                           text_color = text_color_white,
                                           text_size = 13,
                                           box_shadow = TRUE)
      ),
      sales = reactable::colDef(
        name = "Sales",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-base",
                                        fill_color = pill_buttons_color_2,
                                        text_color = text_color_grey,
                                        number_fmt = num_lab,
                                        text_size = 14,
                                        min_value = min(f_tbl$sales),
                                        max_value = max(f_tbl$sales),
                                        bold_text = TRUE,
                                        box_shadow = TRUE)
      ),
      cost = reactable::colDef(
        name = "Cost",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-base",
                                        fill_color = neg_color,
                                        text_color = text_color_red,
                                        number_fmt = num_lab,
                                        background = neg_bgcolor,
                                        min_value = min(f_tbl$cost),
                                        max_value = max(f_tbl$cost),
                                        align_bars = "right",
                                        text_size = 14,
                                        bold_text = TRUE,
                                        box_shadow = TRUE)
      ),
      profit = reactable::colDef(
        name = "Profit",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = text_pos,
                                        fill_color = profit_fill_color,
                                        text_color = "#000000",
                                        brighten_text_color = "#000000",
                                        min_value = min(f_tbl$profit),
                                        max_value = max(f_tbl$profit),
                                        number_fmt = num_lab,
                                        background = pos_bgcolor,
                                        text_size = 13,
                                        bold_text = TRUE,
                                        box_shadow = TRUE)
      )
    ),
    columnGroups = list(
      reactable::colGroup(name = clean_text(agg_names[[s_fun]]),
                          columns = c("sales", "cost", "profit"),
                          headerStyle = list(color = "#A1A1A1"))
    ),
    language = table_style(type = "lang", info_text = "Customers")
  )
}

