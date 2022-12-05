# libraries --------------------------------------------------------------------
# library(shiny)
# library(shinyWidgets)
# library
# library(fontawesome)
# # library(bs4Dash)
# library(waiter)
# library(dplyr)
# library(stringr)
# # library(rlang)
# # library(purrr)
# # library(lubridate)
# library(echarts4r)
# library(highcharter)
# library(reactable)
# library(reactablefmtr)


# Picker input choices ---------------------------------------------------------
aggregate_functions <- c("Minimum" = "min",
                         "Average" = "mean",
                         "Median"  = "median",
                         "Maximum" = "max",
                         "Total"   = "sum")

location_names <- c("Region" = "region",
                    "State" = "state",
                    "County" = "county",
                    "City" = "city_name")

revenue_variables <- c("Sales" = "sales",
                      "Discount" = "discount",
                      "Cost" = "cost",
                      "Profit" = "profit")

date_variables <- c("Procurement Date" = "procureddate",
                    "Order Date" = "orderdate",
                    "Ship Date" = "shipdate",
                    "Delivery Date" = "deliverydate")

# Variable name ----------------------------------------------------------------
date_names <- list(
  procureddate = "Procurement",
  orderdate = "Order",
  shipdate = "Ship",
  deliverydate = "Delivery"
)

var_names <- list(
  "product_name" = "Product",
  "customer_names" = "Customer"
)

agg_names <- list(
  min = "minimum",
  Q25 = "25th quantile",
  mean = "average",
  median = "median",
  Q75 = "75th quantile",
  max = "maximum",
  sum = "total"
)

# Colors -----------------------------------------------------------------------
title_color <- "#D6D6D6"  #"#828282"
header_font_color <- "#858585"


pill_buttons_color_1 <- "#BDBDBD"
pill_buttons_color_2 <- "#ADADAD"
pill_buttons_color_3 <- "#707070"
pill_buttons_color_4 <- "#555555"
pill_buttons_color_5 <- "#333333"
pill_buttons_color_6 <- "#111111"

pill_buttons_bgcolor <- "#EEEEEE"

neg_color <- "#CD3333"
neg_bgcolor <- "#FFC0CB"

pos_color <- "#4EEE94"
pos_bgcolor <- "#C1FFC1"


text_color_white <- "#FFFFFF"
text_color_grey <- "#A1A1A1"
text_color_red <- "#FF4500"
text_color_green <- "#008B45"
text_color_black <- "#242424"

spinner_color <- "#ABABAB"


# useful names -----------------------------------------------------------------
agg_names <- list(
  min = "minimum",
  Q25 = "25th quantile",
  mean = "average",
  median = "median",
  Q75 = "75th quantile",
  max = "maximum",
  sum = "total"
)

var_names <- list(
  "product_name" = "Product",
  "customer_names" = "Customer"
)

date_names <- list(
  procureddate = "Procurement",
  orderdate = "Order",
  shipdate = "Ship",
  deliverydate = "Delivery"
)

q_values <- c(1, 2, 3, 4)
names(q_values) <- paste(c("1st", "2nd", "3rd", "4th"), "Quarter")

# Data -------------------------------------------------------------------------
# sales_orders <- read_csv("~/DataG/regional-sales/data/sales_orders_sheet.csv")
#
# files <- c("customers_sheet.csv", "products_sheet.csv", "store_locations_sheet.csv",
#            "regions_sheet.csv", "sales_team_sheet.csv")
# df_list <- purrr::map(
#   files,
#   ~read_csv(str_glue("~/DataG/regional-sales/data/{.x}"))
# )
#
# names(df_list) <- str_remove_all(files, ".csv")
#
#
#
# sales_orders <- sales_orders |>
#
#   # ref >> Profit Analysis >> Net Profit
#   mutate(sales    = order_quantity * unit_price,
#          discount = sales * discount_applied,
#          cost = (order_quantity * unit_cost) + discount,
#          profit   = sales - cost) |>
#
#   # ref >> Transaction Data Analysis
#   mutate(across(c(procureddate, orderdate, shipdate, deliverydate),
#                 \(.x) lubridate::add_with_rollback(.x, lubridate::years(1))))
#
# all_df <- sales_orders |>
#   inner_join(df_list$customers_sheet, by = "customerid") |>
#   inner_join(df_list$products_sheet, by = "productid") |>
#   inner_join(df_list$store_locations_sheet, by = "storeid") |>
#   inner_join(df_list$regions_sheet, by = "statecode") |>
#   inner_join(df_list$sales_team_sheet, by = "salesteamid") |>
#   select(-c(state.y, region.y)) |>
#   rename(state = state.x, region = region.x)
#
# write_csv(all_df,
#           "C:/Users/AYOMIDE/Documents/R/R_Projects/region sales analysis/region_sales/data/sales.csv")


