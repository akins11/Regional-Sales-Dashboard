#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  sales_data <- mod_overview_server(id = "overview")

  mod_product_all_server(id = "all_product_summary", s_data = sales_data)
  mod_product_single_server(id = "ind_product_summary", s_data = sales_data)

  mod_customer_all_server(id = "all_customer_summary", s_data = sales_data)
  mod_customer_single_server(id = "ind_customer_summary", s_data = sales_data)

  mod_sales_channel_server(id = "sales_channel_summary", s_data = sales_data)

  mod_sales_rep_server(id = "sales_team_summary", s_data = sales_data)
}
