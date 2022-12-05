#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    bslib::page_navbar(

      title = shiny::tags$div(
        shiny::tags$a(
          shiny::tags$img(src="www/dib_logo.png", class = "logo-pos", height = 70),
          href = "#"
        ),
        "Regional Sales Dashboard"
      ),

      windowTitle = "Region Dashboard",

      header = shiny::tags$head(
        shinyjs::useShinyjs(),
        shinyWidgets::useBs4Dash(),
        waiter::useWaiter(),

        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),

      bslib::nav_spacer(), bslib::nav_spacer(), bslib::nav_spacer(),

      shiny::tabPanel(
        title = "Overview",
        value = "tab_home_overview",

        mod_overview_ui(id = "overview")
      ),

      shiny::navbarMenu(
        title = "Product",
        shiny::tabPanel(
          icon  = fontawesome::fa_i("fas fa-cart-plus"),
          title = "All Products",
          value = "tab_all_product_summary",

          mod_product_all_ui(id = "all_product_summary")
        ),
        shiny::tabPanel(
          icon  = fontawesome::fa_i("fas fa-cart-shopping"),
          title = "Single Product",
          value = "tab_individual_product_summary",

          mod_product_single_ui(id = "ind_product_summary")
        ),
      ),

      shiny::navbarMenu(
        title = "Customer",
        shiny::tabPanel(
          icon = fontawesome::fa_i("fas fa-user-group"),
          title = "All Customer",
          value = "tab_all_customer_summary",

          mod_customer_all_ui(id = "all_customer_summary")
        ),
        shiny::tabPanel(
          icon = fontawesome::fa_i("fas fa-user"),
          title = "Single Customer",
          value = "tab_individual_customer_summary",

          mod_customer_single_ui(id = "ind_customer_summary")
        )
      ),

      shiny::navbarMenu(
        title = "Sales Channel",

        shiny::tabPanel(
          icon = fontawesome::fa_i("fas fa-phone"),
          title = "Sales Channel",
          value = "tab_sales_channel_summary",

          mod_sales_channel_ui(id = "sales_channel_summary")
        ),
        shiny::tabPanel(
          icon = fontawesome::fa_i("fas fa-id-card-clip"),
          title = "Sales Rep",
          value = "tab_sales_team_summary",

          mod_sales_rep_ui(id = "sales_team_summary")
        ),
      ),

      bslib::nav_spacer(),

      bslib::nav_item(
        shiny::tags$a(fontawesome::fa_i("fas fa-info"), "Website", # change to modal
                      href = "https://akins11.github.io/Portfolio/")
      ),


      theme = bslib::bs_theme(version = 5,
                              bootswatch = "materia",
                              primary = "#9A9A9A",
                              secondary = "#99D0D3",
                              success = "#7FD8BE"),
      # bg = "#878787",
      id = "sales_nav_container",
      position = "fixed-top",
      footer = shiny::tags$div(shiny::tags$br(), shiny::tags$br())
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ico = "dibcon"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "regionalSalesDashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
