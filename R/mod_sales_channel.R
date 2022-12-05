#' sales_channel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sales_channel_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      page_analysis_settings(
        dropdown_id = ns("sales_c_dropdown"),
        id_all = ns("sc_all"),
        id_19  = ns("sc_19"),
        id_20  = ns("sc_20"),
        id_21  = ns("sc_21"),

        max_width = 180,

        shiny::tags$br(),

        input_switch(id = ns("show_sales_channel_input"))
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h528",

          echarts4r::echarts4rOutput(outputId = ns("sc_transaction_count")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),

      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h528",

          shinyjs::hidden(
            shinyWidgets::pickerInput(inputId = ns("sc_sp_agg_function"),
                                      label = "Aggregate By",
                                      choices = aggregate_functions,
                                      selected = "sum",
                                      width = "250px")
          ),

          echarts4r::echarts4rOutput(outputId = ns("sc_sales_profit")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h440",

          shiny::uiOutput(outputId = ns("sc_product_tran_title")),

          reactable::reactableOutput(outputId = ns("sc_product_tran")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),

      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h440",

          shiny::uiOutput(outputId = ns("sc_customer_tran_title")),

          reactable::reactableOutput(outputId = ns("sc_customer_tran")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    )
  )
}








#' sales_channel Server Functions
#'
#' @noRd
mod_sales_channel_server <- function(id, s_data, parent_session) {

  stopifnot(shiny::is.reactive(s_data))

  moduleServer(
    id,

    function(input, output, session) {

    ns <- session$ns

    # Analysis period -------------------------------------------------------|
    shiny::observe({
      if (isTRUE(input$sc_19) || isTRUE(input$sc_20) ||
          isTRUE(input$sc_21)) {
        a_value <- FALSE

      } else {
        a_value <- TRUE
      }

      shinyWidgets::updatePrettySwitch(session = session,
                                       inputId = "sc_all",
                                       value   = a_value)
    })
    shiny::observe({
      if (isTRUE(input$sc_all)) {
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "sc_19",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "sc_20",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "sc_21",
                                         value   = FALSE)
      }
    })


    # Filter period ---------------------------------------------------------|
    sf_data <- shiny::reactive({
      filter_period(df = s_data(),
                    y_all = input$sc_all,
                    y_19  = input$sc_19,
                    y_20  = input$sc_20,
                    y_21  = input$sc_21)
    }) |>
      shiny::debounce(800)


    # Show & hide input -----------------------------------------------------|
    shiny::observe({
      if (input$show_sales_channel_input) {
        show_input(id = "sc_sp_agg_function")
      } else {
        hide_input(id = "sc_sp_agg_function")
      }
    }) |>
      shiny::bindEvent(input$show_sales_channel_input)


    # Transactions ----------------------------------------------------------|
    output$sc_transaction_count <- echarts4r::renderEcharts4r({
      shiny::req(sf_data())

      plt_sales_channel_count(df = sf_data())
    })


    # Transaction with customers --------------------------------------------|
    output$sc_customer_tran_title <- shiny::renderUI({
      t_t <- stringr::str_glue("Transcations Involving Sales Channel & <span style='color:{pill_buttons_color_5}'>Customers</span>")

      shiny::tagList(table_title(title = t_t, use_html = TRUE, t_size = 24))
    })

    output$sc_customer_tran <- reactable::renderReactable({
      shiny::req(sf_data())

      ftbl_sales_channel_var_count(df = sf_data(),
                                   variable = "customer_names")
    })


    # Transaction with product ----------------------------------------------|
    output$sc_product_tran_title <- shiny::renderUI({
      t_t <- stringr::str_glue("Transcations Involving Sales Channel & <span style='color:{pill_buttons_color_5}'>Products</span>")

      shiny::tagList(table_title(title = t_t, use_html = TRUE, t_size = 24))
    })

    output$sc_product_tran <- reactable::renderReactable({
      shiny::req(sf_data())

      ftbl_sales_channel_var_count(df = sf_data(),
                                   variable = "product_name")
    })


    # Sales & Profit summary ------------------------------------------------|
    output$sc_sales_profit <- echarts4r::renderEcharts4r({
      shiny::req(sf_data(), input$sc_sp_agg_function)

      plt_sc_sales_profit(df = sf_data(), s_fun = input$sc_sp_agg_function)
    })
    }
  )
}
