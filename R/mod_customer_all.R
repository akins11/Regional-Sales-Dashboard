#' customer_all UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_customer_all_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      page_analysis_settings(
        dropdown_id = ns("all_cus_dropdown"),
        id_all = ns("all_cus_all"),
        id_19 = ns("all_cus_19"),
        id_20 = ns("all_cus_20"),
        id_21 = ns("all_cus_21"),

        max_width = 180,

        shiny::tags$br(),

        input_switch(id = ns("show_all_customer_inputs"))
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h580",

          shiny::uiOutput(outputId = ns("purchase_frequency_title")),

          reactable::reactableOutput(outputId = ns("purchase_frequency")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h580",

          shinyjs::hidden(
            shinyWidgets::pickerInput(inputId = ns("cus_scp_agg_fun"),
                                      label = "Aggregate By",
                                      choices = aggregate_functions,
                                      selected = "sum",
                                      width = "300px")
          ),

          reactable::reactableOutput(outputId = ns("cus_sales_profit_summary")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    ),

    shinyjs::hidden(
      shiny::fluidRow(
        id = ns("all_cus_input1"),

        shiny::column(
          width = 5,

          shinyWidgets::pickerInput(inputId = ns("all_customer_location"),
                                    label = "Geo Area",
                                    choices = location_names,
                                    multiple = TRUE,
                                    selected = c("region", "state"),
                                    inline = TRUE),

          shinyWidgets::pickerInput(inputId = ns("customer_name"),
                                    label = "Customer",
                                    choices = NULL,
                                    inline = TRUE)
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shinyWidgets::panel(
          class = "panel-h540",

          shiny::uiOutput(outputId = ns("cus_loc_profit_tbl_title")),

          reactable::reactableOutput(outputId = ns("cus_loc_profit_tbl")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),

      shiny::column(
        width = 5,

        shinyWidgets::panel(
          class = "panel-h540",

          table_title(title = "Number of Days Since The Last Order", t_size = 30),

          reactable::reactableOutput(outputId = ns("n_days_last_purchase"), height = "300px") |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        shinyWidgets::panel(
          shinyjs::hidden(
            shiny::fluidRow(
              id = ns("all_cus_input2"),

              shiny::column(
                width = 2,

                shinyWidgets::prettySwitch(inputId = ns("plt_output"),
                                           label = "Cummulative",
                                           value = FALSE,
                                           status = "primary",
                                           slim = TRUE),
              ),
              shiny::column(
                width = 3,

                shinyWidgets::pickerInput(inputId = ns("cus_order_date_revenue"),
                                          label = "variable",
                                          choices = revenue_variables,
                                          selected = "profit"),
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              highcharter::highchartOutput(outputId = ns("cus_sales_order_date")) |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      )
    )
  )
}









#' customer_all Server Functions
#'
#' @noRd
mod_customer_all_server <- function(id, s_data, parent_session) {

  stopifnot(shiny::is.reactive(s_data))

  shiny::moduleServer(
    id,

    function(input, output, session) {

    ns <- session$ns

    # Analysis period -------------------------------------------------------|
    shiny::observe({
      if (isTRUE(input$all_cus_19) || isTRUE(input$all_cus_20) ||
          isTRUE(input$all_cus_21)) {
        a_value <- FALSE

      } else {
        a_value <- TRUE
      }

      shinyWidgets::updatePrettySwitch(session = session,
                                       inputId = "all_cus_all",
                                       value   = a_value)
    })
    shiny::observe({
      if (isTRUE(input$all_cus_all)) {
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "all_cus_19",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "all_cus_20",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "all_cus_21",
                                         value   = FALSE)
      }
    })


    # Filter period ---------------------------------------------------------|
    sf_data <- shiny::reactive({
      filter_period(df = s_data(),
                    y_all = input$all_cus_all,
                    y_19  = input$all_cus_19,
                    y_20  = input$all_cus_20,
                    y_21  = input$all_cus_21)
    }) |>
      shiny::debounce(1000)


    # Show & hide inputs ----------------------------------------------------|
    shiny::observe({
      if (input$show_all_customer_inputs) {
        show_input(id = "cus_scp_agg_fun")
        show_input(id = "all_cus_input1")
        show_input(id = "all_cus_input2")

      } else {
        hide_input(id = "cus_scp_agg_fun")
        hide_input(id = "all_cus_input1")
        hide_input(id = "all_cus_input2")
      }
    }) |>
      shiny::bindEvent(input$show_all_customer_inputs)


    # Purchase Frequency ----------------------------------------------------|
    output$purchase_frequency_title <- shiny::renderUI({
      t_t <- "Customer Purchase <span style='color:#5E5E5E;'>Frequency</span>"

      shiny::tagList(table_title(title = t_t, use_html = TRUE))
    })

    output$purchase_frequency <- reactable::renderReactable({
      shiny::req(sf_data())

      n_row <- ifelse(input$show_all_customer_inputs, 11, 9)

      customer_purchase_freq(df = sf_data(), page_size = n_row)
    })

    # Sales, cost & profit summary ------------------------------------------|
    output$cus_sales_profit_summary <- reactable::renderReactable({
      shiny::req(sf_data(), input$cus_scp_agg_fun)

      customer_sales_profit_summary(df = sf_data(),
                                    s_fun = input$cus_scp_agg_fun)
    })


    # Update picker input ---------------------------------------------------|
    shiny::observe({
      shiny::req(sf_data())

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "customer_name",
                                      choices = unique(sf_data()$customer_names))
    })


    # customer name ---------------------------------------------------------|
    c_data <- shiny::reactive({
      shiny::req(sf_data(), input$customer_name)

      filter_variable(df = sf_data(),
                      variable = "customer_names",
                      category = input$customer_name)
    })


    # Location and profit ---------------------------------------------------|
    output$cus_loc_profit_tbl_title <- shiny::renderUI({
      shiny::req(input$customer_name)

      t_t <- stringr::str_glue("<span style='color:#A6A6A6'>Customer ::</span> {input$customer_name}")

      shiny::tagList(table_title(title = t_t, use_html = TRUE))
    })

    output$cus_loc_profit_tbl <- reactable::renderReactable({
      shiny::req(c_data(), input$all_customer_location)

      rtbl_sub_cloc_drilldown(f_df = c_data(),
                              gp_vars = input$all_customer_location,
                              sumy_var = "profit",
                              customer_name = input$customer_name,
                              s_fun = "sum")
    })


    # Number of Days since last purchase ------------------------------------|
    output$n_days_last_purchase <- reactable::renderReactable({
      shiny::req(c_data(), input$customer_name)

      rtbl_customer_order_days(f_df = c_data(),
                               customer = input$customer_name)
    })


    # Order date ------------------------------------------------------------|
    output$cus_sales_order_date <- highcharter::renderHighchart({
      shiny::req(c_data(), input$cus_order_date_revenue, input$customer_name)

      plt_order_date(f_df = c_data(),
                     category = input$customer_name,
                     by = input$cus_order_date_revenue,
                     cum = input$plt_output)
    })
    }
  )
}

