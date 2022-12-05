#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    waiter::waiterPreloader(html = waiter::spin_ellipsis(),
                            color = pill_buttons_color_3,
                            fadeout = TRUE),

    shiny::fluidRow(
      page_analysis_settings(dropdown_id = ns("overview_s_dropdown"),
                             id_all = ns("overview_all"),
                             id_19  = ns("overview_19"),
                             id_20  = ns("overview_20"),
                             id_21  = ns("overview_21"))
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        bs_card(
          class_cb = "p-0 m-0",
          style = "background-color: #FCFCFC;",

          shiny::uiOutput(outputId = ns("description_box"))
        )
      ),

      shiny::column(
        width = 6,

        shinyWidgets::panel(
          table_title(
            title = stringr::str_glue("Best Measure By <span style='color:{pill_buttons_color_5}'>Profit</span> Generated"),
            use_html = TRUE,
            t_color = title_color,
            t_size = 20
          ),
          reactable::reactableOutput(outputId = ns("top_measures"), height = "241px") |>
            output_spinner()
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "diff-bg-color",

          shiny::fluidRow(toggle_btn(inputId = ns("yoy_toggle"))),

          shiny::fluidRow(
            id = ns("yoy_control_row"),

            shiny::column(
              width = 4,

              shinyWidgets::prettyRadioButtons(inputId = ns("month_quarter"),
                                               label = "YoY",
                                               choiceNames  = c("Month", "Quarter"),
                                               choiceValues = c("month", "quarter"),
                                               selected = "month",
                                               shape = "curve",
                                               animation = "pulse")
            ),
            shiny::column(
              width = 4,

              shinyjs::hidden(
                shinyWidgets::pickerInput(inputId = ns("q_date"),
                                          label   = "Period",
                                          choices = q_values)
              ),

              shinyWidgets::airMonthpickerInput(inputId = ns("m_date"),
                                                label = "Period",
                                                value = "2021-01-01",
                                                minDate = "2021-01-01",
                                                maxDate = "2021-12-01",
                                                update_on = "close")
            ),

            shiny::column(
              width = 4,

              aggregate_picker_input(id = ns("mq_agg_fun"))
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 8,
              offset = 2,

              shiny::uiOutput(outputId = ns("yoy_card_ui"))
            )
          ),

          shiny::tags$br(), shiny::tags$br(),

          shiny::fluidRow(toggle_btn(inputId = ns("mtd_toggle"))),

          shiny::fluidRow(
            id = ns("mtd_control_row"),

            shiny::column(
              width = 4,

              shinyWidgets::airDatepickerInput(inputId = ns("mtd_start_date"),
                                               label = "Start date",
                                               value = "2021-11-30",
                                               placeholder = "start date",
                                               minDate = "2019-06-01",
                                               maxDate = "2021-11-30")

              # uiOutput(outputId = ns("mtd_start_date_ui"))
            ),
            shiny::column(
              width = 4,

              shinyWidgets::airDatepickerInput(inputId = ns("mtd_end_date"),
                                               label = "End date",
                                               value = "2021-12-31",
                                               placeholder = "end date",
                                               minDate = "2019-06-30",
                                               maxDate = "2021-12-30")

              # uiOutput(outputId = ns("mtd_end_date_ui"))
            ),
            shiny::column(
              width = 4,

              aggregate_picker_input(id = ns("mtd_agg_fun"))
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 8,
              offset = 2,

              shiny::uiOutput(outputId = ns("MTD_card_ui"))
            )
          ),

          shiny::tags$br(), shiny::tags$br(),

          shiny::fluidRow(toggle_btn(inputId = ns("mom_toggle"))),

          shiny::fluidRow(
            id = ns("mom_control_row"),

            shiny::column(
              width = 4,

              shinyWidgets::airMonthpickerInput(inputId = ns("mom_start_date"),
                                                label = "Current month",
                                                value = "2021-12-01",
                                                placeholder = "current month",
                                                minDate = "2019-06-01",
                                                maxDate = "2021-12-01")
            ),
            shiny::column(
              width = 4,

              aggregate_picker_input(id = ns("mom_agg_fun"))
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 8,
              offset = 2,

              shiny::uiOutput(outputId = ns("mom_card_ui"))
            )
          )
        )
      ),

      shiny::column(
        width = 6,

        # uiOutput(outputId = ns("cb_pbr_output"))
        shinyWidgets::panel(
          class = "pb-1",

          echarts4r::echarts4rOutput(outputId = ns("cost_breakdown_plt"),
                                     height = "288px") |>
            output_spinner(),

          echarts4r::echarts4rOutput(outputId = ns("profit_by_region_plt"),
                                     height = "305px") |>
            output_spinner()
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        shinyWidgets::panel(
          toggle_btn(inputId = ns("rev_sub_input_toggle")),

          shiny::fluidRow(
            id = ns("rev_sub_input_row"),

            shiny::column(
              width = 3,

              shinyWidgets::pickerInput(inputId = ns("rev_sub_var"),
                                        label = "Variable choice",
                                        choices = revenue_variables,
                                        selected = "profit",
                                        inline = TRUE)
            ),
            shiny::column(
              width = 3,

              shiny::uiOutput(outputId = ns("analysis_yr_ui"))
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              highcharter::highchartOutput(outputId = ns("year_revenue_plt")) |>
                output_spinner()
            )
          )
        )
      )
    )
  )
}




#' overview Server Functions
#'
#' @param id module server id
#'
#' @noRd
mod_overview_server <- function(id) {

  shiny::moduleServer(
    id,

    function(input, output, session) {

    ns <- session$ns

    s_data <- shiny::reactive({
      vroom::vroom("data/sales.csv", delim = ",")
    })

    # Analysis period -------------------------------------------------------|
    shiny::observe({
      if (isTRUE(input$overview_19) || isTRUE(input$overview_20) ||
          isTRUE(input$overview_21)) {
        a_value <- FALSE

      } else {
        a_value <- TRUE
      }

      shinyWidgets::updatePrettySwitch(session = session,
                                       inputId = "overview_all",
                                       value   = a_value)
    })

    shiny::observe({
      if (isTRUE(input$overview_all)) {
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "overview_19",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "overview_20",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "overview_21",
                                         value   = FALSE)
      }
    })


    # Filter period ---------------------------------------------------------|
    sf_data <- shiny::reactive({
      filter_period(df = s_data(),
                    y_all = input$overview_all,
                    y_19  = input$overview_19,
                    y_20  = input$overview_20,
                    y_21  = input$overview_21)
    }) |>
      shiny::debounce(800)


    # Descriptions ----------------------------------------------------------|
    rev_vars <- shiny::reactive({
      list(
        sales = revenue_summary_list(df = sf_data(), "sales"),
        revenue = revenue_summary_list(df = sf_data(), "profit"),
        discount = revenue_summary_list(df = sf_data(), "discount"),
        cost = revenue_summary_list(df = sf_data(), "cost")
      )
    })

    output$description_box <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 6,
            description_card("Total Sales", rev_vars()$discount$total,
                             "Average Sales", rev_vars()$sales$average)
          ),
          shiny::column(
            width = 6,
            description_card("Total Discount", rev_vars()$discount$total,
                             "Average Discount", rev_vars()$discount$average)
          )
        ),

        shiny::fluidRow(
          shiny::column(
            width = 6,
            description_card("Total Cost", rev_vars()$revenue$total,
                             "Average Cost", rev_vars()$revenue$average,
                             "Percentage of Sales", rev_vars()$revenue$percentage_sales,
                             "blue")
          ),
          shiny::column(
            width = 6,
            description_card("Total Profit", rev_vars()$cost$total,
                             "Average Profit", rev_vars()$cost$average,
                             "Percentage of Sales", rev_vars()$cost$percentage_sales,
                             "red")
          )
        )

      )
    })

    # Best Categories -------------------------------------------------------|

    output$top_measures <- reactable::renderReactable({
      shiny::req(sf_data())

      best_category(df = sf_data())
    })


    # Profit & dates --------------------------------------------------------|

    #
    # update start & end dates --------------------------------------|
    # output$mtd_start_date_ui <- renderUI({
    #   valid_prd <- add_sub_month(isolate(input$mtd_end_date), FALSE)
    #
    #   tagList(
    #     airDatepickerInput(inputId = session$ns("mtd_start_date"),
    #                        label = "Start date",
    #                        value = "2021-11-01",
    #                        placeholder = "start date",
    #                        minDate = "2019-06-01",
    #                        maxDate = valid_prd # "2021-11-30"
    #                        )
    #   )
    # })
    # output$mtd_end_date_ui <- renderUI({
    #   valid_prd <- add_sub_month(isolate(input$mtd_start_date), TRUE)
    #
    #   airDatepickerInput(inputId = session$ns("mtd_end_date"),
    #                      label = "End date",
    #                      value = "2021-12-30",
    #                      placeholder = "end date",
    #                      minDate = valid_prd, # "2019-06-30
    #                      maxDate = "2021-12-30")
    # })

    # Update stati cards ---------------------------------------------|
    shiny::observe({
      if (input$yoy_toggle) {
        show_input(id = "yoy_control_row")

      } else {
        hide_input(id = "yoy_control_row")
      }
    })

    shiny::observe({
      shiny::req(input$month_quarter)

      if (input$month_quarter == "quarter") {
        show_input(id = "q_date")
        hide_input(id = "m_date")

      } else if (input$month_quarter == "month") {
        show_input(id = "m_date")
        hide_input(id = "q_date")
      }
    })

    output$yoy_card_ui <- shiny::renderUI({
      shiny::req(s_data(), input$month_quarter, input$mq_agg_fun)

      if (input$month_quarter == "quarter") {
        period_value <- input$q_date
      } else if (input$month_quarter == "month") {
        period_value <- input$m_date
      }
      yoy_value <- get_yoy(df = s_data(),
                           period = input$month_quarter,
                           period_value = period_value,
                           a_fun = input$mq_agg_fun)

      shiny::tagList(
        bs_stati_card(value = yoy_value,
                      value_lab = numeric_lab(yoy_value, "percent"),
                      subtitle = "YoY 2020 - 2021")
      )
    })


    # Month to Date --------------------------------------------------|
    shiny::observe({
      if (input$mtd_toggle) {
        show_input(id = "mtd_control_row")

      } else {
        hide_input(id = "mtd_control_row")
      }
    })

    output$MTD_card_ui <- shiny::renderUI({
      shiny::req(s_data(), input$mtd_start_date, input$mtd_end_date, input$mtd_agg_fun)

      mtd_value <- get_mtd(df = s_data(),
                           start_date = input$mtd_start_date,
                           end_date = input$mtd_end_date,
                           a_fun = input$mtd_agg_fun)

      shiny::tagList(
        bs_stati_card(value = mtd_value,
                      value_lab = numeric_lab(mtd_value, "dollar"),
                      subtitle = "Month to Date")
      )
    })

    # Month on month ------------------------------------------------|
    shiny::observe({
      if (input$mom_toggle) {
        show_input(id = "mom_control_row")

      } else {
        hide_input(id = "mom_control_row")
      }
    })

    output$mom_card_ui <- shiny::renderUI({
      shiny::req(s_data(), input$mom_start_date, input$mom_agg_fun)

      mom_value <- get_mom(df = s_data(),
                           current_date = input$mom_start_date,
                           a_fun = input$mom_agg_fun)

      shiny::tagList(
        bs_stati_card(value = mom_value,
                      value_lab = numeric_lab(mom_value, "percent"),
                      subtitle = "Month on Month")
      )
    })

    # revenue summary by year -----------------------------------------------|
    shiny::observe({
      if (input$rev_sub_input_toggle) {
        show_input(id = "rev_sub_input_row")

      } else {
        hide_input(id = "rev_sub_input_row")
      }
    })

    shiny::observe({
      sh <- show_hide(y_all = input$overview_all,
                      y_19  = input$overview_19,
                      y_20  = input$overview_20,
                      y_21  = input$overview_21)
      if (sh) {
        show_input(id = "analysis_yr_ui")

      } else {
        hide_input(id = "analysis_yr_ui")
      }
    })

    output$analysis_yr_ui <- shiny::renderUI({
      shiny::req(sf_data())

      yr_choice <- get_year_choice(df = sf_data())

      shiny::tagList(
        shinyWidgets::prettyRadioButtons(inputId = session$ns("summary_year"),
                                         label = "Analysis Year",
                                         choices = yr_choice,
                                         status = "primary",
                                         shape = "curve",
                                         inline = TRUE,
                                         animation = "pulse")
      )
    })

    output$year_revenue_plt <- highcharter::renderHighchart({
      shiny::req(sf_data(), input$rev_sub_var) #input$summary_year,

      plt_revenue_year(df = sf_data(),
                       f_year = input$summary_year,
                       sumy_var = input$rev_sub_var)
    })


    # cost breakdown --------------------------------------------------------|
    output$cost_breakdown_plt <- echarts4r::renderEcharts4r({
      shiny::req(sf_data())

      plt_cost_breakdown(df = sf_data())
    })


    # Profit & Region -------------------------------------------------------|
    output$profit_by_region_plt <- echarts4r::renderEcharts4r({
      shiny::req(sf_data())

      plt_profit_by_region(df = sf_data())
    })


    # Output ----------------------------------------------------------------|
    return(s_data)
   }
  )
}

