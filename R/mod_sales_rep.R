#' sales_rep UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sales_rep_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      page_analysis_settings(
        dropdown_id = ns("sales_t_dropdown"),
        id_all = ns("st_all"),
        id_19  = ns("st_19"),
        id_20  = ns("st_20"),
        id_21  = ns("st_21"),

        max_width = 200,
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        shinyWidgets::panel(
          shiny::fluidRow(
            shiny::column(
              width = 3,

              shinyWidgets::prettySwitch(inputId = ns("use_rev"),
                                         label = "Cost & profit",
                                         value = FALSE,
                                         status = "primary",
                                         slim = TRUE),
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              echarts4r::echarts4rOutput(outputId = ns("all_sp_rev_summary")) |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      shinyWidgets::dropMenu(
        shinyWidgets::actionBttn(inputId = ns("sales_rep_dd"),
                                 label = "",
                                 icon  = fontawesome::fa("fas fa-id-card-clip",
                                                         fill  = "#858585",
                                                         height = "1.2em",
                                                         width = "1.1em",
                                                         title = "Rep"),
                                 style = "material-flat",
                                 color = "default",
                                 size  = "sm"),

        shinyWidgets::pickerInput(inputId = ns("select_sales_person"),
                                  label = "Sales Person",
                                  choices = NULL),

        shiny::tags$br(),

        aggregate_picker_input(id = ns("sp_aggregate_fun")),

        theme = "light",
        maxWidth = 235
      )
    ),

    shiny::tags$br(),


    shiny::fluidRow(
      shiny::column(
        width = 12,

        shinyWidgets::panel(
          uiOutput(outputId = ns("description_sp_box"))
        )
      )
    ),


    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          echarts4r::echarts4rOutput(outputId = ns("sp_trans_region"), height = "485px") |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),

      shiny::column(
        width = 6,

        shinyWidgets::panel(
          shiny::fluidRow(
            shiny::column(
              width = 4,

              shinyWidgets::pickerInput(inputId = ns("sp_ind_rev_variable"),
                                        label = "variable",
                                        choices = revenue_variables,
                                        selected = "profit"),
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              echarts4r::echarts4rOutput(outputId = ns("sp_ind_rev_summary")) |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          reactable::reactableOutput(outputId = ns("sp_customer_rev_summary")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),

      shiny::column(
        width = 6,

        shinyWidgets::panel(
          reactable::reactableOutput(outputId = ns("sp_product_rev_summary")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    )
  )
}









#' sales_rep Server Functions
#'
#' @param id module server id
#' @param s_data sales data.
#' @param parent_session
#'
#' @noRd
mod_sales_rep_server <- function(id, s_data, parent_session) {

  stopifnot(shiny::is.reactive(s_data))

  shiny::moduleServer(
    id,

    function(input, output, session) {

    ns <- session$ns

    # Analysis period -------------------------------------------------------|
    shiny::observe({
      if (isTRUE(input$st_19) || isTRUE(input$st_20) ||
          isTRUE(input$st_21)) {
        a_value <- FALSE

      } else {
        a_value <- TRUE
      }

      shinyWidgets::updatePrettySwitch(session = session,
                                       inputId = "st_all",
                                       value   = a_value)
    })
    shiny::observe({
      if (isTRUE(input$st_all)) {
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "st_19",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "st_20",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "st_21",
                                         value   = FALSE)
      }
    })


    # Filter period ---------------------------------------------------------|
    sf_data <-  shiny::reactive({
      filter_period(df = s_data(),
                    y_all = input$st_all,
                    y_19  = input$st_19,
                    y_20  = input$st_20,
                    y_21  = input$st_21)
    }) |>
      shiny::debounce(800)


    # Update sales person ---------------------------------------------------|
    shiny::observe({
      shiny::req(sf_data())
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "select_sales_person",
                                      choices = unique(s_data()$sales_team))
    })

    # filtered data ---------------------------------------------------------|
    sp_data <-  shiny::reactive({
      shiny::req(sf_data(), input$select_sales_person)

      filter_variable(df = sf_data(),
                      variable = "sales_team",
                      category = input$select_sales_person)
    })

    # Description -----------------------------------------------------------|
    prev_list <-  shiny::reactive({
      shiny::req(sf_data(), sp_data())

      list(
        transaction = sub_category_summary(sp_data(), sf_data(), "transaction"),
        order_quantity = sub_category_summary(sp_data(), sf_data(), "order_quantity"),
        sales = sub_category_summary(sp_data(), sf_data(), "sales"),
        discount = sub_category_summary(sp_data(), sf_data(), "discount"),
        cost = sub_category_summary(sp_data(), variable = "cost"),
        profit = sub_category_summary(sp_data(), variable = "profit")
      )
    })

    output$description_sp_box <-  shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 2,
            description_block(
              textOne = "Total Transcation",
              valueOne = prev_list()$transaction$n_value,
              textThree = "% of overall transactions",
              valueThree = prev_list()$transaction$percentage_total,
              colorThree = "blue"
            )
          ),
          shiny::column(
            width = 2,
            description_block(
              textOne = "Total Order Qty",
              valueOne = prev_list()$order_quantity$sum_value,
              textTwo = "Average Order Qty",
              valueTwo = prev_list()$order_quantity$average_value,
              textThree = "% of overall order qty",
              valueThree = prev_list()$order_quantity$percentage_total,
              colorThree = "blue"
            )
          ),
          shiny::column(
            width = 2,
            description_block(
              textOne = "Total Sales",
              valueOne = prev_list()$sales$sum_value,
              textTwo = "Average Sales",
              valueTwo = prev_list()$sales$average_value,
              textThree = "% of overall sales",
              valueThree = prev_list()$sales$percentage_sales,
              colorThree = "blue"
            )
          ),
          shiny::column(
            width = 2,
            description_block(
              textOne = "Total Discount",
              valueOne = prev_list()$discount$sum_value,
              textTwo = "Average Discount",
              valueTwo = prev_list()$discount$average_value,
              textThree = "% of cost",
              valueThree = prev_list()$discount$percentage_cost,
              colorThree = "red"
            )
          ),
          shiny::column(
            width = 2,
            description_block(
              textOne = "Total Cost",
              valueOne = prev_list()$cost$sum_value,
              textTwo = "Average Cost",
              valueTwo = prev_list()$cost$average_value,
              textThree = "% of product sales",
              valueThree = prev_list()$cost$percentage_own_sales,
              colorThree = "red"
            )
          ),
          shiny::column(
            width = 2,
            description_block(
              textOne = "Total Profit",
              valueOne = prev_list()$profit$sum_value,
              textTwo = "Average Profit",
              valueTwo = prev_list()$profit$average_value,
              textThree = "% of product sales",
              valueThree = prev_list()$profit$percentage_own_sales,
              colorThree = "blue",
              rightBorder = FALSE
            )
          )
        )
      )
    })

    # all sales person summary ----------------------------------------------|
    output$all_sp_rev_summary <- echarts4r::renderEcharts4r({
      shiny::req(sf_data(), input$sp_aggregate_fun)

      use <- ifelse(input$use_rev, "cost_profit", "sales")

      plt_sp_sales_profit_summary(df = sf_data(),
                                  use = use,
                                  s_fun = input$sp_aggregate_fun)
    })

    # Transaction by Region -------------------------------------------------|
    output$sp_trans_region <- echarts4r::renderEcharts4r({
      shiny::req(sp_data())

      plt_sp_trans_region(f_df = sp_data())
    })

    # Revenue summary & region ----------------------------------------------|
    output$sp_ind_rev_summary <- echarts4r::renderEcharts4r({
      shiny::req(sp_data(), input$sp_ind_rev_variable, input$sp_aggregate_fun)

      plt_sp_region_revenue_summary(f_df = sp_data(),
                                    variable = input$sp_ind_rev_variable,
                                    s_fun = input$sp_aggregate_fun)
    })

    # Customer & revenue ----------------------------------------------------|
    output$sp_customer_rev_summary <-  reactable::renderReactable({
      shiny::req(sp_data(), input$sp_aggregate_fun)

      rtbl_sub_sale_profit(f_df = sp_data(),
                           count_var = "customer_names",
                           s_fun = input$sp_aggregate_fun,
                           bold_name = TRUE)
    })

    # Product & revenue ----------------------------------------------------|
    output$sp_product_rev_summary <-  reactable::renderReactable({
      shiny::req(sp_data(), input$sp_aggregate_fun)

      rtbl_sub_sale_profit(f_df = sp_data(),
                           count_var = "product_name",
                           s_fun = input$sp_aggregate_fun,
                           bold_name = TRUE)
    })
    }
  )
}


