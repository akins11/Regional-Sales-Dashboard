#' customer_single UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_customer_single_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        width = 1,

        page_analysis_settings(
          dropdown_id = ns("single_cus_dropdown"),
          id_all = ns("single_cus_all"),
          id_19  = ns("single_cus_19"),
          id_20  = ns("single_cus_20"),
          id_21  = ns("single_cus_21"),

          max_width = 220,

          shiny::tags$br(),

          input_switch(id = ns("show_ind_customer_inputs"), value = TRUE)
        )
      ),

      shiny::column(
        width = 1,

        shinyWidgets::dropMenu(
          shinyWidgets::actionBttn(inputId = ns("cus_dd_btn"),
                                   label = "",
                                   icon  = fontawesome::fa("fas fa-user",
                                                           fill  = "#858585",
                                                           height = "1.2em",
                                                           width = "1.1em",
                                                           title = "Customer"),
                                   style = "material-flat",
                                   color = "default",
                                   size  = "sm"),

          shinyWidgets::pickerInput(inputId = ns("select_customer"),
                                    label = "Cusotmer Name",
                                    choices = NULL),

          theme = "light",
          maxWidth = 235
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        shinyWidgets::panel(
          shiny::uiOutput(outputId = ns("description_cind_box"))
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shinyWidgets::panel(
          class = "panel-h533",

          input_sales_profit(
            id = ns("ind_cus_input1"),
            id1 = ns("sub_customer_sumy_by"),
            id2 = ns("sub_customer_agg_fun"),
            id3 = ns("sub_customer_loc_cat")
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              echarts4r::echarts4rOutput(outputId = ns("sc_sales_profit_plt")) |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      ),
      shiny::column(
        width = 5,

        shinyWidgets::panel(
          class = "panel-h533",

          echarts4r::echarts4rOutput(outputId = ns("sub_customer_sales_bk")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        shiny::fluidRow(
          id = ns("ind_cus_input2"),

          shiny::column(
            width = 3,

            shinyWidgets::pickerInput(inputId = ns("sub_customer_tq_agg_fun"),
                                      label = "Aggregate By",
                                      choices = aggregate_functions,
                                      selected = "sum")
          )
        ),

        shiny::fluidRow(
          shiny::column(
            width = 6,

            shinyWidgets::panel(
              class = "panel-h660",

              shiny::uiOutput(outputId = ns("sub_customer_trans_qty_tbl_title")),

              reactable::reactableOutput(outputId = ns("sub_customer_trans_qty_tbl"), height = "543px") |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          ),

          shiny::column(
            width = 6,

            shinyWidgets::panel(
              class = "panel-h660",

              reactable::reactableOutput(outputId = ns("sub_customer_sale_profit_tbl"), height = "597px") |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      )
    )
  )
}







#' customer_single Server Functions
#'
#' @noRd
mod_customer_single_server <- function(id, s_data, parent_session) {

  stopifnot(shiny::is.reactive(s_data))

  shiny::moduleServer(
    id,

    function(input, output, session) {

    ns <- session$ns

    # Analysis period -------------------------------------------------------|
    shiny::observe({
      if (isTRUE(input$single_cus_19) || isTRUE(input$single_cus_20) ||
          isTRUE(input$single_cus_21)) {
        a_value <- FALSE

      } else {
        a_value <- TRUE
      }

      shinyWidgets::updatePrettySwitch(session = session,
                                       inputId = "single_cus_all",
                                       value   = a_value)
    })
    shiny::observe({
      if (isTRUE(input$single_cus_all)) {
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "single_cus_19",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "single_cus_20",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "single_cus_21",
                                         value   = FALSE)
      }
    })


    # Filter period ---------------------------------------------------------|
    sf_data <- shiny::reactive({
      filter_period(df = s_data(),
                    y_all = input$single_cus_all,
                    y_19  = input$single_cus_19,
                    y_20  = input$single_cus_20,
                    y_21  = input$single_cus_21)
    }) |>
      shiny::debounce(1000)


    # hide & show inputs ----------------------------------------------------|
    shiny::observe({
      if (input$show_ind_customer_inputs) {
        show_input("ind_cus_input1")
        show_input("ind_cus_input2")

      } else {
        hide_input(id = "ind_cus_input1")
        hide_input(id = "ind_cus_input2")
      }
    }) |>
      shiny::bindEvent(input$show_ind_customer_inputs)

    # set up data -----------------------------------------------------------|
    shiny::observe({
      shiny::req(sf_data())

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "select_customer",
                                      choices = unique(sf_data()$customer_names),
                                      selected = NULL)
    })

    c_data <- shiny::reactive({
      shiny::req(sf_data(), input$select_customer)

      filter_variable(df = sf_data(),
                      variable = "customer_names",
                      category = input$select_customer)
    })

    # Description -----------------------------------------------------------|
    prev_list <- shiny::reactive({
      shiny::req(sf_data(), c_data())

      list(
        transaction = sub_category_summary(c_data(), sf_data(), "transaction"),
        order_quantity = sub_category_summary(c_data(), sf_data(), "order_quantity"),
        sales = sub_category_summary(c_data(), sf_data(), "sales"),
        discount = sub_category_summary(c_data(), sf_data(), "discount"),
        cost = sub_category_summary(c_data(), variable = "cost"),
        profit = sub_category_summary(c_data(), variable = "profit")
      )
    })

    output$description_cind_box <- shiny::renderUI({
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
              textThree = "% of sales from customer",
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
              textThree = "% of sales from customer",
              valueThree = prev_list()$profit$percentage_own_sales,
              colorThree = "blue",
              rightBorder = FALSE
            )
          )
        )
      )
    })


    # Sales Cost & Profit ---------------------------------------------------|
    shiny::observe({
      shiny::req(sf_data(), c_data(), input$sub_customer_sumy_by)

      selected_locs <- get_filter_unique_cat(f_df = c_data(),
                                             cat_var = input$sub_customer_sumy_by,
                                             sample = TRUE)
      choice_loc <- get_filter_unique_cat(f_df = c_data(),
                                          cat_var = input$sub_customer_sumy_by,
                                          sample = FALSE)

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "sub_customer_loc_cat",
                                      choices = choice_loc,
                                      selected = selected_locs)
    })

    output$sc_sales_profit_plt <- echarts4r::renderEcharts4r({
      shiny::req(c_data(), input$select_customer, input$sub_customer_agg_fun,
                 input$sub_customer_sumy_by, input$sub_customer_loc_cat)

      plt_ser_summary(f_df = c_data(),
                      fl_var = "customer_names",
                      category = input$select_customer,
                      gp_var = input$sub_customer_sumy_by,
                      s_fun = input$sub_customer_agg_fun,
                      gp_sample = input$sub_customer_loc_cat)
    })

    # Sales Breakdown -------------------------------------------------------|
    output$sub_customer_sales_bk <- echarts4r::renderEcharts4r({
      shiny::req(c_data())

      plt_percent_profit_cost(df = c_data())
    })

    # Product transaction Quantity summary ---------------------------------|
    output$sub_customer_trans_qty_tbl_title <- shiny::renderUI({
      shiny::req(c_data(), input$select_customer)

      n_count_var <- length(unique(c_data()$product_name))

      t_t <- glue::glue("{var_names$customer_names} :: {clean_text(input$select_customer)}")

      t_s <- glue::glue("Purchased {n_count_var} Unique Products")

      shiny::tagList(
        table_title(title = t_t),
        table_title(title = t_s, t_color = "#E3E3E3", t_size = 15)
      )
    })

    output$sub_customer_trans_qty_tbl <- reactable::renderReactable({
      shiny::req(c_data(), input$sub_customer_tq_agg_fun)

      rtbl_sub_pc_summary(f_df = c_data(),
                          count_var = "product_name",
                          s_fun = input$sub_customer_tq_agg_fun)
    })


    # customer Sales, cost & profit -----------------------------------------|
    output$sub_customer_sale_profit_tbl <- reactable::renderReactable({
      shiny::req(c_data(), input$sub_customer_tq_agg_fun)

      rtbl_sub_sale_profit(f_df = c_data(),
                           count_var = "product_name",
                           s_fun = input$sub_customer_tq_agg_fun)
    })
    }
  )
}

