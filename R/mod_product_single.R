#' product_single UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_product_single_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        width = 1,

        page_analysis_settings(
          dropdown_id = ns("single_prod_dropdown"),
          id_all = ns("single_prod_all"),
          id_19  = ns("single_prod_19"),
          id_20  = ns("single_prod_20"),
          id_21  = ns("single_prod_21"),

          max_width = 200,

          shiny::tags$br(),

          input_switch(id = ns("ind_show_product_inputs"), value = TRUE)
        )
      ),

      shiny::column(
        width = 1,

        shinyWidgets::dropMenu(
          shinyWidgets::actionBttn(inputId = ns("prod_dd_btn"),
                                   label = "",
                                   icon  = fontawesome::fa("fas fa-cart-shopping",
                                                           fill  = "#858585",
                                                           height = "1.2em",
                                                           width = "1.1em",
                                                           title = "Product"),
                                   style = "material-flat",
                                   color = "default",
                                   size  = "sm"),

          shinyWidgets::pickerInput(inputId = ns("select_product"),
                                    label = "Select A Product",
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
          shiny::uiOutput(outputId = ns("description_pind_box"))
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shinyWidgets::panel(
          class = "panel-h533",

          input_sales_profit(
            id = ns("ind_prod_input1"),
            id1 = ns("sub_product_sumy_by"),
            id2 = ns("sub_product_agg_fun"),
            id3 = ns("sub_product_loc_cat")
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              echarts4r::echarts4rOutput(outputId = ns("sp_sale_profit_plt")) |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      ),
      shiny::column(
        width = 5,

        shinyWidgets::panel(
          class = "panel-h533",

          echarts4r::echarts4rOutput(outputId = ns("sub_product_sales_bk"), height = "488px") |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        shiny::fluidRow(
          id = ns("ind_prod_input2"),

          shiny::column(
            width = 3,

            shinyWidgets::pickerInput(inputId = ns("sub_product_tq_agg_fun"),
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

              shiny::uiOutput(outputId = ns("sub_product_trans_qty_tbl_title")),

              reactable::reactableOutput(outputId = ns("sub_product_trans_qty_tbl")) |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          ),

          shiny::column(
            width = 6,

            shinyWidgets::panel(
              class = "panel-h660",

              shiny::uiOutput(outputId = ns("sub_product_sale_profit_tbl_title")),

              reactable::reactableOutput(outputId = ns("sub_product_sale_profit_tbl")) |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      )
    )
  )
}







#' product_single Server Functions
#'
#' @param id module server id
#' @param s_data sales data.
#' @param parent_session
#'
#' @noRd
mod_product_single_server <- function(id, s_data, parent_session) {

  stopifnot(shiny::is.reactive(s_data))

  shiny::moduleServer(
    id,

    function(input, output, session) {

    ns <- session$ns

    # Analysis period -------------------------------------------------------|
    shiny::observe({
      if (isTRUE(input$single_prod_19) || isTRUE(input$single_prod_20) ||
          isTRUE(input$single_prod_21)) {
        a_value <- FALSE

      } else {
        a_value <- TRUE
      }

      shinyWidgets::updatePrettySwitch(session = session,
                                       inputId = "single_prod_all",
                                       value   = a_value)
    })
    shiny::observe({
      if (isTRUE(input$single_prod_all)) {
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "single_prod_19",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "single_prod_20",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "single_prod_21",
                                         value   = FALSE)
      }
    })


    # Filter period ---------------------------------------------------------|
    sf_data <- shiny::reactive({
      filter_period(df = s_data(),
                    y_all = input$single_prod_all,
                    y_19  = input$single_prod_19,
                    y_20  = input$single_prod_20,
                    y_21  = input$single_prod_21)
    }) |>
      shiny::debounce(1000)


    # hide & show inputs ----------------------------------------------------|
    shiny::observe({
      if (input$ind_show_product_inputs) {
        show_input("ind_prod_input1")
        show_input("ind_prod_input2")

      } else {
        hide_input(id = "ind_prod_input1")
        hide_input(id = "ind_prod_input2")
      }
    }) |>
      shiny::bindEvent(input$ind_show_product_inputs)

    # set up data -----------------------------------------------------------|
    shiny::observe({
      shiny::req(sf_data())

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "select_product",
                                      choices = unique(sf_data()[["product_name"]]),
                                      selected = NULL)
    })

    f_data <- shiny::reactive({
      shiny::req(sf_data(), input$select_product)

      filter_variable(df = sf_data(),
                      variable = "product_name",
                      category = input$select_product)
    })


    # Description -----------------------------------------------------------|
    prev_list <- shiny::reactive({
      shiny::req(sf_data(), f_data())

      list(
        transaction = sub_category_summary(f_data(), sf_data(), "transaction"),
        order_quantity = sub_category_summary(f_data(), sf_data(), "order_quantity"),
        sales = sub_category_summary(f_data(), sf_data(), "sales"),
        discount = sub_category_summary(f_data(), sf_data(), "discount"),
        cost = sub_category_summary(f_data(), variable = "cost"),
        profit = sub_category_summary(f_data(), variable = "profit")
      )
    })

    output$description_pind_box <- shiny::renderUI({
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


    # Sales Cost & Profit ---------------------------------------------------|
    shiny::observe({
      shiny::req(sf_data(), f_data(), input$sub_product_sumy_by)

      selected_locs <- get_filter_unique_cat(f_df = f_data(),
                                             cat_var = input$sub_product_sumy_by,
                                             sample = TRUE)
      choice_loc <- get_filter_unique_cat(f_df = f_data(),
                                          cat_var = input$sub_product_sumy_by,
                                          sample = FALSE)

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "sub_product_loc_cat",
                                      choices = choice_loc,
                                      selected = selected_locs)
    })

    output$sp_sale_profit_plt <- echarts4r::renderEcharts4r({
      shiny::req(f_data(), input$select_product, input$sub_product_agg_fun,
                 input$sub_product_sumy_by, input$sub_product_loc_cat)

      plt_ser_summary(f_df = f_data(),
                      fl_var = "product_name",
                      category = input$select_product,
                      gp_var = input$sub_product_sumy_by,
                      s_fun = input$sub_product_agg_fun,
                      gp_sample = input$sub_product_loc_cat)
    })


    # Sales Breakdown -------------------------------------------------------|
    output$sub_product_sales_bk <- echarts4r::renderEcharts4r({
      shiny::req(f_data())

      plt_percent_profit_cost(df = f_data())
    })

    # customer transaction Quantity summary ---------------------------------|
    output$sub_product_trans_qty_tbl_title <- shiny::renderUI({
      shiny::req(f_data())

      n_count_var <- length(unique(f_data()$customer_names))

      t_t <- glue::glue("{var_names$product_name} :: {clean_text(input$select_product)}")
      t_s <- glue::glue("Purchased By {n_count_var} Customers.")

      shiny::tagList(
        table_title(title = t_t),
        table_title(title = t_s, t_color = "#E3E3E3", t_size = 15)
      )
    })

    output$sub_product_trans_qty_tbl <- reactable::renderReactable({
      shiny::req(f_data(), input$sub_product_tq_agg_fun)

      rtbl_sub_pc_summary(f_df = f_data(),
                          count_var = "customer_names",
                          s_fun = input$sub_product_tq_agg_fun)
    })


    # customer Sales, cost & profit -----------------------------------------|
    output$sub_product_sale_profit_tbl_title <- shiny::renderUI({
      shiny::tagList(table_title(title = "Summary of Product Revenue"))
    })
    output$sub_product_sale_profit_tbl <- reactable::renderReactable({
      shiny::req(f_data(), input$sub_product_tq_agg_fun)

      rtbl_sub_sale_profit(f_df = f_data(),
                           count_var = "customer_names",
                           s_fun = input$sub_product_tq_agg_fun)
    })
    }
  )
}


