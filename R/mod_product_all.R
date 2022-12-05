top_bottom_toggle <- function(id,
                              lab_on = "Top", lab_off = "Bottom",
                              in_line = FALSE) {

    shinyWidgets::prettyToggle(inputId = id,
                               label_on = lab_on,
                               label_off = lab_off,
                               value = TRUE,
                               status_on = "primary",
                               status_off = "success",
                               shape = "curve",
                               thick = TRUE,
                               bigger = TRUE,
                               outline = TRUE,
                               animation = "pulse",
                               inline = in_line)
}

n_products_numeric <- function(id, label, width = "150px") {
  shiny::numericInput(inputId = id,
                      label = label,
                      min = 5, max = 20, value = 10, step = 1,
                      width = width)
}



#' product_all UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_product_all_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      page_analysis_settings(
        dropdown_id = ns("all_prod_dropdown"),
        id_all = ns("all_prod_all"),
        id_19 = ns("all_prod_19"),
        id_20 = ns("all_prod_20"),
        id_21 = ns("all_prod_21"),

        max_width = 180,

        shiny::tags$br(),

        input_switch(id = ns("show_all_product_inputs"))
      )
    ),

    shiny::tags$br(),

    shinyjs::hidden(
      shiny::fluidRow(
        id = ns("all_prod_input1"),

        shiny::column(
          width = 1,
          class = "pt-5",

          top_bottom_toggle(id = ns("top_bottom_toggle"))
        ),
        shiny::column(
          width = 2,

          shiny::numericInput(inputId = ns("number_products"),
                              label = "number of products",
                              min = 5, max = 20, value = 10, step = 1,
                              width = "200px")
        ),
        shiny::column(
          width = 3,

          shinyWidgets::pickerInput(inputId = ns("prod_order_agg_fun"),
                                    label = "Aggregate By",
                                    choices = aggregate_functions,
                                    selected = "sum",
                                    width = "200px")
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          echarts4r::echarts4rOutput(outputId = ns("n_product_trans")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          echarts4r::echarts4rOutput(outputId = ns("n_prod_order")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    ),

    shinyjs::hidden(
      shiny::fluidRow(
        id = ns("all_prod_input2"),

        shiny::column(
          width = 6,

          top_bottom_toggle(id = ns("pl_highest_lowest"),
                            lab_on = "Highest", lab_off = "Lowest",
                            in_line = TRUE),

          location_picker_input(id = ns("loc_product_input"),
                                in_line = TRUE),

          aggregate_picker_input(id = ns("agg_product_input"),
                                 selected = "mean",
                                 in_line = TRUE)
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h540",

          shiny::uiOutput(outputId = ns("hl_prod_loc_sumy_title")),

          reactable::reactableOutput(outputId = ns("hl_prod_loc_sumy")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h540",

          shiny::uiOutput(outputId = ns("prod_loc_sumy_title")),

          reactable::reactableOutput(outputId = ns("prod_loc_sumy"), height = "438px") |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h487",

          shinyjs::hidden(
            shinyWidgets::pickerInput(inputId = ns("prod_rev_variables"),
                                      label = "Variable",
                                      choices = revenue_variables,
                                      selected = "profit",
                                      width = "300px")
          ),

          shiny::uiOutput(outputId = ns("prod_rev_summary_title")),

          reactable::reactableOutput(outputId = ns("prod_rev_summary")) |>
            shinycssloaders::withSpinner(type = 6, color = spinner_color)
        )
      ),
      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-h487",

          shinyjs::hidden(
            shiny::fluidRow(
              id = ns("all_prod_input3"),

              shiny::column(
                width = 6,

                shinyWidgets::pickerInput(inputId = ns("p_r_date"),
                                          label = "Date Variable",
                                          choices = date_variables[date_variables != "procureddate"],
                                          selected = "deliverydate")
              ),
              shiny::column(
                width = 6,

                shinyWidgets::pickerInput(inputId = ns("p_nr_date"),
                                          label = "Date Variable",
                                          choices = date_variables[date_variables != "deliverydate"],
                                          selected = "orderdate")
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              shiny::uiOutput(outputId = ns("product_date_diff_title")),

              table_title(title = "In Days", t_color = "#E3E3E3", t_size = 15),

              reactable::reactableOutput(outputId = ns("product_date_diff"), height = "382px") |>
                shinycssloaders::withSpinner(type = 6, color = spinner_color)
            )
          )
        )
      )
    )
  )
}







#' product_all Server Functions
#'
#' @param id module server id
#' @param s_data sales data
#' @param parent_session
#'
#' @noRd
mod_product_all_server <- function(id, s_data, parent_session) {

  stopifnot(shiny::is.reactive(s_data))

  shiny::moduleServer(
    id,

    function(input, output, session) {

    ns <- session$ns

    # Analysis period -------------------------------------------------------|
    shiny::observe({
      if (isTRUE(input$all_prod_19) || isTRUE(input$all_prod_20) ||
          isTRUE(input$all_prod_21)) {
        a_value <- FALSE

      } else {
        a_value <- TRUE
      }

      shinyWidgets::updatePrettySwitch(session = session,
                                       inputId = "all_prod_all",
                                       value   = a_value)
    })
    shiny::observe({
      if (isTRUE(input$all_prod_all)) {
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "all_prod_19",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "all_prod_20",
                                         value   = FALSE)
        shinyWidgets::updatePrettySwitch(session = session,
                                         inputId = "all_prod_21",
                                         value   = FALSE)
      }
    })


    # Filter period ---------------------------------------------------------|
    sf_data <- shiny::reactive({
      filter_period(df = s_data(),
                    y_all = input$all_prod_all,
                    y_19  = input$all_prod_19,
                    y_20  = input$all_prod_20,
                    y_21  = input$all_prod_21)
    }) |>
      shiny::debounce(1000)


    # hide & unhide inputs --------------------------------------------------|
    shiny::observe({
      if (input$show_all_product_inputs) {
        show_input("all_prod_input1")
        show_input("all_prod_input2")
        show_input("prod_rev_variables")
        show_input("all_prod_input3")

      } else {
        hide_input(id = "all_prod_input1")
        hide_input(id = "all_prod_input2")
        hide_input(id = "prod_rev_variables")
        hide_input(id = "all_prod_input3")
      }
    }) |>
      shiny::bindEvent(input$show_all_product_inputs)


    # Top/Bottom number of products -----------------------------------------|
    output$n_product_trans <- echarts4r::renderEcharts4r({
      shiny::req(sf_data(), input$number_products)

      count_chr_plt(df  = sf_data(),
                    gp_var = "product_name",
                    top = input$top_bottom_toggle,
                    n   = input$number_products)
    })


    # Top/bottom product by number of order ---------------------------------|
    output$n_prod_order <- echarts4r::renderEcharts4r({
      shiny::req(sf_data(), input$number_products, input$prod_order_agg_fun)

      plt_product_quantity(df  = sf_data(),
                           sumy_fun = input$prod_order_agg_fun,
                           top = input$top_bottom_toggle,
                           n   = input$number_products)
    })


    # Highest/lowest order quantity by product and location -----------------|
    output$hl_prod_loc_sumy_title <- shiny::renderUI({
      shiny::req(input$loc_product_input, input$agg_product_input)

      dirc <- ifelse(input$pl_highest_lowest, "highest", "Least")

      fun_lab <- ifelse(input$agg_product_input == "sum",
                        "Amount of",
                        clean_text(agg_names[[input$agg_product_input]]))

      t_t <- stringr::str_glue("Product & {clean_text(input$loc_product_input)} With The {dirc} {fun_lab} Order")

      shiny::tagList(table_title(title = t_t, use_html = FALSE))
    })

    output$hl_prod_loc_sumy <- reactable::renderReactable({
      shiny::req(sf_data(), input$loc_product_input, input$agg_product_input)

      min_max_product_order(df = sf_data(),
                            gp_var   = input$loc_product_input,
                            sumy_fun = input$agg_product_input,
                            get_max  = input$pl_highest_lowest)
    })


    # order quantity by product & location ----------------------------------|
    output$prod_loc_sumy_title <- shiny::renderUI({
      shiny::req(input$loc_product_input, input$agg_product_input)

      agg_lab <-  clean_text(agg_names[[input$agg_product_input]])

      t_t <- stringr::str_glue("{agg_lab} Quantity Order For All Products By {clean_text(input$loc_product_input)}")

      shiny::tagList(table_title(title = t_t))
    })

    output$prod_loc_sumy <- reactable::renderReactable({
      shiny::req(sf_data(), input$loc_product_input, input$agg_product_input)

      rtbl_product_quantity(df = sf_data(),
                            gp_var2  = input$loc_product_input,
                            sumy_fun = input$agg_product_input)
    })


    # Product summary by revenue features -----------------------------------|
    output$prod_rev_summary_title <- shiny::renderUI({
      shiny::req(input$prod_rev_variables)

      if (input$prod_rev_variables %in% c("cost", "discount")) {
        title_clr <- neg_color
      } else {
        title_clr <- pos_color
      }
      t_t <- stringr::str_glue("Product Summary By <span style='color:{title_clr};'>{clean_text(input$prod_rev_variables)}</span>")

      shiny::tagList(table_title(title = t_t, use_html = TRUE))
    })

    output$prod_rev_summary <- reactable::renderReactable({
      shiny::req(sf_data(), input$prod_rev_variables)

      ftbl_product_summary_by(df = sf_data(),
                              sumy_var = input$prod_rev_variables)
    })


    # Date Difference -------------------------------------------------------|
    shiny::observe({
      if(!is.null(input$p_nr_date)) {
        shinyWidgets::updatePickerInput(session  = session,
                                        inputId  = "p_r_date",
                                        choices  = dyn_date(x = input$p_nr_date, recent = TRUE),
                                        selected = isolate(input$p_r_date))
      }
    })
    shiny::observe({
      if(!is.null(input$p_r_date)) {
        shinyWidgets::updatePickerInput(session  = session,
                                        inputId  = "p_nr_date",
                                        choices  = dyn_date(x = input$p_r_date, recent = FALSE),
                                        selected = isolate(input$p_nr_date))
      }
    })

    output$product_date_diff_title <- shiny::renderUI({
      shiny::req(input$p_r_date, input$p_nr_date)

      t_t <- glue::glue("Difference Between {date_names[[input$p_r_date]]} & {date_names[[input$p_nr_date]]} Date")

      shiny::tagList(table_title(title = t_t))
    })

    output$product_date_diff <- reactable::renderReactable({
      shiny::req(sf_data(), input$p_r_date, input$p_nr_date)

      rtbl_product_datediff(df = sf_data(),
                            date_var1 = input$p_r_date,
                            date_var2 = input$p_nr_date)
    })
    }
  )
}
