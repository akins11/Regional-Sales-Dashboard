#' <component> description box
#'
#' @param textOne header text.
#' @param valueOne header value.
#' @param textTwo  second header text.
#' @param valueTwo second header value.
#' @param rightBorder whether to add a border at the right side of the box.
#' @param textThree third text.
#' @param valueThree third value.
#' @param iconThree icon to be added to the third value.
#' @param colorThree the color of the third value.
#'
#' @return html tool tag.
#' @export
#'
description_block <- function(textOne = NULL, valueOne = NULL,
                              textTwo = NULL, valueTwo = NULL, rightBorder = TRUE,
                              textThree = NULL, valueThree = NULL, iconThree = NULL,
                              colorThree = "blue") {
  cls <- "description-block"

  if (isTRUE(rightBorder)) {
    cls <- paste0(cls, " border-right")
  }
  if (colorThree == "blue") {
    p_cls <- "description-value-percent-blue"
  } else if (colorThree == "red") {
    p_cls <- "description-value-percent-red"
  }

  shiny::tags$div(
    class = cls,
    shiny::tags$span(
      class = "description-text",
      textOne
    ),
    shiny::tags$h5(
      class = "description-value",
      valueOne
    ),
    shiny::tags$span(
      textTwo,
      class = "description-text"
    ),
    shiny::tags$h5(
      class = "description-value",
      valueTwo
    ),
    shiny::tags$span(
      class = "description-text-percent",
      textThree
    ),
    shiny::tags$h5(
      class = p_cls,
      valueThree,
      if (!is.null(iconThree)) iconThree
    )
  )
}



#' bootstrap 5 card.
#'
#' @param ... all ui components to include.
#' @param class_c class for the whole card.
#' @param class_cb class for the card body.
#'
#' @return
#' @export
#'
bs_card <- function(..., class_c = "", class_cb = "") {

  shiny::div(
    class = glue::glue("card {class_c}"),

    shiny::div(
      class = glue::glue("card-body {class_cb}"),

      ...
    )
  )
}



#' <component> description box
#'
#' @param text1  header text.
#' @param value1 header value.
#' @param text2  second header text.
#' @param value2 second header value.
#' @param text3 third text.
#' @param value3 third value.
#' @param color3 color for the percentage change.
#'
#' @return
#' @export
#'
description_card <- function(text1 = NULL, value1 = NULL,
                             text2 = NULL, value2 = NULL,
                             text3 = NULL, value3 = NULL,
                             color3 = NULL) {
  if (!is.null(text3) && !is.null(color3)) {
    text_color <- ifelse(color3 == "blue", "#007FFF", "#FF3030")

    third_desc_value <- shiny::div(
      class = "mt-2",

      shiny::span(text3, class = "fs-6 text-mute fw-lighter"),

      shiny::h6(value3,
                class = "fs-6 fw-normal m-0 p-2",
                style = glue::glue("color: {text_color};"))
    )
  } else {
    third_desc_value <- shiny::div(class = "m-0 p-1")
  }


  shiny::div(
    class = "card shadow-none border rounded-1 text-center m-2",
    style = "border-color: #FCFCFC !important;",

    shiny::div(
      shiny::span(text1, class = "fs-5 fw-light"),

      shiny::h6(value1, class = "fs-6 fw-bold m-0")
    ),

    shiny::div(
      class = "mt-2",

      shiny::span(text2, class = "fs-6 fw-lighter"),

      shiny::h6(value2, class = "fs-6 fw-bold m-0")
    ),

    third_desc_value
  )
}


#' Page analysis filter drop down
#'
#' @param dropdown_id id of the drop down menu.
#' @param id_all id for selecting all the years.
#' @param id_19 id for selecting 2019.
#' @param id_20 id for selecting 2020.
#' @param id_21 id for selecting 2021.
#'
#' @return shiny widgets drop down menu.
#' @export
#'
#' @examples
page_analysis_settings <- function(dropdown_id,
                                   id_all, id_19, id_20, id_21,
                                   max_width = 180, ...) {

  ps_input <- function(s_id, s_label, s_value) {
    shinyWidgets::prettySwitch(inputId = s_id,
                               label = s_label,
                               value = s_value,
                               status = "primary",
                               slim = TRUE,
                               bigger = TRUE)
  }

  shinyWidgets::dropMenu(
    shinyWidgets::actionBttn(inputId = dropdown_id,
                             label = "",
                             icon  = fontawesome::fa("fas fa-filter",
                                                     fill  = "#858585",
                                                     height = "1.2em",
                                                     width = "1.1em",
                                                     title = "Filter"),
                             style = "material-flat",
                             color = "default",
                             size  = "sm"),
    tags$h5("Analysis Year"),

    ps_input(id_all, "ALL", TRUE),
    ps_input(id_19, "2019", FALSE),
    ps_input(id_20, "2020", FALSE),
    ps_input(id_21, "2021", FALSE),

    ...,

    theme = "translucent",
    maxWidth = max_width
  )
}


#' <component> add title to a table.
#'
#' @param title title text.
#' @param use_html whether to render text as html.
#' @param t_color color of the text.
#' @param t_size the size of the text.
#'
#' @return html tool h2 tag.
#' @export
#'
#' @examples
#'
table_title <- function(title, use_html = FALSE, t_color = "#DDDDDD", t_size = 20) {
  style <- paste0("color: ", t_color, " ;", "font-size: ", t_size, "px ;")

  if (isTRUE(use_html)) {
    htmltools::h2(
      htmltools::HTML(text = title),
      style = style
    )
  } else {
    htmltools::h2(title, style = style)
  }
}


#' <component> wrapper for shinyWidget picker input.
#'
#' @param id input id.
#' @param selected the selected choice.
#' @param in_line  render input inline.
#'
#' @return shiny select input Widget for geo location choice.
#' @export
#'
#' @examples
location_picker_input <- function(id, selected = "region", in_line = FALSE) {
  shinyWidgets::pickerInput(inputId = id,
                            label = "Geo Area",
                            choices = location_names,
                            selected = selected,
                            inline = in_line)
}


#' <component> wrapper for shinyWidget picker input.
#'
#' @param id input id.
#' @param selected the selected choice.
#' @param in_line render input inline.
#'
#' @return shiny select input Widget for aggregate function choice.
#' @export
#'
#' @examples
#'
aggregate_picker_input <- function(id, selected = "sum", in_line = FALSE) {
  shinyWidgets::pickerInput(inputId = id,
                            label = "Aggregate By",
                            choices = aggregate_functions,
                            selected = selected,
                            inline = in_line)
}



#' ui inputs
#'
#' @param id ui row input.
#' @param id1 id of the geographical area select input.
#' @param id2 id of the aggregate function select input.
#' @param id3 id of the location select input.
#'
#' @return
#' @export
#'
input_sales_profit <- function(id, id1, id2, id3) {
  shiny::fluidRow(
    id = id,

    shiny::column(
      width = 4,

      shinyWidgets::pickerInput(inputId = id1,
                                label = "Geo Area",
                                choices = location_names)
    ),
    shiny::column(
      width = 4,

      shinyWidgets::pickerInput(inputId = id2,
                                label = "Aggregate By",
                                choices = aggregate_functions,
                                selected = "sum")
    ),
    shiny::column(
      width = 4,

      shinyWidgets::pickerInput(inputId = id3,
                                label = "Select Location",
                                choices = NULL,
                                multiple = TRUE,
                                options = shinyWidgets::pickerOptions(liveSearch = TRUE,
                                                                      maxOptions = 6,
                                                                      maxOptionsText = "Only 6 categories can be selected",
                                                                      multipleSeparator = " | "))
    )
  )
}


#' <component> wrapper for shinyWidget pretty Switch input.
#'
#' @param id  input id.
#' @param value TRUE or FALSE.
#' @param in_line render input inline.
#'
#' @return shiny check box input Widget for showing inputs.
#' @export
#'
#' @examples
#'
input_switch <- function(id, value = FALSE, in_line = FALSE) {
  shinyWidgets::prettySwitch(inputId = id,
                             value = value,
                             label = "Show Inputs",
                             slim = TRUE,
                             status = "primary",
                             inline = in_line)
}


#' wrapper for shinyjs show function.
#'
#' @param id input id.
#'
#' @return
#' @export
#'
#' @examples
show_input <- function(id) {
  shinyjs::show(id = id, anim = TRUE, animType = "slide")
}


#' wrapper for shinyjs hide function.
#'
#' @param id input id.
#'
#' @return
#' @export
#'
#' @examples
hide_input <- function(id) {
  shinyjs::hide(id = id, anim = TRUE, animType = "slide")
}


#' Get the available year choices
#'
#' @param df data frame or reactive data frame
#'
#' @return a vector
#' @export
#'
#' @examples
get_year_choice <- function(df) {
  lubridate::year(df$orderdate) |> unique()
}



#' Statistic card
#'
#' @param value card value.
#' @param value_lab value to display
#' @param subtitle card subtitle.
#' @param bg_color background color.
#'
#' @return
#' @export
#'
#' @examples
bs_stati_card <- function(value, value_lab, subtitle, bg_color = "#FFFFFF") {
  if (!is.null(value)) {
    if (value > 0) {
      s_icon <- "fas fa-arrow-trend-up"
      s_color <- "#007FFF"

    } else if (value < 0) {
      s_icon <- "fas fa-arrow-trend-down"
      s_color <- "#CD3333"

    } else {
      s_icon <- "minus"
      s_color <- "#ABABAB"
    }

    shiny::div(
      class = "card rounded-0",
      style = glue::glue("width: 25rem; background-color: {bg_color}"),

      shiny::div(
        class = "card-body p-2",

        shiny::div(
          class = "d-flex flex-row justify-content-between align-items-center",

          shiny::div(
            class = "ps-2",

            fontawesome::fa(name = s_icon, fill = s_color, height = "3.5em")
          ),

          shiny::div(
            class = "pe-2",
            style = glue::glue("color: {s_color};"),

            shiny::h3(
              class = "fs-1 fw-bold text-end",

              value_lab
            ),

            shiny::h6(
              class = "fs-5 fw-light text-end",

              subtitle
            )
          )
        )
      )
    )
  }
}



#' input toggle button.
#'
#' @param inputId button input id.
#' @param value button active state.
#'
#' @return toggle button.
#' @export
#'
#' @examples
toggle_btn <- function(inputId, value = FALSE) {

  icon_up   <- fontawesome::fa_i("fas fa-circle-chevron-up")
  icon_down <- fontawesome::fa_i("fas fa-circle-chevron-down")

  icon_up$attribs$class   <- paste("icon", icon_up$attribs$class)
  icon_down$attribs$class <- paste("icon", icon_down$attribs$class)

  inputTag <- tags$input(id = inputId, type = "checkbox")

  if (!is.null(value) && value) {
    inputTag$attribs$checked <- "checked"
  }

  toggleTag <- tags$div(
    class = "form-group shiny-input-container",
    style = "padding-left: 92%;",

    tags$div(
      class = "pretty p-toggle",
      inputTag,
      class = "p-plain p-bigger p-icon",

      tags$div(
        class = "state p-on p-default",
        icon_up
      ),

      tags$div(
        class = "state p-off p-default",
        icon_down
      )
    )
  )

  shinyWidgets:::attachShinyWidgetsDep(toggleTag, "pretty")
}



#' Loading/busy screen.
#'
#' @param ouput_element a ui output element.
#' @param s_color the color of the loading image.
#'
#' @return
#' @export
#'
output_spinner <- function(ouput_element, s_color = spinner_color) {
  shinycssloaders::withSpinner(ui_element = ouput_element,
                               type = 6,
                               color = s_color)
}
