

#' set_check_widget
#' Opens a Shiny widget that allows for browsing potetial data entry issues
#' or exclude outliers.
#' @param dataSET SET data set as provided by set_get_sets()
#'
#' @return dataframe of selected points.
#' @export
#'
#' @examples
#'
set_check_widget <- function(dataSET) {

# UI ----
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("SET measures QA widget",
                   left =  miniUI::miniTitleBarCancelButton(),
                   right =  miniUI::miniTitleBarButton(inputId = "done", label = "Done", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::selectInput(inputId = "SETstation",
                         choices = unique(dataSET$Plot_Name), label = "Select SET Study Site Station"),
      shiny::actionButton(inputId = "reset", "Undo selections"),
      shiny::plotOutput("plot1", click = "plot_click"),
      shiny::dataTableOutput(outputId = "dtable")

    )
  )
# TODO: Clean up to make plots easier to read.
# TODO: Incorporate updateSelect to provide direction choices based on station selected.
  server <- function(input, output, session) {

    vals <- shiny::reactive(
     data = dataSET %>%
      filter(Plot_Name == input$SETstation) %>%
      select(Site_Name, SET_Type, Plot_Name, Arm_Direction, Pin_number, issuePin, Date, Raw, Notes, SET_Reader, incrementalChange))

    vals$keeprows  <-  rep(TRUE, nrow(vals$data))

    # Define reactive expressions, outputs, etc.
    output$plot1 <- renderPlot({
      includes <- data()[vals$keeprows, , drop = FALSE]
      excludes <- data()[!vals$keeprows, , drop = FALSE]

      includes() %>%
      ggplot(aes(x = Date, y = incrementalChange, group = pin_ID)) +
        geom_point(aes(color = issuePin)) +
        geom_line() +
        geom_smooth(method = "lm") +
        geom_point(data = excludes, shape = 21, fill = NA, alpha = .7) +
        scale_color_viridis_d() +
        facet_grid(rows = vars(Pin_number), cols = vars(Arm_Direction)) +
        theme_minimal()
    })
    output$dtable <- renderDataTable({
      shiny::nearPoints(df = includes %>% select(-pin_ID),
                        coordinfo = input$plot_click, allRows = F, xvar = "Date", yvar = "incrementalChange")
    })


    shiny::observeEvent(input$plot_click, {
      res <- nearPoints(data(), input$plot_click, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    shiny::observeEvent(input$reset, {vals$keeprows <- rep(TRUE, nrow())})
        # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      selectedPts <- includes
      shiny::stopApp(selectedPts)
    })
}


  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(dialogName = "SET Explorer", width = 800, height = 800))

}


