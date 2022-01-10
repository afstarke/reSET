

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
set_check_widget <- function(dataSET, val) {


  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("SET measures QA widget",
                   left =  miniUI::miniTitleBarCancelButton(),
                   right =  miniUI::miniTitleBarButton(inputId = "done", label = "Done", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::selectInput(inputId = "SETstation", choices = unique(dataSET$Plot_Name), label = "Select SET Study Site"),
      shiny::selectInput(inputId = "Direction", choices = unique(dataSET$Arm_Direction), label = "Choose arm direction:"),
      shiny::plotOutput("plot1", brush = "plot_brush"),
      shiny::dataTableOutput(outputId = "dtable")

    )
  )
# TODO: Clean up to make plots easier to read.
# TODO: Incorporate updateSelect to provide direction choices based on station selected.
  server <- function(input, output, session) {
    data <- reactive({dataSET %>%
      filter(Plot_Name == input$SETstation, Arm_Direction == input$Direction) %>%
      select(Site_Name, SET_Type, Plot_Name, Arm_Direction, Pin_number, issuePin, Date, Raw, Notes, SET_Reader, incrementalChange)})

    # Define reactive expressions, outputs, etc.
    output$plot1 <- renderPlot({

      data() %>%
      ggplot(aes(x = Date, y = incrementalChange, group = pin_ID, label = Pin_number)) +
        geom_point(aes(color = issuePin)) + geom_text() +
        geom_line() +
        scale_color_viridis_d() +
        facet_grid(rows = vars(Pin_number), cols = vars(Arm_Direction)) +
        theme_minimal()
    })
    output$dtable <- renderDataTable({
      shiny::brushedPoints(df = data() %>% select(-pin_ID),
                    brush = input$plot_brush, allRows = F, xvar = "Date", yvar = "incrementalChange")
    })

        # When the Done button is clicked, return a value
    observeEvent(input$done, {
      selectedPts <- shiny::brushedPoints(data(), brush = input$plot_brush)
      shiny::stopApp(selectedPts)
    })
}


  shiny::runGadget(ui, server, viewer = shiny::browserViewer())

}


