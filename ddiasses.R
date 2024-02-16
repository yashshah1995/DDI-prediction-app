# Load necessary libraries
library(shiny)
library(knitr)
library(kableExtra)
library(readxl) # for reading Excel files

# Read the Excel file
df2 <- read_excel("DDI template - Copy.xlsx", sheet = "Substrates for various CYPs")

# Define UI
ui <- fluidPage(
  titlePanel("DDI assessment tool"),
  tabsetPanel(
    tabPanel("Tab 1",
             fluidRow( # Switch to fluidRow for horizontal layout
               column(3, selectInput("dropdown1", "Reversible/timedep inhib",
                                     choices = (c("CYP Substrate",
                                                  "Reversible Inhibitor",
                                                  "TDI"
                                     )))),
               column(3, selectInput("dropdown2", "Select substrates",
                                     choices = unique(df2[["Enzyme full"]]))),
               column(3, selectInput("dropdown3", "Select Inhibitors", choices = NULL)),
               column(3, selectInput("dropdown4", "[i]", choices = NULL),
                      actionButton("addrow", "Add row")) # Add row button
             ),
             mainPanel(
               htmlOutput("table")
             )
    ),
    tabPanel("Tab 2",
             htmlOutput("table2") # Output for the second table
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize the table data
  table_data <- reactiveVal(data.frame(Compound.ID = character(),
                                      substrateval = character() ,
                                       figut = character(),
                                      fgut = character(),
                                      fm = character(),
                                      fm.CYP = character()))

  # Render the table
  output$table <- renderUI({
    df <- table_data()

    # Create table with multi-level headers
    table_html <- kable(df, "html", row.names = FALSE) %>%
      kable_styling() %>%
      add_header_above(c(" " = 2, "SUBSTRATE (VICTIM)" = 4), line = TRUE) %>%
      column_spec(1, width = "50px") # Limit the width of the first column

    # Render the HTML code as a table
    HTML(table_html)
  })

  # Show a modal when the user clicks the "Add row" button
  observeEvent(input$addrow, {
    showModal(modalDialog(
      textInput("name", "Enter a name"),
      actionButton("submit", "Submit")
    ))
  })

  # Observe the submit button in the modal
  observeEvent(input$submit, {
    # Add the name to the first column of the table
    selected_row <- df2[df2$`Enzyme full` == input$dropdown2, ]
    new_row <- data.frame(Compound.ID = input$name,
                          substrateval = selected_row$`Enzyme full`,
                          figut = selected_row$fabs,
                          fgut = selected_row$fgut,
                          fm = selected_row$`fm (1-fe)`,
                          fm.CYP = selected_row$fmCYP3A4)
    table_data(rbind(table_data(), new_row))

    # Close the modal
    removeModal()
  })

  # Render the second table
  output$table2 <- renderUI({
    # Create table with kable
    table_html2 <- kable(df2, "html") %>%
      kable_styling()

    # Render the HTML code as a table
    HTML(table_html2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
