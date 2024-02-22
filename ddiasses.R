# Load necessary libraries
library(shiny)
library(knitr)
library(kableExtra)
library(readxl) # for reading Excel files

# Read the Excel file
df_substrate <- read_excel("DDI template - Copy.xlsx", sheet = "Substrates for various CYPs")
df_inhibitor <- read_excel("DDI template - Copy.xlsx", sheet = "Inhibitors for various CYPs")

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
                                     choices = unique(df_substrate[["Enzyme full"]]))),
               column(3, selectInput("dropdown3", "Select Inhibitors",
                                     choices = unique(df_inhibitor[["Enzyme full"]]))),
               column(3, selectInput("dropdown4", "[i]", choices = NULL),
                      actionButton("addrow", "Add row")) # Add row button
             ),
             mainPanel(
               htmlOutput("table")
             )
    ),
    tabPanel("Tab 2",
             htmlOutput("table2") # Output for the second table
    ),
    tabPanel("Tab 3",
             htmlOutput("table3") # Output for the second table
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize the table data
  table_data <- reactiveVal(data.frame())

  # Render the table
  output$table <- renderUI({
    df <- table_data()

    # Create table with multi-level headers
    table_html <- kable(df, "html", row.names = FALSE) %>%
      kable_styling() %>%
      add_header_above(c(" " = 1,
                         "SUBSTRATE (VICTIM)" = 5,
                         "REVERSIBLE INHIBITOR (PERPETRATOR)" = 12,
                         "TDI" = 2
                         ),
                       line = TRUE) %>%
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
    selected_row_substr <- df_substrate[df_substrate$`Enzyme full` == input$dropdown2, ]
    selected_row_inhibtr <- df_inhibitor[df_inhibitor$`Enzyme full` == input$dropdown3, ]
    new_row <- data.frame(Compound.ID = input$name,
                          substrateval = selected_row_substr$`Enzyme full`,
                          figut = selected_row_substr$fabs,
                          fgut = selected_row_substr$fgut,
                          fm = selected_row_substr$`fm (1-fe)`,
                          fm.CYP = selected_row_substr$fmCYP3A4,
                          inhibval = selected_row_inhibtr$`Enzyme full`,
                          MW = selected_row_inhibtr$MW,
                          Dose = selected_row_inhibtr$`Dose (mg)`,
                          tau = selected_row_inhibtr$`τ (h)`,
                          f_uplasma = selected_row_inhibtr$fu,
                          Kiu = selected_row_inhibtr$`Ki,u (µM)`,
                          ka_ = selected_row_inhibtr$`ka (min-1)`,
                          f_abs = selected_row_inhibtr$fabs,
                          f_gut = selected_row_inhibtr$fgut,
                          "F" = selected_row_inhibtr$F,
                          CLorCLF = selected_row_inhibtr$`CL/F (mL/min)`,
                          elimrateK = "not sure",
                          Kiu2 = ifelse(selected_row_inhibtr$`MBI/TDI` == 1,
                                        selected_row_inhibtr$`Ki,u (µM)`,"NA"),
                          Kinact = ifelse(selected_row_inhibtr$`MBI/TDI` == 1,
                                          selected_row_inhibtr$`Kinact (min-1)`,"NA")
                          )
    table_data(rbind(table_data(), new_row))

    # Close the modal
    removeModal()
  })

  # Render the second table
  output$table2 <- renderUI({
    # Create table with kable
    table_html2 <- kable(df_substrate, "html") %>%
      kable_styling()

    # Render the HTML code as a table
    HTML(table_html2)
  })

  # Render the third table
  output$table3 <- renderUI({
    # Create table with kable
    table_html3 <- kable(df_inhibitor, "html") %>%
      kable_styling()

    # Render the HTML code as a table
    HTML(table_html3)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
