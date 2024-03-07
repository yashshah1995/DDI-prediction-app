# Load necessary libraries
library(shiny)
library(knitr)
library(kableExtra)
library(readxl) # for reading Excel files

# Read the Excel file
df_substrate <- read_excel("DDI template - Copy (version 1).xlsb.xlsx",
                           sheet = "Substrates for various CYPs")

df_inhibitor <- read_excel("DDI template - Copy (version 1).xlsb.xlsx",
                           sheet = "Inhibitors for various CYPs")

df_bloodflowparams <- read_excel("DDI template - Copy (version 1).xlsb.xlsx",
                                 sheet = "bloodflowparameters")

df_humanphysioparams <- read_excel("DDI template - Copy (version 1).xlsb.xlsx",
                                   sheet = "humanphysiologparams")

df <- read.csv("varmap.csv")

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
               column(3, selectInput("dropdown4", "[i]", choices = c("Isys",
                                                                     "Imax",
                                                                     "Iinlet")),
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

vals_cal <- function(DDI.ID, substrateval, figut, fgut, fm, fm.CYP,
                     inhibval, MW, Dose, tau, f_uplasma, Kiu, ka_,
                     f_abs, f_gut, Fval, CLorCLF, elimrateK, Kiu2, Kinact, ipdrop4,
                     df_bloodflowparamsval3,
                     df_bloodflowparamsval2,
                     df_humanphysioparamskdeg6,
                     df_humanphysioparamskdeg13) {

  lsys_val <- ((Fval * Dose) / (CLorCLF * tau * MW * 60)) * 1e6

  lmax_val <- ((Fval * tau * 60) / (1 - exp(-Fval * tau * 60))) * lsys_val

  linlet_val <- (((Dose * ka_ * f_abs * f_gut) / (df_bloodflowparamsval3
                                                  * MW)) * 1e6) + lsys_val

  I_u <- dplyr::case_when(
    ipdrop4 == "Isys"   ~ lsys_val   * f_uplasma,
    ipdrop4 == "Imax"   ~ lmax_val   * f_uplasma,
    ipdrop4 == "Iinlet" ~ linlet_val * f_uplasma,
    TRUE ~ NA_real_
  )


  # Results

  auc_ratio1 <- (figut / fgut) * (1 / (fm * fm.CYP / (1 + (I_u / Kiu)) +
                                         (1 - fm * fm.CYP)))

  iguteq7 <- (Dose * ka_ * f_abs / (df_bloodflowparamsval2 * MW)) * 1e6

  gutcontribution <- 1 / (fgut + ((1 - fgut) / (1 + (iguteq7 / Kiu))))

  auc_ratio2 <- gutcontribution * (1 / ((fm * fm.CYP / (1 + (I_u / Kiu))) +
                                          (1 - fm * fm.CYP)))

  # apply tdi if else
  auc_ratio3 <- (figut / fgut) *
    (1 / ((fm.CYP * fm) / (1 + (Kinact * I_u / (df_humanphysioparamskdeg6 *
                                                  (Kiu + I_u))))) + (1 - (fm.CYP * fm)))

  auc_ratio4support1 <- 1 / (fm.CYP * fm / (1 + (Kinact * I_u /
                                                   (df_humanphysioparamskdeg6 * (Kiu + I_u))))) + (1 - (fm.CYP * fm))

  auc_ratio4support2 <- 1 / (fgut + ((1 - fgut) / (1 + (Kinact * iguteq7 /
                                                          (df_humanphysioparamskdeg13 * (Kiu + iguteq7))))))

  # apply tdi if else
  auc_ratio4 <- auc_ratio4support2 * auc_ratio4support1

  return(list(Iu_val = I_u, auc_ratio1 = auc_ratio1,
              auc_ratio2 = auc_ratio2, auc_ratio3 = auc_ratio3,
               auc_ratio4 = auc_ratio4))


}


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
      add_header_above(c(" "                                  = 1,
                         "SUBSTRATE (VICTIM)"                 = 5,
                         "REVERSIBLE INHIBITOR (PERPETRATOR)" = 12,
                         "TDI"                                = 2,
                         "RESULTS"                            = 5
                         ),
                       line = TRUE) %>%
      column_spec(1, width = "50px") # Limit the width of the first column

    # Render the HTML code as a table
    HTML(table_html)
  })


  # Show a modal when the user clicks the "Add row" button
  observeEvent(input$addrow, {
    showModal(modalDialog(
      radioButtons("selection", "Choose input type:",
                   choices = list("Default values" = "default", "Custom values" = "custom"),
                   selected = "default"),
      conditionalPanel(
        condition = "input.selection == 'default'",
        textInput("name1", "Enter a name")
      ),
      conditionalPanel(
        condition = "input.selection == 'custom'",
        textInput("name2", "Enter a name"),
        numericInput(df$ids[1], df$Excel.names[1], value = 0),
        numericInput(df$ids[2], df$Excel.names[2], value = 0),
        numericInput(df$ids[3], df$Excel.names[3], value = 0),
        numericInput(df$ids[4], df$Excel.names[4], value = 0),
        numericInput(df$ids[5], df$Excel.names[5], value = 0),
        numericInput(df$ids[6], df$Excel.names[6], value = 0),
        numericInput(df$ids[7], df$Excel.names[7], value = 0),
        numericInput(df$ids[8], df$Excel.names[8], value = 0),
        numericInput(df$ids[9], df$Excel.names[9], value = 0),
        numericInput(df$ids[10], df$Excel.names[10], value = 0),
        numericInput(df$ids[11], df$Excel.names[11], value = 0),
        numericInput(df$ids[13], df$Excel.names[13], value = 0),
        numericInput(df$ids[14], df$Excel.names[14], value = 0),
        numericInput(df$ids[16], df$Excel.names[16], value = 0),
        numericInput(df$ids[17], df$Excel.names[17], value = 0),
        numericInput(df$ids[20], df$Excel.names[20], value = 0),
        numericInput(df$ids[21], df$Excel.names[21], value = 0)
      ),
      actionButton("submit", "Submit")
    ))
  })



  # Observe the submit button in the modal
  observeEvent(input$submit,{

    if(input$selection == "default") {

      selected_row_substr  <- df_substrate[df_substrate$`Enzyme full` == input$dropdown2, ]
      selected_row_inhibtr <- df_inhibitor[df_inhibitor$`Enzyme full` == input$dropdown3, ]


      DDI.ID = input$name1
      substrateval = selected_row_substr$`Enzyme full`
      figut = selected_row_substr$fabs
      fgut = selected_row_substr$fgut
      fm = selected_row_substr$`fm (1-fe)`
      fm.CYP = selected_row_substr$fmCYP3A4
      inhibval = selected_row_inhibtr$`Enzyme full`
      MW = selected_row_inhibtr$MW
      Dose = selected_row_inhibtr$`Dose (mg)`
      tau = selected_row_inhibtr$`τ (h)`
      f_uplasma = selected_row_inhibtr$fu
      Kiu = selected_row_inhibtr$`Ki,u (µM)`
      ka_ = selected_row_inhibtr$`ka (min-1)`
      f_abs = selected_row_inhibtr$fabs
      f_gut = selected_row_inhibtr$fgut
      Fval = selected_row_inhibtr$F
      CLorCLF = selected_row_inhibtr$`CL/F (mL/min)`
      elimrateK = selected_row_inhibtr$F
      Kiu2 = ifelse(selected_row_inhibtr$`MBI/TDI` == 1, selected_row_inhibtr$`Ki,u (µM)`, NA)
      Kinact = ifelse(selected_row_inhibtr$`MBI/TDI` == 1, selected_row_inhibtr$`Kinact (min-1)`, NA)

    }
    else {

      DDI.ID = input$name2
      substrateval = "Custom"
      figut = input[[df$ids[1]]]
      fgut = input[[df$ids[2]]]
      fm = input[[df$ids[3]]]
      fm.CYP = input[[df$ids[4]]]
      inhibval = "Custom"
      MW = input[[df$ids[5]]]
      Dose = input[[df$ids[6]]]
      tau = input[[df$ids[7]]]
      f_uplasma = input[[df$ids[8]]]
      Kiu = input[[df$ids[9]]]
      ka_ = input[[df$ids[10]]]
      f_abs = input[[df$ids[11]]]
      f_gut = input[[df$ids[13]]]
      Fval = input[[df$ids[14]]]
      CLorCLF = input[[df$ids[16]]]
      elimrateK = input[[df$ids[17]]]
      Kiu2 = input[[df$ids[20]]]
      Kinact = input[[df$ids[21]]]

    }


    results <- vals_cal(DDI.ID, substrateval, figut, fgut, fm, fm.CYP,
                                 inhibval, MW, Dose, tau, f_uplasma, Kiu, ka_,
                                 f_abs, f_gut, Fval, CLorCLF, elimrateK, Kiu2, Kinact,
                        input$dropdown4,
                        df_bloodflowparams$Value[3],
                        df_bloodflowparams$Value[2],
                        df_humanphysioparams$`kdeg (min-1)`[6],
                        df_humanphysioparams$`kdeg (min-1)`[13])


    new_row <- data.frame("DDI.ID" = DDI.ID,
                          "substrateval" = substrateval,
                          "figut" = figut,
                          "fgut" = fgut,
                          "fm" = fm,
                          "fm.CYP" = fm.CYP,
                          "inhibval" = inhibval,
                          "MW" = MW,
                          "Dose" = Dose,
                          "tau" = tau,
                          "f_uplasma" = f_uplasma,
                          "Kiu" = Kiu,
                          "ka_" = ka_,
                          "f_abs" = f_abs,
                          "f_gut" = f_gut,
                          "Fval" = Fval,
                          "CLorCLF" = CLorCLF,
                          "elimrateK" = elimrateK,
                          "Kiu2" = Kiu2,
                          "Kinact" = Kinact,
                          "[I]u" = results$Iu_val,
                          "AUC ratio (fgut = fabs)" = results$auc_ratio1,
                          "AUC ratio eqs 2,3"       = results$auc_ratio2,
                          "AUC ratio (TDI_ eq4)"    = results$auc_ratio3,
                          "AUC ratio (TDI_ eq4,5)"  = results$auc_ratio4
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
