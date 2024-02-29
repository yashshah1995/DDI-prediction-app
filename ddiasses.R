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

# Define functions

getFGutRatio <- function (

  f_gut,
  k_inact   = NULL,
  k_deg_gut = NULL,
  I_gut,
  K_I_u,
  type = c("reversible", "irreversible")

) {

  type <- match.arg(type, c("reversible", "irreversible"))

  denom_f_gut <- switch (type,

    "reversible"   = {
      1 + sum(I_gut / K_I_u)
    },

    "irreversible" = {
      1 + sum(k_inact * I_gut / (k_deg_gut * (K_I_u + I_gut)))
    }

  )

  f_gut_ratio <- ((1 - f_gut) / denom_f_gut + f_gut)^-1

  return (f_gut_ratio)

}

getAUCRatio <- function (

  f_gut_ratio,
  f_m,
  f_m_cyp,
  k_inact = NULL,
  k_deg   = NULL,
  I_u,
  K_I_u,
  type = c("reversible", "irreversible")

) {

  type <- match.arg(type, c("reversible", "irreversible"))

  denom_auc <- switch (type,

    "reversible"   = {
      1 + sum(I_u / K_I_u)
    },

    "irreversible" = {
      1 + sum(k_inact * I_u / (k_deg * (K_I_u + I_u)))
    }

  )

  auc_ratio <- f_gut_ratio * (sum(f_m * f_m_cyp) / denom_auc + (1 - sum(f_m * f_m_cyp)))^-1

  return (auc_ratio)

}

f_gut_ratio <- selected_row_substr$fabs / selected_row_substr$fgut

auc_ratio1  <- getAUCRatio(
  f_gut_ratio = f_gut_ratio,
  f_m         = selected_row_substr$`fm (1-fe)`,
  f_m_cyp     = selected_row_substr$fmCYP3A4,
  I_u         = Iu_val,
  K_I_u       = selected_row_inhibtr$`Ki,u (µM)`,
  type        = "rev"
)



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
      textInput("name", "Enter a name"),
      actionButton("submit", "Submit")
    ))
  })

  # Observe the submit button in the modal
  observeEvent(input$submit, {

    # Add the name to the first column of the table
    selected_row_substr  <- df_substrate[df_substrate$`Enzyme full` == input$dropdown2, ]
    selected_row_inhibtr <- df_inhibitor[df_inhibitor$`Enzyme full` == input$dropdown3, ]

    #All results calculation

    #[I]u values

    lsys_val <- ((selected_row_inhibtr$F *
                    selected_row_inhibtr$`Dose (mg)`) /
                   (selected_row_inhibtr$`CL/F (mL/min)` *
                      selected_row_inhibtr$`τ (h)` *
                      selected_row_inhibtr$MW * 60)) * 1e6

    lmax_val <- ((selected_row_inhibtr$F *
                    selected_row_inhibtr$`τ (h)` * 60) /
                   (1 - exp(-selected_row_inhibtr$F *
                                     selected_row_inhibtr$`τ (h)` *
                                     60))) * lsys_val

    linlet_val <- (((selected_row_inhibtr$`Dose (mg)` *
                       selected_row_inhibtr$`ka (min-1)` *
                       selected_row_inhibtr$fabs *
                       selected_row_inhibtr$fgut) /
                      (df_bloodflowparams$Value[3] *
                         selected_row_inhibtr$MW)) * 1e6) + lsys_val

    Iu_val <- dplyr::case_when(
      input$dropdown4 == "Isys"   ~ lsys_val   * selected_row_inhibtr$fu,
      input$dropdown4 == "Imax"   ~ lmax_val   * selected_row_inhibtr$fu,
      input$dropdown4 == "Iinlet" ~ linlet_val * selected_row_inhibtr$fu,
      TRUE ~ NA_real_
      )

    auc_ratio1 <- (selected_row_substr$fabs / selected_row_substr$fgut) *
      (1 / (selected_row_substr$`fm (1-fe)` *
            selected_row_substr$fmCYP3A4 /
            (1 + (Iu_val/selected_row_inhibtr$`Ki,u (µM)`)) +
            (1 - selected_row_substr$`fm (1-fe)` *
               selected_row_substr$fmCYP3A4)))

    iguteq7 <- (selected_row_inhibtr$`Dose (mg)` *
                  selected_row_inhibtr$`ka (min-1)` *
                  selected_row_inhibtr$fabs /
                  (df_bloodflowparams$Value[2] *
                     selected_row_inhibtr$MW)) * 1e6


    gutcontribution <- 1 / (selected_row_substr$fgut +
                              ((1 - selected_row_substr$fgut) /
                                 (1 + (iguteq7 /
                                         selected_row_inhibtr$`Ki,u (µM)`))))

    auc_ratio2 <- gutcontribution *
      (1 / ((selected_row_substr$`fm (1-fe)` *
               selected_row_substr$fmCYP3A4 /
               (1 + (Iu_val / selected_row_inhibtr$`Ki,u (µM)`))) +
              (1 - selected_row_substr$`fm (1-fe)` *
                 selected_row_substr$fmCYP3A4)))

    #apply tdi if else
    auc_ratio3 <- (selected_row_substr$fabs / selected_row_substr$fgut) *
      (1 / ((selected_row_substr$fmCYP3A4 * selected_row_substr$`fm (1-fe)`) /
              (1 + (selected_row_inhibtr$`Kinact (min-1)` * Iu_val /
                      (df_humanphysioparams$`kdeg (min-1)`[6] *
                         (selected_row_inhibtr$`Ki,u (µM)` + Iu_val))))) +
         (1 - (selected_row_substr$fmCYP3A4 * selected_row_substr$`fm (1-fe)`)))

    auc_ratio4support1 <- 1 / (selected_row_substr$fmCYP3A4 *
                                 selected_row_substr$`fm (1-fe)` /
                                 (1 + (selected_row_inhibtr$`Kinact (min-1)` *
                                         Iu_val /
                                         (df_humanphysioparams$`kdeg (min-1)`[6] *
                                            (selected_row_inhibtr$`Ki,u (µM)` +
                                               Iu_val))))) +
      (1 - (selected_row_substr$fmCYP3A4 *
              selected_row_substr$`fm (1-fe)`))

    auc_ratio4support2 <- 1 / (selected_row_substr$fgut +
                                 ((1 -  selected_row_substr$fgut) /
                                    (1 + (selected_row_inhibtr$`Kinact (min-1)` *
                                            iguteq7 / (df_humanphysioparams$`kdeg (min-1)`[13] *
                                                         (selected_row_inhibtr$`Ki,u (µM)` +
                                                            iguteq7))))))
    #apply tdi if else
    auc_ratio4 <- auc_ratio4support2 * auc_ratio4support1

    new_row <- data.frame(DDI.ID       = input$name,
                          substrateval = selected_row_substr$`Enzyme full`,
                          figut        = selected_row_substr$fabs,
                          fgut         = selected_row_substr$fgut,
                          fm           = selected_row_substr$`fm (1-fe)`,
                          fm.CYP       = selected_row_substr$fmCYP3A4,
                          inhibval     = selected_row_inhibtr$`Enzyme full`,
                          MW           = selected_row_inhibtr$MW,
                          Dose         = selected_row_inhibtr$`Dose (mg)`,
                          tau          = selected_row_inhibtr$`τ (h)`,
                          f_uplasma    = selected_row_inhibtr$fu,
                          Kiu          = selected_row_inhibtr$`Ki,u (µM)`,
                          ka_          = selected_row_inhibtr$`ka (min-1)`,
                          f_abs        = selected_row_inhibtr$fabs,
                          f_gut        = selected_row_inhibtr$fgut,
                          "F"          = selected_row_inhibtr$F,
                          CLorCLF      = selected_row_inhibtr$`CL/F (mL/min)`,
                          elimrateK    = selected_row_inhibtr$F, #Formula and the excel sheet answer not matching, need to check
                          Kiu2   = ifelse(selected_row_inhibtr$`MBI/TDI` == 1,
                                          selected_row_inhibtr$`Ki,u (µM)`, "NA"),
                          Kinact = ifelse(selected_row_inhibtr$`MBI/TDI` == 1,
                                          selected_row_inhibtr$`Kinact (min-1)`, "NA"),
                          "[I]u"                    = Iu_val,
                          "AUC ratio (fgut = fabs)" = auc_ratio1,
                          "AUC ratio eqs 2,3"       = auc_ratio2,
                          "AUC ratio (TDI_ eq4)"    = auc_ratio3,
                          "AUC ratio (TDI_ eq4,5)"  = auc_ratio4
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
