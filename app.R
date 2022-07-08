options(shiny.maxRequestSize = 80*1024^2)
library(tidyverse)
library(readxl)
library(shiny)
library(shinyjs)
library(readr)
library(gargle)
library(shinythemes)
library(DT)
library(shinyalert)
library(googledrive)
library(googlesheets4)

options(gargle_oauth_cache = ".secrets")
drive_auth(email ="nextgen.flowering.app@gmail.com", cache = ".secrets")
gs4_auth(token = drive_token())

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
    tags$head(HTML("<title>Flowering app</title>")), 
    # Application title
    titlePanel(h2(strong("NextGen Cassava Flowering Application"), style="color:lightblue")),
    hr(), br(),

    sidebarLayout(
        sidebarPanel(
            useShinyalert(),
            actionButton("Format", "Download Template", class = "btn-success", style = "width:100%"),
            br(), br(),
            textInput("userName", "Please enter your name"),
            uiOutput("selectLocation", label = "Select a location"),
            uiOutput("locationInput"),
            fileInput("inputFile", "Choose your flowering data file",multiple = FALSE, 
                      accept = c(".csv", "text/csv",".xlsx",".xls", "text/comma-seperated-values", "text/plain" )),
            hr(),
            fluidRow(column(3, uiOutput("submit")), column(1), column(3, uiOutput("submit2")))
        ),

        ##### SHOW RESULT FROM THE DATA ENTERED
        mainPanel(
          fluidRow(
            column(3, span(textOutput("resHeader"),style="font-size: 20px; font-weight: light; color: lightblue")),
            column(1),
          ),
          hr(),
          DT::dataTableOutput("finalResult"),
          hr(), br(),
          fluidRow(column(1), 
                   column(3, uiOutput("button1")), 
                   column(1),
                   column(3, uiOutput("button2"))),
          hr(), br(),
        )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  validator <- function(){
    valid <- (input$userName == "" || is.null(input$userName)) || is.null(input$inputFile) ||
       input$selectedLocation == "Others" && 
      (input$enterLocation == "" || is.null(input$enterLocation))
    return(valid)
  }
  
  ###### ACTIVATE THE SHINY ALERT POPUP
  observeEvent(input$Format, {
    showModal(modalDialog(easyClose = TRUE, 
                footer = tagList(modalButton("Cancel"),
                downloadButton("template", "Template", class = "btn-success"),
                downloadButton("sample", "Sample file", class = "btn-success")),
                title = "Requirements", "Please download template and fill the data accordingly"))
  })
  
  ######  TEMPLATE DOWNLOAD HANDLER
  data <- read_csv("template.csv")
  output$template <- downloadHandler(
    filename = function() {paste("temp.csv", sep = "")},
    content = function(file){write.csv(data, file, na = "", row.names = FALSE)}
  )
  
  output$sample <- downloadHandler(
    filename = function() {paste("sample_file.csv", sep = "")},
    content = function(file){
      data <- read_csv("sample_file.csv")
      write.csv(data, file, row.names = FALSE)
    }
  )

  ###### DOWNLOAD OPTION 1 FOR CSV
  output$button1 <- renderUI({
    req(input$getData)
    downloadButton("download", "Treatment", class = "btn-info")
  })
  
  ###### DOWNLOAD OPTION 2 FOR EXCEL
  output$button2 <- renderUI({
    req(input$getData)
    downloadButton("download2", "NA Treatment", class = "btn-info")
  })
  
  ##### INPUT OPTION FOR LOCATION, A SELECTBOX
  output$selectLocation <- renderUI({
    selectInput("selectedLocation", "Select your location", 
                choices = c("Ibadan", "Ubiaja", "Others"), selected = "Ibadan")
  })
  
  ###### INPUT OPTIONS FOR UNAVAILABLE SELECTED LOCATION
  output$locationInput <- renderUI({
    req(input$selectedLocation)
    if (!input$selectedLocation == 'Others') return() else {
      textInput("enterLocation", "Please enter your location below")
    }
  })
  
  ####### SUBMIT BUTTON TO GET YOUR DATA
  output$submit <- renderUI({
    req(input$selectedLocation)
    if((input$selectedLocation == 'Others' && input$enterLocation != "" && input$userName != "") 
       || (input$selectedLocation != "Others" && input$userName != "")){
      actionButton("getData", "Treatments", class = "btn-primary", style = "width:100px")
    } else {
      return(NULL)
    }
  })
  output$submit2 <- renderUI({
    req(input$selectedLocation)
    if((input$selectedLocation == 'Others' && input$enterLocation != "" && input$userName != "") 
       || (input$selectedLocation != "Others" && input$userName != "")){
      actionButton("getData2", "NA Treatments", class = "btn-primary", style = "width:150px")
    } else {
      return(NULL)
    }
  })
  
  
  ##### MAKE YOUR FILE INPUT DATA REACTIVE
  df <- reactive({
    file <- input$inputFile
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == c("csv", "xlsx", "xls"), "Please your data must be a csv file or an excel file"))
    if(ext == "csv"){
      #### READ CSV FILE
      read.csv(file = file$datapath)
   } else {
      ##### READ EXCEL FILE
      read_excel(file$datapath)
    }
  })

  
 ###### RENDER THE RESULT TEXT ON THE DATA TABLE
  output$resHeader <- renderText({
    req(input$getData)
    "Treatments Result"
  })
  
  output$resHeader <- renderText({
    req(input$getData2)
    "NA treatments"
  })
  ############## REACTIVE ALGORITHM, TO GET THE TREATMENTS, RETURNS A DATA FRAME
  ############## WHICH WILL BE RENDERED IN A TABLE
 
  GET_TREATMENTS <- reactive({
    #### VALIDATE THE WEEKLY DATA
    weekly_data  <- df()
    column_names <- colnames(weekly_data)
    selected_column_names <- c("unique_id", "height", "forked", "damage", 
                               "ml_STS_previous_week", "mM_BA_previous_week")
    valid <- selected_column_names %in% column_names
    as.list(valid)
    if(FALSE %in% valid){
      shinyalert("Requirements", "Sorry we can't find some column names in your data, 
                 \n Please download a template in the user guide and use carefully",
                 type = "error")
      return(NULL)
    }
    ####### PROCEED WITH THE SELECTION IF VALIDATION IS CORRECT
    selections<-select(weekly_data,unique_id,height,forked,damage,ml_STS_previous_week,
                       mM_BA_previous_week) %>% 
      mutate(weekly_data,BA_last_week=if_else(mM_BA_previous_week>0,"y","n")) %>% 
      mutate(weekly_data,STS_last_week=if_else(ml_STS_previous_week>0,"y","n")) %>%
      select(unique_id,treatment,height,forked,damage,BA_last_week,STS_last_week)

      ###### CALCULATE TREATMENTS AFTER SELECTED COLUMNS
    treatments <- selections %>% 
      mutate(start_PGR = if_else(height < 60 & forked == "n", "n",
                                 if_else(height > 45 & forked == "y", "y",
                                         if_else(height > 60, "y", "NA" )))) %>% 
      
      mutate(prune_ok = if_else(BA_last_week=="y","y","n")) %>% 
      
      mutate(base_mL_STS = if_else(height %in% 0:45,"0",
                                   if_else(height %in% 46:60, "1.25",
                                    if_else(height %in% 61:80, "2.5",
                                     if_else(height %in% 81:100, "3.5",
                                     if_else(height %in% 101:120, "4.5",
                                      if_else(height %in% 121:140, "5.5",
                                      if_else(height %in% 141:160, "6.5",
                                      if_else(height %in% 161:180, "7.5",
                                      if_else(height %in% 181:200, "8.5",
                                      "10")))))))))) %>% 
      
      mutate(damage_reduction_factor = if_else(damage == 0, "1",
                                               if_else(damage == 1, "1",
                                                       if_else(damage == 2, "0.5",
                                                               if_else(damage == 3, "0", "NA" ))))) %>% 
      
      mutate(base_mM_BA = if_else(start_PGR == "y", "0.5", 
                                  if_else (start_PGR == "n", "0", "NA" ))) %>% 
      
      mutate(start_PGR_factor = if_else(start_PGR == "y", 1,0)) %>% 
      mutate(STS_last_week_factor = if_else(STS_last_week == "y", 0,1)) %>%
      mutate(treatment_factor = if_else(treatment == "best practice",1,0)) %>% 
      
      type_convert(
        col_types = cols(
          base_mL_STS = col_double(),
          base_mM_BA = col_double(),
          damage_reduction_factor = col_double(),
          treatment_factor = col_double()
        )
      ) %>% 
      mutate(mL_STS_treatment = base_mL_STS * damage_reduction_factor * 
               start_PGR_factor * STS_last_week_factor * treatment_factor) %>% 
      mutate(mM_BA_treatment = base_mM_BA * damage_reduction_factor * start_PGR_factor * treatment_factor)%>% 
      ####added in treatment factor
      
      select(unique_id,treatment,height,BA_last_week,prune_ok,mL_STS_treatment,mM_BA_treatment)
      return(treatments)
      #### check treatments for NA
  })
  
  ########### CHECK NA TREATMENTS
  NA_TREATMENTS <- reactive({
    treatments <- GET_TREATMENTS()
    treatments <- data.frame(treatments)
    NA_treatments <- treatments %>% 
      filter_all(any_vars(is.na(.)))
    #### View(NA_treatments)
    return(NA_treatments)
  })

  ###### RENDER TABLE FOR THE RESULT
  observeEvent(input$getData2,{
    if(validator()) { 
      showModal(modalDialog(title = "Bad Request", 
                                "All fields are required, Please fill accordingly"))
        return(NULL)
    }
    output$finalResult <- DT::renderDataTable({
        DT::datatable(data.frame(NA_TREATMENTS()), 
                      options = list(scrollX = TRUE,
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$('td').css({'border': '1px solid black'});",
                                       "$('th').css({'border': '1px solid black'});",
                                       "}")))
    })
  })
  observeEvent(input$getData,{
    if(validator()) { 
      showModal(modalDialog(title = "Bad Request", 
                                "All fields are required, Please fill accordingly"))
        return(NULL)
    }
    output$finalResult <- DT::renderDataTable({
        DT::datatable(data.frame(GET_TREATMENTS()), 
                      options = list(scrollX = TRUE,
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$('td').css({'border': '1px solid black'});",
                                       "$('th').css({'border': '1px solid black'});",
                                       "}")))
    })   
  })

  ########## WHEN THE CSV DOWNLOAD BUTTON IS CLICKED
  write_to_googlesheet <- reactive({
    INITIAL_DATA <- as_tibble(df())
    FINAL_DATA <- as_tibble(GET_TREATMENTS());
    FINAL_DATA2 <- as_tibble(NA_TREATMENTS())
    username <- input$userName
    userLocation <- input$selectedLocation
    if(validator()) { showModal(modalDialog(title = "Bad Request", 
                                            "All fields are required, please fill accordingly"))
      return(NULL)
    }
    showModal(
      modalDialog(title = "Loading...","Please wait, We are getting your file ready", 
                  easyClose = TRUE, footer = NULL )
    )
    if(userLocation == "Others") userLocation <- input$enterLocation
    xlsx_data_input <- paste0(Sys.time(), "_",username,"_",userLocation,"_PGR_Treatments_input",sep="")
    xlsx_data_output <- paste0(Sys.time(), "_",username,"_",userLocation,"_PGR_Treatments_output",sep="")
    xlsx_data_output_na <- paste0(Sys.time(), "_",username,"_",userLocation,"_PGR_Treatments_output_NA",sep="")
    ss <- gs4_create(name = xlsx_data_input, sheets = list(input = INITIAL_DATA))
    ss2 <- gs4_create(name = xlsx_data_output, sheets = list(output = FINAL_DATA))
    ss3 <- gs4_create(name = xlsx_data_output_na, sheets = list(output = FINAL_DATA2))
    drive_find("Flowering_input")
    drive_mv(file = xlsx_data_input, path = as_id("1upMJHaN8gJ56Kfq0MKkqc0p1EzQYfwHf"))
    drive_mv(file = xlsx_data_output, path = as_id("1qamAZKjyllTNJAqJdYZUx5B3uQArHv6G"))
    drive_mv(file = xlsx_data_output_na, path = as_id("1qamAZKjyllTNJAqJdYZUx5B3uQArHv6G"))
    shinyalert("Success", "Your data has been downloaded", type="success")
    removeModal()
  })

  output$download <- downloadHandler(
    ##### WRITE INITIAL AND FINAL XLSX DATA TO GOOGLESHEET ACCOUNT
    filename = function() {
      username <- input$userName
      userLocation <- input$selectedLocation
      xlsx_data_output <- paste0(Sys.time(), "_",username,"_",userLocation,"_PGR_Treatments_output",sep="")
      paste0(xlsx_data_output,".xls", sep = "")
    },
    content = function(file){
      FINAL_DATA <- as_tibble(GET_TREATMENTS());
      write_to_googlesheet();
      WriteXLS::WriteXLS(FINAL_DATA, ExcelFileName = file, row.names = FALSE)
    }
  )
  
  ####### WHEN THE EXCEL DOWNLOAD BUTTON IS CLICKED
  output$download2 <- downloadHandler(
    filename = function() {
      username <- input$userName
      userLocation <- input$selectedLocation
      xlsx_data_output_na <- paste0(Sys.time(), "_",username,"_",userLocation,"_PGR_Treatments_output_NA",sep="")
      paste0(xlsx_data_output_na,".xls",sep = "")
    },
    content = function(file){
      FINAL_DATA2 <- as_tibble(NA_TREATMENTS());
      WriteXLS::WriteXLS(FINAL_DATA2, ExcelFileName = file, row.names = FALSE)
    }
  )
}
# Run the application 
shinyApp(ui = ui, server = server)


###### OUTLINED STEPS
###### RENDER TWO DOWNLOAD BUTTONS, XLS DOWNLOAD BUTTONS
###### ONE DOWNLOAD BUTTON WORKS FOR TOTAL TREATMENTS AND THE OTHER FOR NA TREATMENTS
###### 