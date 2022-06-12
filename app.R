library(shiny)
library(shinydashboard)
library(purrr)
library(shinyWidgets)
library(highcharter)

ui <- shinydashboard::dashboardPage(
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  UI: HEADER ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  shinydashboard::dashboardHeader(),
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  UI: SIDEBAR ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  shinydashboard::dashboardSidebar(sidebarMenu(
    numericInput(
      "ndiag",
      "Number of diagnosis",
      value = 5,
      min = 1
    ),
    menuItem("Anamnesis", tabName = "anamnesis", icon = icon("question")),
    menuItem("Find tests", tabName = "tests", icon = icon("list")),
    menuItem("Diagnosis", tabName = "diagnosis", icon = icon("check"))
  )),
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  UI: PAGE ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  shinydashboard::dashboardBody(tabItems(
    ## .. anamnesis
    tabItem(tabName = "anamnesis",
            fluidRow(column(width = 12)),
            fluidRow(column(
              uiOutput("pretest"),
              width = 12
            ))),
    
    ## .. tests
    tabItem(tabName = "tests",
            fluidRow(column(
              h2("Search the literature"),
              p("...."),
              width = 12
            )),
            fluidRow(column(
              uiOutput("select"),
              width = 12
            ))),
    ## .. diagnosis
    tabItem(tabName = "diagnosis",
            # fluidRow(
            #   DT::dataTableOutput("DT")
            # ),
            fluidRow(column(
              uiOutput("result"),
              width = 12
            )))
  ))
  
)

server <- function(input, output, session) {
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  SERVER: ANAMNESIS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  pretest_input <-
    reactive({
      paste0("pretest", seq_len(input$ndiag))
    })
  disorder_input <-
    reactive({
      paste0("disorder", seq_len(input$ndiag))
    })
  
  output$pretest <- renderUI({
    pmap(list(pretest_input(), disorder_input()),
         ~
           fluidRow(
             column(textInput(
               ..2, "", value = "", width = '100%'
             ),
             width = 6),
             column(
               sliderInput(
                 ..1,
                 "",
                 value = 1,
                 min = 1,
                 max = 100,
                 width = '100%'
               ),
               width = 6
             )
           ))
    
  })
  
  pretest_dis <- reactive({
    pretest <- map_chr(pretest_input(), ~ input[[.x]] %||% "")
    names <- map_chr(disorder_input(), ~ input[[.x]] %||% "")
    
    return(list("pretest" = pretest, "names" = names))
  })
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  SERVER: SELECT TESTS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sn_input <-
    reactive({
      paste0("sn", seq_len(input$ndiag))
    })
  
  sp_input <-
    reactive({
      paste0("sp", seq_len(input$ndiag))
    })
  
  test_input <-
    reactive({
      paste0("test", seq_len(input$ndiag))
    })
  
  result_input <-
    reactive({
      paste0("result", seq_len(input$ndiag))
    })
  
  output$select <- renderUI({
    pmap(
      list(pretest_dis()$names, sn_input(), sp_input(), test_input(), result_input()),
      ~ box(
        solidHeader = TRUE,
        status = "warning",
        title = ..1,
        textInput(..4, "Selected test:"),
        br(),
        sliderInput(..2, "Sensitivity:", 1, 100, 50),
        sliderInput(..3, "Specificity:", 1, 100, 50),
        radioGroupButtons(
          inputId = ..5,
          label = "Test result:", 
          choices = c(`<i class='fa fa-plus'></i>` = "+", 
                      `<i class='fa fa-question'></i>` = "?", 
                      `<i class='fa fa-minus'></i>` = "-"),
          justified = TRUE
        )
      )
    )
    
  })
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  SERVER: DIAGNOSIS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  test_dis <- reactive({
    sn <- map_chr(sn_input(), ~ input[[.x]] %||% "")
    sp <- map_chr(sp_input(), ~ input[[.x]] %||% "")
    names <- map_chr(test_input(), ~ input[[.x]] %||% "")
    result <- map_chr(result_input(), ~ input[[.x]] %||% "")
    
    return(list(
      "sn" = sn,
      "sp" = sp,
      "names" = names,
      "result" = result
    ))
  })
  
  ## .. calculate post-test probability
  post_test <-
    reactive({
      ## pre_test to post_test
      pre_prob <- as.numeric(pretest_dis()$pretest) / 100
      pre_odds <- pre_prob / (1 - pre_prob)
      sn <- as.numeric(test_dis()$sn) / 100
      sp <- as.numeric(test_dis()$sp) / 100
      lr_neg <- (1 - sn) / sp
      lr_pos <- sn / (1 - sp)
      post_odds_pos <- pre_odds * lr_pos
      post_odds_neg <- pre_odds * lr_neg
      post_prob_pos <- post_odds_pos / (1 + post_odds_pos)
      post_prob_neg <- post_odds_neg / (1 + post_odds_neg)
      
      ## calculate post_prob
      post_prob <- c()
      post_prob[test_dis()$result == "+"] <- post_prob_pos[test_dis()$result == "+"]
      post_prob[test_dis()$result == "-"] <- post_prob_neg[test_dis()$result == "-"]
      post_prob[test_dis()$result == "?"] <- pre_prob[test_dis()$result == "?"]
      
      tibble::tibble(
        pre_prob = pre_prob,
        sn = sn,
        sp = sp,
        post_prob_pos = post_prob_pos,
        post_prob_neg = post_prob_neg,
        result = test_dis()$result,
        post_prob = post_prob
      )
    })
  
  output$result <- renderUI({
    pmap(
      list(
        post_test()$pre_prob, 
        post_test()$post_prob,
        pretest_dis()$names,
        post_test()$result,
        test_dis()$names
        ),
      ~ box(
        solidHeader = TRUE,
        status = "warning",
        title = paste0(..3, " | ", ..5),
        p(paste0("The test result was: ", ..4)),
        create_bar_hc(c("Pre-test", "Post-test"), c(..1, ..2))
      )
    )
  })
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  HELPER FUNCTIONS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## create chart
  create_bar_hc <- function(names, values){
    tibble::tibble(names = names, values = values) %>%
      hchart('column', hcaes(x = names, y = values*100), name = 'Probability') %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_colors(c("black", "black")) %>%
      hc_yAxis(title = list(text = "Probability"),
               labels = list(format = "{value}%"),
               min = 0,
               max = 100,
               plotBands = list(
                 list(
                   from = 80,
                   to = 100,
                   color = hex_to_rgba("green", 0.1),
                   label = list(text = "Present", rotation = -90, align = "left"),
                   # the zIndex is used to put the label text over the grid lines
                   zIndex = 1
                 ),
                 list(
                   from = 20,
                   to = 80,
                   color = hex_to_rgba("orange", 0.1),
                   label = list(text = "Undecided", rotation = -90),
                   # the zIndex is used to put the label text over the grid lines
                   zIndex = 1
                 ),
                 list(
                   from = 0,
                   to = 20,
                   color = hex_to_rgba("red", 0.1),
                   label = list(text = "Absent", rotation = -90),
                   # the zIndex is used to put the label text over the grid lines
                   zIndex = 1
                 )
               )) %>%
      hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
      hc_plotOptions(series = list(colorByPoint = TRUE))
  }
  
  
  
  
  
  output$DT <- DT::renderDT({post_test()})
  # observe({
  #   t <- map_chr(pretest_input(), ~ input[[.x]] %||% "")
  #   u <- map_chr(disorder_input(), ~ input[[.x]] %||% "")
  #   print(t)
  #   print(u)
  # })
  
  # observe({
  #   print(test_dis()$sn)
  #   print(test_dis()$sp)
  #   print(test_dis()$names)
  # })
  
  
}

shinyApp(ui, server)