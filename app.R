library(shiny)
library(shinydashboard)
library(purrr)
library(shinyWidgets)
library(highcharter)
source("https://raw.githubusercontent.com/achekroud/nomogrammer/master/nomogrammer.r")
source("www/pages/info_page.R")
library(fresh)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                  THEME ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    dark_bg = "#434C5E",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440", 
    light_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF", 
    info_box_bg = "#FFF"
  )
)

header <- dashboardHeader()
anchor <- tags$a(href='https://reva.ugent.be',
                 tags$img(src='ugent.png', height='30', width='40'),
                 'Hitchhiker Dx')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: white }"))),
  anchor,
  class = 'name')

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                  UI ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ui <- shinydashboard::dashboardPage(
  skin = "black",
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  UI: HEADER ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  header,
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  UI: SIDEBAR ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      menuItem("Info", tabName = "info", icon = icon("info"))
    ),
    fluidRow(h4(column("Hitchhiker guide", width = 12, offset = 1))),
    sidebarMenu(
    numericInput(
      "ndiag",
      "# dx in differential diagnosis",
      value = 2,
      min = 1
    ),
    menuItem("1-Anamnesis", tabName = "anamnesis", icon = icon("question")),
    menuItem("2-Find tests", tabName = "tests", icon = icon("fingerprint")),
    menuItem("3-Diagnosis", tabName = "diagnosis", icon = icon("check"))
    ),
    fluidRow(h4(column("Extra functions", width = 12, offset = 1))),
    sidebarMenu(
    menuItem("Nomogram", tabName = "nomogram")
    #div(img(src = "ugent.png", height="20%", width="20%"), style="text-align: center;")
  )),
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  UI: PAGE ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  shinydashboard::dashboardBody(
    # use_theme(mytheme),
    tabItems(
    ## .. info page
    info_page,
    ## .. anamnesis
    tabItem(tabName = "anamnesis",
            fluidRow(
              column(
                h2("Step 1 - Anamnesis"),
                HTML("<p>Please fill in the different diagnoses you are considering,
                     and assign a pre-test probability based on the symptomatology
                     of the patient. The number of diagnoses can be changed in the
                     sidebar. </p>"),
                width = 12
                )
              ),
            fluidRow(column(
              uiOutput("pretest"),
              width = 12
            ))),
    
    ## .. tests
    tabItem(tabName = "tests",
            fluidRow(column(
              h2("Step 2 - Select tests"),
              HTML("After the anamnesis, it is important to select the most approriate tests
                   in the literature, and extract the most approriate diagnostic accuracy measures,
                   such as the sensitivity and specificity. Once you have identified the most
                   approriate test for each of the disorders, you can fill out the sensitivity
                   and specificity in each of the boxes. To calculate the final probability
                   of each tested disorder, you will also need to fill out the final test
                   result, which could be inconclusive ('?'), positive ('+') or negative ('-')"),
              width = 12
            )),
            fluidRow(
              column(
                h4("PubMed"),
                p("PubMed is a free search engine accessing primarily the MEDLINE database of 
                  references and abstracts on life sciences and biomedical topics."),
                actionButton("pubmed", 
                             label = "Search Pubmed",  
                             onclick ="window.open('https://pubmed.ncbi.nlm.nih.gov/', '_blank')"),
                width = 4
              ),
              column(
                h4("Web of Science"),
                p("The Web of Science (WoS) is a paid-access 
                  platform that provides (typically via the internet) access to multiple databases 
                  that provide reference and citation data from academic journals."),
                actionButton("wos", 
                             label = "Search Web of Science",
                             onclick ="window.open('https://www.webofknowledge.com/', '_blank')"),
                width = 4
              ),
              column(
                h4("Google Scholar"),
                p("Google Scholar is a freely accessible web search engine that 
                  indexes the full text or metadata of scholarly literature across an 
                  array of publishing formats and disciplines."),
                actionButton("google", 
                             label = "Search Google Scholar",
                             onclick ="window.open('https://scholar.google.com/', '_blank')"),
                width = 4
              )
            ),
            fluidRow(column(
              br(),
              uiOutput("select"),
              width = 12
            ))),
    ## .. diagnosis
    tabItem(tabName = "diagnosis",
            fluidRow(
              column(
                h2("Step 3 - Final diagnosis"),
                HTML("The final step is to decide on the most likely diagnosis based
                     on the pre-test probability based on the anamnesis, and the
                     test result. Combing both will yield a post-test probability
                     for each of the diagnoses under evaluation."),
                width = 12
              )
            ),
            fluidRow(column(
              uiOutput("result"),
              width = 12
            ))),
    ## .. nomogram
    tabItem(tabName = "nomogram",
            fluidRow(
              column(
                sliderInput(inputId = "se", label = "Sensitivity", value = 50, min = 1, max = 100),
                sliderInput(inputId = "sp", label = "Specificity", value = 50, min = 1, max = 100),
                sliderInput(inputId = "prob", label = "Pre-test probability", value = 50, min = 1, max = 100),
                width = 12
              ),
              column(
                plotOutput("nomogram"),
                width = 12
              )))
  ))
  
)

server <- function(input, output, session) {
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##                  SERVER: ANAMNESIS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  n_input <- 
    reactive({
      paste0(seq_len(input$ndiag))
    })
  
  pretest_input <-
    reactive({
      paste0("pretest", seq_len(input$ndiag))
    })
  
  disorder_input <-
    reactive({
      paste0("disorder", seq_len(input$ndiag))
    })
  
  output$pretest <- renderUI({
    pmap(list(pretest_input(), disorder_input(), n_input()),
         ~
           fluidRow(
             column(
               textInput(
                 inputId = ..2, 
                 label = paste0("Disoder ", ..3), 
                 value = "", 
                 width = '100%'
             ),
             width = 6),
             column(
               sliderInput(
                 inputId = ..1, 
                 label = paste0("Pre-test probability ", ..3), 
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
  ##                  SERVER: DIAGNOSIS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$nomogram <- renderPlot({
    req(input$prob)
    req(input$se)
    req(input$sp)
    nomogrammer(Prevalence = input$prob/100, 
                Sens = input$se/100, 
                Spec = input$sp/100, 
                NullLine = TRUE)
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