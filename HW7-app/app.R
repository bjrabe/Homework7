library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Define UI for application 
ui <- fluidPage(
  
  # Create a page title and layout ----
  h2("Correlation Exploration"),
  sidebarLayout(
    
    # Add widgets to the sidebar ----
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      
      # Add two select boxes to pick variables for plotting/correlation----
      selectInput(
        inputId = 'corr_x',
        label = 'x Variable',
        choices = numeric_vars,
        selected = 'PINCP',
        selectize = T
      ),
      
      selectInput(
        inputId = 'corr_y',
        label = 'y Variable',
        choices = numeric_vars,
        selected = 'JWMNP',
        selectize = T
      ),
      
      h2('Choose a subset of the data:'),
      
      # Add three radio buttons to choose how to subset the data ----
      radioButtons(
        inputId = 'hhl_corr',
        label = 'Household Language',
        choiceNames = c('All', 'English only', 'Spanish', 'Other'),
        choiceValues = c('all', 'english', 'spanish', 'other'),
        selected = 'all'
      ),
      
      radioButtons(
        inputId = 'fs_corr',
        label = 'SNAP Recipient',
        choiceNames = c('All', 'Yes', 'No'),
        choiceValues = c('all', 'yes', 'no'),
        selected = 'all'
      ),
      
      radioButtons(
        inputId = 'schl_corr',
        label = 'Educational attainment',
        choiceNames = c('All', 'High School not Completed', 'High School or GED', 'College Degree'),
        choiceValues = c('all', 'no_hs', 'hs', 'college'),
        selected = 'all'
      ),
      
      
      h2("Select a Sample Size"),
      
      # Add a slider for selecting the sample size ----
      sliderInput(
        inputId = 'corr_n',
        label = '',
        min = 20,
        max = 500,
        value = 20
      ),
      
      # Add an action button for grabbing a sample ----
      actionButton("corr_sample","Get a Sample!")
    ),
    
    # Add scatter plot output and correlation coefficient guessing game to main panel ----
    # coefficient guessing game is conditional on action button 'corr_sample' having been clicked ----
    mainPanel(
      plotOutput('scatter'),
      conditionalPanel('input.corr_sample',
                       h2("Guess the correlation!"),
                       column(6, 
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1, 
                                           max = 1
                              )
                       ),
                       column(6, 
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)


# Load the data ----
my_sample <- readRDS("my_sample_temp.rds")

# Define server logic required for application
server <- function(input, output, session) {

    
    #################################################
    ##Correlation tab
    #This code makes sure the select boxes update so they can't select the same variable in both!
    #first, update the 'y' selections available
    observeEvent(input$corr_x, {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      if (corr_x != corr_y){
        choices <- choices[-which(choices == corr_x)]
        updateSelectizeInput(session,
                             "corr_y",
                             choices = choices,
                             selected = corr_y)
      }
    })
    #now, update the 'x' selections available
    observeEvent(input$corr_y, {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      if (corr_x != corr_y){
        choices <- choices[-which(choices == corr_y)]
        updateSelectizeInput(session,
                             "corr_x",
                             choices = choices,
                             selected = corr_x)
      }
    })
    



    #Create a reactiveValues() object called sample_corr
    #this object should have two elements, corr_data and corr_truth
    #both should be set to null to start with!
    sample_corr <- reactiveValues(
      corr_data = NULL,
      corr_truth = NULL
    )



    # ##############################################################
    # #Uncomment the next large block of code to go in an
    # #observeEvent() to look for the action button (corr_sample)
 
    
    observeEvent(input$corr_sample, {
      if(input$hhl_corr == "all"){
        hhl_sub <- HHLvals
      } else if(input$hhl_corr == "english"){
        hhl_sub <- HHLvals["1"]
      } else if(input$hhl_corr == "spanish"){
        hhl_sub <- HHLvals["2"]
      } else {
        hhl_sub <- HHLvals[c("0", "3", "4", "5")]
      }

      if(input$fs_corr == "all"){
        fs_sub <- FSvals
      } else if(input$fs_corr == "yes"){
        fs_sub <- FSvals["1"]
      } else {
        fs_sub <- FSvals["2"]
      }

      if(input$schl_corr == "all"){
        schl_sub <- SCHLvals
      } else if(input$schl_corr == "no_hs"){
        schl_sub <- SCHLvals[c("0", "01", "02", "03", "04",
                               "05", "06", "07", "08", "09",
                               "10", "11", "12", "13", "14", "15")]
      } else if(input$schl_corr == "hs"){
        schl_sub <- SCHLvals[as.character(16:19)]
      } else {
        schl_sub <- SCHLvals[as.character(20:24)]
      }

      corr_vars <- c(input$corr_x, input$corr_y)

      subsetted_data <- my_sample |>
        filter(#cat vars first
          HHLfac %in% hhl_sub,
          FSfac %in% fs_sub,
          SCHLfac %in% schl_sub
        ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
        {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
        {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
        {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
        {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
        {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
        {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
        {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
        {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
        {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}

      index <- sample(1:nrow(subsetted_data),
                      size = input$corr_n,
                      replace = TRUE,
                      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
      
      
      sample_corr$corr_data <- subsetted_data[index,]
      sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(corr_vars))[1,2]
      
    })
    
    # Check if sample_corr$corr_data has data, which it should only have if the action button for choosing a sample has been clicked by the user ----
    # If no, then reveal text asking user to select variables, subsets, and action button ----
    # If yes, then create a scatter plot ----
    output$scatter <- renderPlot({
      validate(
        need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
      )
      ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
        geom_point()
    })
    
    

    #This code does the correlation guessing game! Nothing to change here
    observeEvent(input$corr_submit, {
      close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
      if(close){
        shinyalert(title = "Nicely done!",
                   paste0("The sample correlation is ", 
                          round(sample_corr$corr_truth, 4), 
                          "."),
                   type = "success"
        )
      } else {
        if(input$corr_guess > sample_corr$corr_truth){
          shinyalert(title = "Try again!",
                     "Try guessing a lower value.")
        } else {
          shinyalert(title = "Try again!",
                     "Try guessing a higher value.")
        }
      }
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
