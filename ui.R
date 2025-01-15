# set wd first
# runApp()


library(shiny)

fluidPage(
  titlePanel("Interactive Nomogram for CVD Identification"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Adjust Predictor Variables"),
      selectInput("sex", "Sex", choices = c("Male" = "Male", "Female" = "Female"), selected = "Male"),
      sliderInput("age", "Age", min = 20, max = 90, value = 50, step = 1),
      selectInput("Nicotine", "Smoking Status", 
                  choices = c("Never" = "Never", "Current" = "Current", "Former" = "Former"), selected = "Never"),
      selectInput("Cholesterol", "Cholesterol Status", 
                  choices = c("No" = "No", "Yes" = "Yes"), selected = "No"),
      selectInput("DM", "Diabetes Mellitus", 
                  choices = c("No" = "No", "Yes" = "Yes"), selected = "No"),
      selectInput("BPQ050A", "Hypertension Treatment", 
                  choices = c("No" = "No", "Yes" = "Yes"), selected = "No"),
      selectInput("ACES_with_complexity_combined", "ACES Classification",
                  choices = c("Other" = "Other", "Stage III" = "Stage III", 
                              "Stage IV" = "Stage IV", "Edentulous" = "Edentulous"), selected = "None"),
      actionButton("calculate", "Calculate Likelihood")
    ),
    
    mainPanel(
      h4("Nomogram Plot"),
      plotOutput("nomogramPlot"),
      h4("Predicted Probability of CVD"),
      textOutput("predictedProbability")
    )
  )
)
