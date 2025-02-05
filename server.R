library(shiny)

function(input, output, session) {
  output$nomogramPlot <- renderPlot({
    par(mar = c(2, 2, 1, 1))
    plot(
      nom,
      xfrac = 0.1,
      cex.var = 0.7,
      cex.axis = 0.7,
      lineheight = 1,
      main = "Nomogram for Predicting CVD"
    )
  })
  
  observeEvent(input$calculate, {
    new_data <- data.frame(
      sex = factor(input$sex, levels = c("Male", "Female")),
      age = input$age,
      Nicotine = factor(input$Nicotine, levels = c("Never", "Current", "Former")),
      Cholesterol = factor(input$Cholesterol, levels = c("No", "Yes")),
      BPQ050A = factor(input$BPQ050A, levels = c("No", "Yes")),
      ACES_with_complexity_combined = factor(input$ACES_with_complexity_combined, 
                                             levels = c("Other", "Stage III", "Stage IV", "Edentulous"))
    )
    
    risk <- predict(nom_model, new_data, type = "fitted")
    
    output$predictedProbability <- renderText({
      paste("Predicted Probability of CVD:", round(risk * 100, 2), "%")
    })
  })
}
