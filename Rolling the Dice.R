library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Rolling the Dice: Unveiling Normal Distributions"),
  
  sidebarLayout(
    sidebarPanel(
      lapply(1:10, function(i) {
        fluidRow(
          column(12, radioButtons(paste0("die", i), paste("Die", i, ":"), choices = 1:6, inline = TRUE))
        )
      }),
      actionButton("computeButton", "Compute Sum")
    ),
    
    mainPanel(
      plotOutput("histogram"),
      textOutput("errorText"),
      textOutput("sumText")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Initialize rolls data
  rolls <- read.csv("dice_rolls.csv")$Sum
  
  # Initialize histogram with existing data or empty data
  output$histogram <- renderPlot({
    if (length(rolls) > 0) {
      hist(rolls, breaks = seq(10, 61, by = 5), main = "Histogram of Dice Rolls", xlab = "Sum of 10 Dice")
    } else {
      hist(NULL, main = "Histogram of Dice Rolls", xlab = "Sum of 10 Dice")
    }
  })
  
  # Function to validate dice values
  validateDice <- function() {
    for (i in 1:10) {
      if (!is.null(input[[paste0("die", i)]])) {
        die_value <- as.numeric(input[[paste0("die", i)]])
        if (!is.na(die_value) && die_value %in% 1:6) {
          # Everything is fine, continue to the next die
        } else {
          return(FALSE)
        }
      } else {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  # Function to compute sum
  computeSum <- function() {
    die_values <- sapply(1:10, function(i) as.numeric(input[[paste0("die", i)]]))
    sum(die_values)
  }
  
  # Generate histogram data
  observeEvent(input$computeButton, {
    if (validateDice()) {
      new_sum <- computeSum()
      rolls <<- c(rolls, new_sum)  # Append new sum to rolls
      
      # Update histogram
      output$histogram <- renderPlot({
        hist(rolls, breaks = seq(10, 61, by = 5), main = "Histogram of Dice Rolls", xlab = "Sum of 10 Dice", col = "red")
        hist(rolls[-length(rolls)], breaks = seq(10, 61, by = 5), add = TRUE)
      })
      
      output$errorText <- renderText("")
      output$sumText <- renderText(paste("Sum of 10 Dice:", new_sum))
      
      # Save results to a CSV file
      saveResults(rolls)
    } else {
      output$errorText <- renderText("Error: Please enter valid values for each die (1 through 6).")
      output$sumText <- renderText("")
    }
  })
  
  # Function to save results to a CSV file
  saveResults <- function(rolls) {
    data <- data.frame(Sum = rolls)
    write.csv(data, "dice_rolls.csv", row.names = FALSE)
  }
}

# Run the application 
shinyApp(ui = ui, server = server)