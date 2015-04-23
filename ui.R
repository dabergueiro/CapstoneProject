shinyUI(
  pageWithSidebar(
    headerPanel("Capstone Project - Shiny App"),
    sidebarPanel(
      h1("Text to Predict On"),
      textInput(inputId="pred", label = "Enter text:", "For example: I want to"),
      actionButton("predict", "Predict"),
      h3("Notes:"),
      p("It has been noticed during testing that sometimes the first prediction takes longer than the rest. Please reload the page if this happens."),
      p("Average prediction time should be well under 10 seconds. Please refresh the page and try again if it takes longer.")
    ),
    mainPanel(
      h1("Instructions"),
      #code('some code'),
      p("The purpose of this project is to develop a text prediction tool using the text mining packages available in R."),
      p("To use this app, the only thing needed is to enter a text string into the text box in the left panel, and click the Predict button to use the developed algorithm and receive the 5 best options for the next word based on the analyzed text."),
      h3("Predicted next word:"),
      verbatimTextOutput("pred1"),
      h3("Other words that might be suitable:"),
      #h4("Option 2:"),
      verbatimTextOutput("pred2"),
      #h4("Option 3:"),
      verbatimTextOutput("pred3"),
      #h4("Option 4:"),
      verbatimTextOutput("pred4"),
      #h4("Option 5:"),
      verbatimTextOutput("pred5")#,
    )
  )
)