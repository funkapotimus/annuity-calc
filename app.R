# ------------------------------------------------------------------------------
#
#			  Annuity Calculator by Juan Trejo
#
# This app relies on the lifecontingencies package avaialable here:
#
#	https://github.com/spedygiorgio/lifecontingencies

#
# There 
#
# Load packages ----------------------------------------------------------------
library(shiny)
library(openintro)
library(lifecontingencies)


ui <- pageWithSidebar(

  # Title ----
  headerPanel("Annuity Calculator"),

  # Sidebar ----
  sidebarPanel(
		# helpText("annuity(i, n,m=0,k=1, type=immediate)"),
		uiOutput("annuityn"),
		uiOutput("annuityi"),
		uiOutput("annuitym"),
		# uiOutput("annuityk"),
		uiOutput("annuitytype"),
		# uiOutput("annuitytypeInc"),
		checkboxInput("checkbox", "Increasing Annuity (P,Q)", value = FALSE),
		htmlOutput("NumericInputUI"),
		htmlOutput("NumericInputUI2")
		
  ),

  mainPanel(
    div(textOutput("annuitycalculation"), align = "center", style = "font-size:200%;")
  )
)

		
		
server <- function(input, output) {


	# annuity=function(i, n,m=0,k=1, type="immediate")
	output$annuityi	 <- renderUI({
	# print("mean")
	  sliderInput("annuityinterest",
		h4("Interest (i)"),
		value = .03,
		min = 0,
		max = 1,
		step = 0.001
	  )
	})	
	
	output$annuityn	 <- renderUI({
	# print("mean")
	  # sliderInput("annuitytermn",
		# "Term (n)",
		# value = 25,
		# min = 0,
		# max = 100,
	  # )
	  numericInput("annuitytermn",  min = 0, label = h4("Term (n)"), value = 25)
	})
	
	output$annuitym	 <- renderUI({
	# print("mean")
	  # sliderInput("annuitytermm",
		# "Deferment Period (m)",
		# value = 1,
		# min = 0,
		# max = 20,
	  # )
	  numericInput("annuitytermm", min = 0, label = h4("Deferment Period (m)"), value = 0)
	})
	
	# output$annuityk	 <- renderUI({
	# print("mean")
	  # sliderInput("annuityfrequency",
		# "Payment Frequency (k)",
		# value = 1,
		# min = 0,
		# max = 1,
	  # )
	# })	
	
	output$annuitytype	 <- renderUI({
	# print("mean")
	   selectInput(
        inputId = "annuitytypeinput",
        label = "Choose annuity type:",
        choices = c(
          "Immediate" = "immediate",
          "Due" = "due"
        ),
        selected = "immediate"
      )
	})
	
	output$annuitytypeInc <- renderUI({
	# print("mean")
	   selectInput(
        inputId = "annuitytypeinput",
        label = "Choose annuity type:",
        choices = c(
          "Increasing" = "increasing",
          "Decreasing" = "decreasing"
        ),
        selected = "decreasing"
      )
	})

	increasingAnnuityTwo=function(i, n,type="immediate")
	{
		out=NULL
		if(missing(n)) stop("Error! Need periods")
		if(missing(i)) stop("Error! Need interest rate")
		# type <- testpaymentarg(type)
		#here we shud check if this value is correct
		out=((input$num1)*annuity(i, n, type="immediate") + (input$num2/i)*(annuity(i, n, type="immediate")- n*(1+i)^-n))*((1+i)^input$annuitytermm)
		return(out)
		
	}
	output$NumericInputUI <- renderUI({
		if (input$checkbox == TRUE) {
			numericInput("num1", label = h3("P first payment"), value = 1)
			# output$value <- renderPrint({ input$num })
		}
	})
	
	output$NumericInputUI2 <- renderUI({
		if (input$checkbox == TRUE) {
			numericInput("num2", label = h3("P+Q second payment"), value = 1)
			# output$value <- renderPrint({ input$num })
		}
	})
	
	
	
	output$annuitycalculation <- renderText({
		# annuityinterest
		if (input$checkbox == TRUE) {
			y <- increasingAnnuityTwo(input$annuityinterest, input$annuitytermn,type="immediate")
			y
		}
		else {
			x <- annuity(input$annuityinterest, input$annuitytermn, input$annuitytermm, 1,input$annuitytypeinput)
			x
		}
		
	})
 
}

# Create the Shiny app object --------------------------------------------------
shinyApp(ui = ui, server = server)
