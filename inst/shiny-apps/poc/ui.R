library(leaflet)
library(DT)
library(shinydashboard)
library(shinyjs)

dbHeader <- dashboardHeader(
	titleWidth = 450,
	title = "Swedish Butterflies", disable = FALSE,
	tags$li(a(href = 'http://www.dagfjarilar.lu.se/om-oss',
			img(src = 'logo.png', height = 30, width = 30),
			title = "Svensk Dagfjärilsövervakning",
			style = "padding-top:10px; padding-bottom:10px;"),
		class = "dropdown")
)


dashBody <- dashboardBody(
	tags$head(
	tags$style(type = "text/css", "#mapbox { height: 80vh !important; }"),
	tags$style(type = "text/css", "#birdmap { height: 75vh !important; }")),
	uiOutput("tab_box")
)

dashboardPage(skin = "green",
	dbHeader,
	dashboardSidebar(width = 450,
		uiOutput("lang"),
		hr(),
		flowLayout(
			uiOutput("species"),
			uiOutput("country")
		),
		hr(),
		flowLayout(
			uiOutput("months"),
			uiOutput("years")
		)
	),
	dashBody
)

function(request) {

	dashBody <- dashboardBody(
		tags$head(
		tags$style(type = "text/css", "#mapbox { height: 80vh !important; }"),
		tags$style(type = "text/css", "#birdmap { height: 75vh !important; }")),
		useShinyjs(),
		uiOutput("tab_box")
	)

# ui <- fluidPage(
#   textInput("ID", "Enter person ID:", "1"),
#   actionButton(inputId = "go", 
#                label = "Update"),
#   tableOutput("tbl")
#   
# )	
	
	dashboardPage(skin = "green",
		dbHeader,
		dashboardSidebar(width = 450,
			uiOutput("lang"),
			hr(),
			flowLayout(
				uiOutput("species"),
				uiOutput("country")
			),
			hr(),
			flowLayout(
				uiOutput("months"),
				uiOutput("years")
			)
		),
		dashBody
	)
}
