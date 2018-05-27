
server <- function(input, output, session) {
  
  observeEvent(input$go, {
   
    #smb_per_update_modified
    
    output$table <- DT::renderDataTable({
     #sbm_usr
    }, options = list(scrollX = TRUE, pageLength = 5,
      lengthChange = FALSE, rownames = FALSE)
    )
    
  })
  
  
  output$table <- DT::renderDataTable({
   sebms_users()
  }, options = list(scrollX = TRUE, pageLength = 5,
    lengthChange = FALSE, rownames = FALSE))
  
  output$menu2_UI <- renderUI ({
    res <- includeHTML("www/about.html")
  	fluidRow(box(res, width = 12))
  })
  
  output$tab_box <- renderUI({
  	fluidRow(
  		tabBox(
  			title = "",
   			id = "tabset1", height = "100%", width = 12,
 				tabPanel("Table", DT::dataTableOutput("table")),
  			#tabPanel("Map", leafletOutput("birdmap")),
 				tabPanel("About", uiOutput("menu2_UI"))
  		)
  	)
  })

}



