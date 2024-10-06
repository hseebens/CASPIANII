
library(shiny)
library(data.table)
library(shinybusy)


all_pot_spec <- readRDS(file.path("..","WorkingDocs","WP4","List_all_pot_spec.rds"))

# all_pot_spec[["Hausen"]]


coords <- fread(file.path("..","WorkingDocs","WP4","Neobiota_AllOccurrences.gz"))
uni_spec <- unique(coords$Taxon)

ui <- fluidPage(
  
  add_busy_bar(centered=TRUE, color = "#FF0000"),
  
  titlePanel(div(h1("Informationen zur Verbreitung von Neobiota"),align="center",style="color:darkgreen")),
  
  selectizeInput(
    inputId="Gemeinde_Daten",
    label="Gemeindenamen", 
    choices=names(all_pot_spec)),
  # tableOutput("preview"),
  downloadButton("download", "Download .csv"),
  hr(),
  
  selectizeInput(
    inputId="Art_Daten",
    label="Artnamen",
    choices=uni_spec),
  # tableOutput("preview"),
  downloadButton("download_spec", "Download .csv")
)

server <- function(input, output, session) {
  
  data_regs <- reactive({
    all_pot_spec[[input$Gemeinde_Daten]]
  })
  
  output$preview <- renderTable({
    head(data_regs())
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Gemeinde_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_regs(), file)
    }
  )
  
  data_spec <- reactive({
    subset(coords, Taxon==input$Art_Daten)
  })
  
  output$preview <- renderTable({
    head(data_spec())
  })
  
  output$download_spec <- downloadHandler(
    filename = function() {
      paste0(input$Art_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_spec(), file)
    }
  )
  
}

shinyApp(ui, server)
