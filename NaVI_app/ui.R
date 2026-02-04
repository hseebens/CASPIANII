################################################################################
#
# Shiny App NaVI - Funktion 'user interface'
#
# Die Funktion 'user interface' stellt das 'frontend' der Shiny App dar und 
# liefert damit das Layout der Oberflaeche, die fuer den Nutzer zu sehen und zu 
# bedienen ist.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 11.11.25
################################################################################



ui <- fluidPage(
  
  ### Busy bar & allgemeines Styling
  add_busy_bar(centered = TRUE, color = "#FF0000"),
  tags$head(
    tags$style(HTML("
      hr {border-top: 1px solid #000000;}
      #main-title {
        color: darkgreen;
        text-align: left;
        font-size: 32px;
        font-weight: bold;
      }
    ")),
    tags$title("NaVI - Neobiota an Verkehrswegen Informationssystem")
  ),
  

  ### Title
  h1("NaVI - Neobiota an Verkehrswegen Informationssystem", id = "main-title"),
  hr(),
  
  ### Layout #########################
  sidebarLayout(
    
    ## Sidebar panel 
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Landkreis",
          br(),
          
          # select Kreise
          selectizeInput(
            inputId = "Kreise_Daten",
            label = "Landkreis",
            choices = c("Alle Kreise", sort(unique(all_reg_data$RegionName))),
            selected = "Alle Kreise"
          ),
          
          # Download Buttons
          h5("Download Liste vorkommende Neobiota"),
          downloadButton("download_ist", "Download .csv"),
          h5("Download Liste potenzielle Neobiota"),
          downloadButton("download_pot", "Download .csv")
        ),
        
        tabPanel(
          "Artengruppe",
          br(),
          selectizeInput(
            inputId = "Artlisten",
            label = "Management:",
            choices = c("Alle Neobiota","Unionsliste","Aktionsliste","Beobachtungsliste",
                        "Handlungsliste","Managementliste"),
            selected = "Alle Neobiota"
          ),
          
          selectizeInput(
            inputId = "Artengruppe",
            label = "Taxa:",
            choices = c("Alle Taxa", sort(unique(all_reg_data$Artengruppe))),
            selected = "Alle Taxa"
          ),
          
          h5("Download der Arten"),
          downloadButton("download_reg_list", "Download .csv")
        ),
        
        tabPanel(
          "Art",
          br(),
          selectizeInput(
            inputId = "Art_Daten",
            label = "Art auswählen",
            choices = NULL,
            selected = "Keine",
            options = list(
              placeholder = "Artname eingeben…",
              maxOptions = 20
            )
          ),
          
          h5("Download der Vorkommen dieser Art in Deutschland"),
          downloadButton("download_spec", "Download .csv")
        )
      )
    ),
    
    ## Main panel ####################################
    mainPanel(
      tabsetPanel(
        tabPanel("Karte",
                 leafletOutput(outputId = "map"),
                 hr(),
                 h4("Neobiota im Landkreis"),
                 DTOutput(outputId = "ListeNeobiota"),
                 br(),
                 hr(),
                 h4("Neobiota mit hohem Potenzial zur Etablierung im Landkreis"),
                 DTOutput(outputId = "table")
        ),
        
        tabPanel("über NaVI", includeMarkdown("Hintergrunddetails.md"))
      )
    )
  ),
  navbarPage(
    title = "NaVI - Neobiota an Verkehrswegen Informationssystem",
    footer = tags$div(
      style = "text-align:right; font-size:12px;",
      HTML('Made with <a href="https://cran.r-project.org/package=shiny" target="_blank">Shiny</a>')
    ),
  )
)
