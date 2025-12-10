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
  
  ### General stuff
  add_busy_bar(centered=TRUE, color = "#FF0000"),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  titlePanel(div(h1("NaVI - Neobiota an Verkehrswegen Informationssystem"),
                 align="left",style="color:darkgreen")),
  hr(),
  
  ### Layout #########################
  sidebarLayout(
    
    ## Sidebar panel 
    sidebarPanel(
      
      tabsetPanel(
        tabPanel(
          "Landkreis",
          br(),
          
          # select Kreise #######################
          selectizeInput(
            inputId = "Kreise_Daten",
            label = "Landkreis",
            choices = c("Alle Kreise", sort(unique(all_reg_data$RegionName)))
          ),
          # download button
          h5("Download Liste vorkommende Neobiota"),
          downloadButton("download_ist", "Download .csv"),

          # download button
          h5("Download Liste potenzielle Neobiota"),
          downloadButton("download_pot", "Download .csv"),
          
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
          
          # download button
          h5("Download der Arten"),
          downloadButton("download_reg_list", "Download .csv"),
          
        ),
        tabPanel(
          "Art",
          br(),
          selectizeInput(
            inputId = "Art_Daten",
            label = "Wissenschaftlicher Name",
            choices = c("Keine", sort(unique(all_reg_data$Taxon)))
          ),
          
          # download button
          h5("Download der Vorkommen dieser Art in Deutschland"),
          downloadButton("download_spec", "Download .csv"),
          
        ),
      )
    ),
    
    ## Main panel ####################################
    
    mainPanel(
      tabsetPanel(
        tabPanel("Karte",
                 
                 # interactive map
                 leafletOutput(outputId = "map"),
                 
                 # output table 1
                 hr(),
                 h4("Neobiota im Landkreis"),
                 DTOutput(outputId = "ListeNeobiota"),
                 
                 # output table 2
                 br(),
                 hr(),
                 h4("Neobiota mit hohem Potenzial zur Etablierung im Landkreis"),
                 DTOutput(outputId = "table")
        ),
        
        tabPanel("über NaVI", includeMarkdown("Hintergrunddetails.md"))

        # tabPanel("über NaVI", includeMarkdown("details.md"))
        # tabPanel("über NaVI", htmlOutput("about", height = 600, width = 600))
      )
    )
  )
)
