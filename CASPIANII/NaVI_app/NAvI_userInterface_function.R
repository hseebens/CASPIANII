################################################################################
#
# Shiny App NaVI - Funktion 'user interface'
#
# Die Funktion 'user interface' stellt das 'frontend' der Shiny App dar und 
# liefert damit das Layout der Oberflaeche, die fuer den Nutzer zu sehen und zu 
# bedienen ist.
#
# Die Shiny App muss über die Funktion ... aufgerufen werden, die wiederum 
# diese Funktion aufruft.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung
################################################################################



ui <- fluidPage(
  
  ### General stuff
  add_busy_bar(centered=TRUE, color = "#FF0000"),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  titlePanel(div(h1("NaVI - Neobiota an Verkehrswegen Informationssystem"),
                 align="left",style="color:darkgreen")),
  
  ### Layout #########################
  sidebarLayout(
    
    ## Sidebar panel 
    sidebarPanel(
      
      h4("Welche Artengruppe?"),
      actionButton("dataNeobiota","Alle Neobiota"),
      actionButton("dataEUIAS","Unionsliste"),
      actionButton("dataBeoL","Beobachtungsliste"),
      actionButton("dataHanL","Handlungsliste"),
      actionButton("dataManL","Managementliste"),
      actionButton("dataAktL","Aktionsliste"),
      hr(),
      
      # h3("Auswahl eines Kreises"),
      h4("Welcher Landkreis?"), #, align="center"
      
      # select Kreise #######################
      selectizeInput(
        inputId = "Kreise_Daten",
        label = "Landkreis",
        choices = c("Keine",uni_regs)
      ),
      
      # download button
      h5("Download Liste Neobiota für Kreise"),
      downloadButton("download_ist", "Download .csv"),
      
      # download button
      h5("Download Liste potenzieller Neobiota für Kreise"),
      downloadButton("download_pot", "Download .csv"),
      hr(),
      
      # select species data ########################
      h4("Welche Art?"), #, align="center"
      selectizeInput(
        inputId = "Art_Daten",
        label = "Wissenschaftlicher Name",
        choices = c("Keine",uni_spec)
      ),
      
      # download button
      h5("Download der Vorkommen dieser Art in Deutschland"),
      downloadButton("download_spec", "Download .csv"),
      
      # adds text
      br(),
      br(),
      p("Made with", a("Shiny", href = "http://shiny.rstudio.com")),
      
    ),
    
    ## Main panel ####################################
    
    mainPanel(
      tabsetPanel(
        tabPanel("Karte",
                 
                 # interactive map
                 leafletOutput(outputId = "map"),
                 
                 # output table 1
                 hr(),
                 h4("Neobiota/IAS im Landkreis"),
                 DTOutput(outputId = "ListeNeobiota"),
                 
                 # output table 2
                 br(),
                 hr(),
                 h4("Neobiota/IAS mit hohem Potenzial zur Etablierung im Landkreis"),
                 DTOutput(outputId = "table")
        ),
        tabPanel("über NaVI", includeHTML("NaVIdetails.html"))
      )
    )
  )
)
