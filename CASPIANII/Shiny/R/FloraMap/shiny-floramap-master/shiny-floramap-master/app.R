#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinycssloaders)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(rgbif)
library(sp)
library(stringr)
library(dplyr)
library(xml2) # fuer read_xml()
library(XML) # fuer xmlInternalTreeParse
library(htmltools)
library(rlist)

# Define UI
ui <- fluidPage(title = "FloraMap - Beobachtungen und Verbreitung",
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  add_busy_bar(centered=TRUE, color = "#FF0000"),
#  use_busy_spinner(spin = "double-bounce", color = "#112446",position = "top-left"),
#  add_busy_spinner(spin="fading-circle",position = "bottom-right", margins = c(200,200)),                
# Application title
  titlePanel(div(h1("FloraMap - Verbreitungsatlas und Beobachtungen"),align="center",style="color:darkgreen")),
# sidebarlayout
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="mytabs",type = "tabs",
        tabPanel(title = "Beobachtungen", value = "obs",
                 br(),
          h4("Namenssuche"),        
          textInput("suchName",label="Wortanfang Gattung Art"),
          actionButton("taxSuche","Trefferliste"),
          br(),br(),
          selectizeInput("artWahl","Trefferauswahl",choices=setNames(80,"Adonis vernalis")),
          hr(),
          h4("Nachweise abfragen"),
          actionButton("distMap","Atlas Verbreitung"),
          actionButton("gbifMap","GBIF Daten"),
          actionButton("afMap", "Artenfinder Daten"),
          hr(),
          h4("Kartenanzeige ein/aus"),
          checkboxInput("cb_florkart", label = "Atlas", value = FALSE),
          checkboxInput("cb_gbif", label = "GBIF", value = FALSE),
          checkboxInput("cb_artenfinder", label = "Artenfinder", value = FALSE)),br()
    ) # tabsetpanel
    ), # sidebarpanel  
    mainPanel(
      tabsetPanel(
        tabPanel("Karte",
          textOutput("atlasrecs"),textOutput("gbifrecs"),textOutput("afrecs"),
          leafletOutput("myMap",height = 600)),
        tabPanel("über FloraMap", includeHTML("floramap-hilfe.html"))
      ))
  ) # sidebarlayout
) # fluidpage

# Define server logic required to draw a map
server <- function(input, output, session) {

#  EPSG_25832 <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:25832",
#             proj4def = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
#             resolutions = 2^(13:-1), # 8192 down to 0.5
#             origin = c(0, 0)
#  )
# initialize map output
  output$myMap <- renderLeaflet(leaflet() %>% 
    setView(lng=10,lat=51.2, zoom = 6) %>% addTiles(group = "OSM") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Sat") %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/pnv500?",
                group="PNV",
                layers = c("Vegetationsgebiete","PNV500"),
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015") %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="NSG",
                layers = "Naturschutzgebiete",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="NP",
                layers = "Nationalparke",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015") %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="FFH",
                layers = "Fauna_Flora_Habitat_Gebiete",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="BSR",
                layers = "Biosphaerenreservate",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/gliederungen?",
                group="RGL",
                layers = "Naturraeume",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addWMSTiles(baseUrl = "http://sg.geodatenzentrum.de/wms_vg250-ew?",
                group="VG250",
                layers = "vg250_krs",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarte: (c) Bundesamt für Kartographie und Geodäsie (BKG) 2014" ) %>%
    addLayersControl(
      baseGroups = c("OSM", "Topo", "ESRI Sat"),
      overlayGroups = c("PNV","NSG","NP","FFH","BSR","RGL","VG250"),
      options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("PNV") %>% hideGroup("NSG") %>% hideGroup("NP") %>% hideGroup("FFH") %>%
      hideGroup("BSR") %>% hideGroup("RGL") %>% hideGroup("VG250") %>%
    addResetMapButton()
    )
# define image urls for wms legends
  legNP <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Nationalparke&'>"
  legNSG <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Naturschutzgebiete&'>"
  legFFH <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Fauna_Flora_Habitat_Gebiete&'>"
  legBSR <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Biosphaerenreservate&'>"
  legRGL <- "<img src='http://geodienste.bfn.de/ogc/wms/gliederungen?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Naturraeume&'>Naturraeume<br/>"
  legVG250 <-"<img src='http://sg.geodatenzentrum.de/wms_vg250-ew?request=GetLegendGraphic&version=1.3.0&format=image/png&width=16&height=16&layer=vg250_krs'>Kreise<br/>"

# map-proxy for changing map-content witout redrawing the complete map
  proxy <- leafletProxy("myMap",session)

# reactive functions 
# search names according genus and species nameparts
  er_Namen <- eventReactive(input$taxSuche,{
    sn <- str_split(input$suchName," ",simplify = TRUE)
    if (nchar(sn[1])<=2)
    {print("Bitte mindesten 3 Gattungsbuchstaben")}
    else
    {
      if (is.na(sn[2])){sn[2]<-""}
      df_namen <- fromJSON(paste0("https://www.floraweb.de/pflanzenarten/taxa_json.xsql?gat=",sn[1],"&art=",sn[2]),simplifyDataFrame = TRUE)
      setNames(df_namen$taxonId,df_namen$latName)
    }
  })
  # retrieve and process data from florkart database (bfn)
  er_Florkart <- eventReactive(input$distMap,{
    df <- fromJSON(paste0("https://www.floraweb.de/pflanzenarten/atlas_json.xsql?suchnr=",input$artWahl,"&grid=quad"),simplifyDataFrame = TRUE)
#    updateTextInput(session,inputId = "taxName",value = df$taxname)
    numPoints <- length(df$records$lat)-1 # json-Ausgabe der records hat immer einen n/a am Ende
    output$atlasrecs <- renderText(paste0("Fertig: ",as.character(numPoints)," Atlas MTB-Quaranten"))
    if (numPoints >= 1){updateCheckboxInput(session,"cb_florkart",value = TRUE)} 
    SpatialPointsDataFrame(cbind(as.double(df$records$lon[1:numPoints]),
                                 as.double(df$records$lat[1:numPoints])),
                           data.frame(zeit=df$records$zeitraum_text[1:numPoints],
                                      rad=2700,
                                      plabel=paste("FloraWeb Atlaspunkt",
                                                   "<em>TK25-Quadrant: </em>",df$records$gridcode[1:numPoints],
                                                   "<br/><em>Nachweiszeitraum: </em>",df$records$zeitraum_text[1:numPoints],
                                                   "<br/><em>Status: </em>",df$records$status[1:numPoints])))
  })
  # retrieve and process data from GBIF
  er_Gbif <- eventReactive(input$gbifMap,{
    df_name <- fromJSON(paste0("https://www.floraweb.de/pflanzenarten/taxnamebyid_json.xsql?taxon_id=",input$artWahl),simplifyDataFrame = TRUE)
    sname <- URLencode(df_name$latName)
    sciname <- URLencode(df_name$sciName)
    gf <- occ_data(scientificName = sname, country="DE", hasCoordinate = TRUE, limit = 10000)$data
    if (length(gf)==0){
      gf <- occ_data(scientificName = sciname, country="DE", hasCoordinate = TRUE, limit = 10000)$data
    }
    if (length(gf)>0){
      gf <- subset(gf, institutionCode != "BfN")
    }
    if (length(gf$decimalLatitude) >= 1) {
      output$gbifrecs <- renderText(paste0("Fertig: ",as.character(length(gf$decimalLatitude))," GBIF-Beobachtungen"))
      updateCheckboxInput(session,"cb_gbif", value = TRUE)
      if("occurrenceID" %in% colnames(gf) && "verbatimLocality" %in% colnames(gf)){
      SpatialPointsDataFrame(cbind(as.double(gf$decimalLongitude),as.double(gf$decimalLatitude)),
             data.frame(gLabel=ifelse(is.na(gf$occurrenceID),
                        paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                               '<br/><em>Fundort: </em>',paste0(gf$locality,"/",gf$verbatimLocality),
                               '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                               '<br/><em>Datum: </em>',gf$month,"/",gf$year),
                        paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                               '<br/><em>Fundort: </em>',paste0(gf$locality,"/",gf$verbatimLocality),
                               '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                               '<br/><em>Datum: </em>',gf$month,"/",gf$year,
                               '<br/><a href="',gf$occurrenceID,'" target="_blank">publizierter Nachweis</a>'))))
      } else if ("occurrenceID" %in% colnames(gf)){
        SpatialPointsDataFrame(cbind(as.double(gf$decimalLongitude),as.double(gf$decimalLatitude)),
              data.frame(gLabel=ifelse(is.na(gf$occurrenceID),
                         paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                                '<br/><em>Fundort: </em>',gf$locality,
                                '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                                '<br/><em>Datum: </em>',gf$month,"/",gf$year),
                         paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                                '<br/><em>Fundort: </em>',gf$locality,
                                '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                                '<br/><em>Datum: </em>',gf$month,"/",gf$year,
                                '<br/><a href="',gf$occurrenceID,'" target="_blank">publizierter Nachweis</a>'))))
      } else {
        SpatialPointsDataFrame(cbind(as.double(gf$decimalLongitude),as.double(gf$decimalLatitude)),
              data.frame(gLabel=paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                                       '<br/><em>Fundort: </em>',gf$locality,
                                       '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                                       '<br/><em>Datum: </em>',gf$month,"/",gf$year)))
              }
    }
  })
# retrieve and process data from Artenfinder
  er_Artenfinder <- eventReactive(input$afMap,{
    df_name <- fromJSON(paste0("https://www.floraweb.de/pflanzenarten/taxnamebyid_json.xsql?taxon_id=",input$artWahl),simplifyDataFrame = TRUE)
    sname <- URLencode(df_name$latName)
    af_url <- paste0("https://artenfinder.rlp.de/api/v2/sichtbeobachtungen?format=xml&restrict=id,lat,lon,datum,bemerkung,foto&titel_wissenschaftlich=",sname)
    af_xml <- read_xml(af_url)
    af_xmldoc <- xmlInternalTreeParse(af_xml,encoding = "utf-8")
    af_df <- xmlToDataFrame(nodes = getNodeSet(af_xmldoc,"//xml/result/row"),stringsAsFactors=FALSE)
    output$afrecs <- renderText(paste0("Fertig: ",as.character(length(af_df$lon))," Artenfinder-Beobachtungen"))
    if (length(af_df) > 0){
      updateCheckboxInput(session,"cb_artenfinder", value = TRUE)
      if ("foto" %in% colnames(af_df)){
      af_sp <- SpatialPointsDataFrame(cbind(as.double(af_df$lon),as.double(af_df$lat)),
                                      proj4string = CRS("+init=epsg:25832"),
                                      data.frame(afLabel=ifelse(is.na(af_df$foto),# foto %in% colnames(af_df)
                                                                paste0("<em>Artenfinder Beobachtung<br/>Datum: </em>",af_df$datum,
                                                                       "<br/><em>Bemerkung: </em>",af_df$bemerkung,
                                                                       "<br/>kein Foto"),
                                                                paste0("<em>Artenfinder Beobachtung<br/>Datum: </em>",af_df$datum,
                                                                       "<br/><em>Bemerkung: </em>",af_df$bemerkung,
                                                                       "<br/><a href='",af_df$foto,"' target='_blank'>Fotolink</a>"))))
      }
      else {
        af_sp <- SpatialPointsDataFrame(cbind(as.double(af_df$lon),as.double(af_df$lat)),
                                        proj4string = CRS("+init=epsg:25832"),
                                        data.frame(afLabel=paste0("<em>Artenfinder Beobachtung<br/>Datum: </em>",af_df$datum,
                                                                  "<br/><em>Bemerkung: </em>",af_df$bemerkung)))
      }
      spTransform(af_sp,CRS("+proj=longlat +datum=WGS84 +no_defs"))
    }
  })
# observers
# observe if an url parameter has been provided
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['taxonid']])) {
      url_namen <- fromJSON(paste0("https://floraweb.de/pflanzenarten/taxnamebyid_json.xsql?taxon_id=",query[['taxonid']]),simplifyDataFrame = TRUE)
      updateTextInput(session, "suchName", value = url_namen$latName)
      updateSelectizeInput(session, "artwahl", choices = setNames(url_namen$taxonId,url_namen$latName))
    }
  })
# observing checkboxes for map overlay groups
  observe({
    proxy %>% clearControls()
    # adding legend controls when group is selected
    if (any(input$myMap_groups %in% "NP")){
      proxy %>% addControl(html = legNP, position = "bottomright")}
    if (any(input$myMap_groups %in% "NSG")){
      proxy %>% addControl(html = legNSG, position = "bottomright")} 
    if (any(input$myMap_groups %in% "FFH")){
      proxy %>% addControl(html = legFFH, position = "bottomright")} 
    if (any(input$myMap_groups %in% "BSR")){
      proxy %>% addControl(html = legBSR, position = "bottomright")}
    if (any(input$myMap_groups %in% "RGL")){
      proxy %>% addControl(html = legRGL, position = "bottomright")}  
    if (any(input$myMap_groups %in% "VG250")){
      proxy %>% addControl(html = legVG250, position = "bottomright")}  
# adding dynamic distribution data overlays
    if (input$cb_florkart)
    {proxy %>% showGroup("FlorKart")
      pal <- colorFactor(palette = c("yellow", "green","red"), domain = c("vor 1950","1950-1979","ab 1980"))
      proxy %>% addLegend(position = "bottomleft",pal=pal, values = c("vor 1950","1950-1979","ab 1980"),
                title = "Nachweiszeitraum", group = "FlorKart", layerId = "legFlorkart")
    }
    else {proxy %>% hideGroup("FlorKart") %>% removeControl("legFlorkart")}
    if (input$cb_gbif)
    {proxy %>% showGroup("GBIF")}
    else {proxy %>% hideGroup("GBIF")}
    if (input$cb_artenfinder)
    {proxy %>% showGroup("AF")}
    else {proxy %>% hideGroup("AF")}
  })
# event observer for selectize box: clear distribution overlays, when selected species changes
  observeEvent(input$artWahl,{
    output$atlasrecs <- renderText("Atlas Quadranten...")
    output$gbifrecs <- renderText("GBIF Beobachtungen...")
    output$afrecs <- renderText("Artenfinder Beobachtungen...")
    proxy %>% clearControls()
    proxy %>% clearGroup("AF") %>% clearGroup("GBIF") %>% clearGroup("FlorKart")
    updateCheckboxInput(session,"cb_florkart",value = FALSE)
    updateCheckboxInput(session,"cb_gbif", value = FALSE)
    updateCheckboxInput(session,"cb_artenfinder", value = FALSE)
  })
# event observers for buttonclick reactive functions
# fill selectize box with new hits from namesearch and clear distribution overlays
  observeEvent(er_Namen(),{
    updateSelectizeInput(session,"artWahl",choices = er_Namen())
    updateCheckboxInput(session,"cb_florkart",value = FALSE)
    updateCheckboxInput(session,"cb_gbif",value = FALSE)
    updateCheckboxInput(session,"cb_artenfinder",value = FALSE)
    proxy %>% clearGroup("AF") %>% clearGroup("GBIF") %>% clearGroup("FlorKart")
  })
# show Florkart distribution data
  observeEvent(er_Florkart(), {
# Achtung: die domain-Werte müssen mit den möglichen Werten in df$records$zeitraum_text übereinstimmen, 
# die domain-Liste wird alphabetisch sortiert und in dieser Reihenfolge den Palette-Weren zugewiesen    
    pal <- colorFactor(palette = c("yellow", "green","red"), domain = c("vor 1950","1950-1979","ab 1980"))
    proxy %>% addCircles(data=er_Florkart(),group="FlorKart",color=~pal(zeit),stroke = FALSE, fillOpacity = 0.5,
                                           popup=~plabel,radius=~rad) %>%
              addLegend(position = "bottomleft",pal=pal, values = c("vor 1950","1950-1979","ab 1980"),
                        title = "Nachweiszeitraum", group = "FlorKart", layerId = "legFlorkart")
  })
# show GBIF distribution data (takes some minutes...)
  observeEvent(er_Gbif(), {
      proxy %>% addCircleMarkers(data = er_Gbif(),group="GBIF",popup = ~gLabel)
  })
# show Artenfinder distribution data  
  observeEvent(er_Artenfinder(), {
        proxy %>% addCircleMarkers(data = er_Artenfinder(), group = "AF", popup = ~afLabel, color = "purple")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
