library(shinythemes)
library(shiny)
library(leaflet.extras)
library(leaflet)
library(sp)
library(rgdal)

greenLeafIcon <- makeIcon( 
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 1, iconHeight = 1)


ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> Choose state</font></center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("stateInput", label = h3("State"),
                                        choices = c("Choose state",
                                                    "Andaman and Nicobar Islands",
                                                    "Andhra Pradesh",
                                                    "Arunachal Pradesh",
                                                    "Assam",
                                                    "Bihar",
                                                    "Chandigarh",
                                                    "Chhattisgarh",
                                                    "Dadra and Nagar Haveli",
                                                    "Daman and Diu",
                                                    "Goa",
                                                    "Gujarat",
                                                    "Haryana",
                                                    "Himachal Pradesh",
                                                    "Jammu and Kashmir",
                                                    "Jharkhand",
                                                    "Karnataka",
                                                    "Kerala",
                                                    "Madhya Pradesh",
                                                    "Maharashtra"
                                        ),
                                        selected = "Choose state")),
                          mainPanel(leafletOutput(outputId = 'mapp', height = 
                                                    800) 
                          ))
))


server <- shinyServer(function(input, output, session) 
{
  updateSelectizeInput(session, "stateInput", choices = map$State,server = TRUE)
  selectedState <- reactive(map[map$State == input$stateInput])
  
  output$mapp <- renderLeaflet(
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
      setView(lng = 79.088860, lat = 21.146633, zoom = 5) %>%
      addTiles(options = tileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>% 
      addCircleMarkers(data= map, lng= ~lon, lat= ~lat, weight = 3, radius=1, 
                       color="#ffa500", stroke = TRUE, fillOpacity = 0.8,label=~Hospital.Name,
                       popup= ~paste("<h3>hospital Details</h3>","<b>Hospital Name:</b>",
                                     Hospital.Name,"<br>","<b>Location:</b>",
                                     Location,
                                     "<br>","<b>Pin:</b>",Pincode,
                                     "<br>","<b>category:</b>",Hospital.Category,
                                     "<br>","<b>State::</b>",State,
                                     "<br>","<b>Telephone:</b>",Telephone))%>%
      addMarkers(data= map, lng= ~lon, lat= ~lat, icon = greenLeafIcon, label=~Hospital.Name,group ='hos')%>%
      addResetMapButton() %>%
      addSearchFeatures(
        targetGroups = 'hos',
        options = searchFeaturesOptions(
          zoom=15, openPopup = TRUE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )))
})

shinyApp(ui = ui, server = server)


a=grepl("eye|Sight|Vision|Eye|Cornea|Retina", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"EyeHospital",map$Hospital.Category)
a=grepl("Nursing", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"Nursinghome",map$Hospital.Category)
a=grepl("Bone", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"Bone and joint hospital",map$Hospital.Category)
a=grepl("Cancer", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"cancer care and research",map$Hospital.Category)
a=grepl("Child|NewBorn", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"Pediatrics hospital",map$Hospital.Category)
a=grepl("Heart|Cardiac|Cardio", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"heart hospital",map$Hospital.Category)
a=grepl("Ortho", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"orthopaedic hospital",map$Hospital.Category)
a=grepl("Plastic", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"plastic surgery",map$Hospital.Category)
a=grepl("Urology", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"urology hospital",map$Hospital.Category)
a=grepl("Neuro|Brain", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"Neurosurgical Hospital",map$Hospital.Category)
a=grepl("Maternity", map$Hospital.Name)
map$Hospital.Category<-ifelse(a,"Maternity Hospital",map$Hospital.Category)