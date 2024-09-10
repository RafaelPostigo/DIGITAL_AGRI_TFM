# Load required packages
library(DT) ##For interative datatable 
library(shiny) ## Package to create an app 
library(shinydashboard) ## Package that allow divide the interface in sections 
library(openxlsx)
library(shinyjs)
library(rgdal) #necesario para el mapa
library(leaflet) #Package to add maps 
library(XML)        ## Package for work with blast outmf 5
library(shinyalert) ## Package for blastp end
library(Biostrings) ## Package to read the aminoacid sequences 
library(ggplot2) ##Graphics display package
library(grid)
library(png) ##Package that allows to display image


final_dataframe <- read.csv("final_subida.csv")
filo <- file.path("atodos.PNG")
filo2 <- file.path("aCastanea.PNG")

################################### PHYLOGENETIC TREE MENU  #############################################
####
# Crear la imagen con ggplot2 y ajustar el tamaño
gg_image <- function(file_path, ranges) {
  img <- readPNG(file_path)
  g <- rasterGrob(img, interpolate = TRUE)
  img_width <- ncol(img)
  img_height <- nrow(img)
  
  ggplot() +
    annotation_custom(g, xmin = 0, xmax = img_width, ymin = 0, ymax = img_height) +
    coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, img_width)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, img_height)) +
    theme_void()  # Quitar ejes y fondo
}

##########################  MENU BLASTP  ####################################
###############################  Formatting the alignment  ############################################
format_alignment <- function(alignment) {
  query_seq <- xmlValue(getNodeSet(alignment, "./Hsp_qseq")[1])
  midline <- xmlValue(getNodeSet(alignment, "./Hsp_midline")[1])
  subject_seq <- xmlValue(getNodeSet(alignment, "./Hsp_hseq")[1])
  
  # Define maximum width for displaying characters per line
  max_width <- 60
  
  # Function for splitting a sequence into lines of maximum length
  split_sequence <- function(sequence) {
    n <- nchar(sequence)
    num_lines <- ceiling(n / max_width)
    split_seq <- sapply(1:num_lines, function(i) {
      start <- (i - 1) * max_width + 1
      end <- min(i * max_width, n)
      substr(sequence, start, end)
    })
    split_seq
  }
  
  query_lines <- split_sequence(query_seq)
  midline_lines <- split_sequence(midline)
  subject_lines <- split_sequence(subject_seq)
  
  # Get the total length of the query sequence
  query_length <- nchar(query_seq)
  
  # Create text formatted in HTML format
  formatted_text <- "<pre>"
  for (i in 1:length(query_lines)) {
    query_line <- query_lines[i]
    midline_line <- midline_lines[i]
    subject_line <- subject_lines[i]
    
    #Calculate start and end item numbers
    start_pos <- (i - 1) * max_width + 1
    end_pos <- min(i * max_width, query_length)
    
    # Add formatted line to HTML text
    formatted_text <- paste0(formatted_text, 
                             sprintf("Query  %d  %s  %d\nMidln    %s%s\nSbjct  %d  %s  %d\n\n",
                                     start_pos, 
                                     paste(mapply(function(x, y) {
                                       if (x == y) {
                                         x
                                       } else {
                                         paste('<span style="background-color:red">', x, '</span>', sep='')
                                       }
                                     }, strsplit(query_line, '')[[1]], strsplit(subject_line, '')[[1]]), collapse=""),
                                     end_pos,
                                     strrep(" ", nchar(as.character(start_pos))),  # Agregar espacios en función del número de cifras en start_pos
                                     midline_line,
                                     start_pos, 
                                     paste(mapply(function(x, y) {
                                       if (x == y) {
                                         y
                                       } else {
                                         paste('<span style="background-color:red">', y, '</span>', sep='')
                                       }
                                     }, strsplit(query_line, '')[[1]], strsplit(subject_line, '')[[1]]), collapse=""), 
                                     end_pos))
  }
  formatted_text <- paste0(formatted_text, "</pre>")
  
  # Return formatted HTML text
  formatted_text
}



#### Create dataframes of the tree's images ####
image_paths <- list(
  `Castanea crenata` = paste0("Ccrenata_", 1:4, ".jpg"),
  `Castanea dentata`  = paste0("Cdentata", 1:3, ".jpg"),
  `Castanea mollissima` = paste0("Cmollissima", 1:3, ".jpg"),
  `Castanea sativa` = paste0("Csativa", 1:4, ".jpg"),
  `Castanopsis hystrix` = paste0("Chystrix", 1:3, ".jpg"),
  `Quercus dentata` = paste0("Qdentata", 1:3, ".jpg"),
  `Quercus ilex` = paste0("Qilex", 1:3, ".jpg"),
  `Quercus lobata`= paste0("Qlobata", 1:3, ".jpg"),
  `Quercus robur` = paste0("Qrobur", 1:3, ".jpg"),
  `Quercus suber` = paste0("Qsuber", 1:3, ".jpg"),
  `Quercus variabilis` =  paste0("Qvariabilis", 1:4, ".jpg")
)

# Define polygons from SHP file
C.crentata_shapefile <- readOGR(dsn = ".", layer = "Ccrenata")
C.dentata_shapefile <- readOGR(dsn = ".", layer = "Cdent")
csativa_shapefile1 <- readOGR(dsn = ".", layer = "Castanea_sativa_plg_clip")
csativa_shapefile2 <- readOGR(dsn = ".", layer = "Castanea_sativa_syn_plg_clip")
cmoll_shapefile1 <- readOGR(dsn = ".", layer = "Cmolli")
Chystrix_shapefile1 <- readOGR(dsn = ".", layer = "Chystrix")
Qdentata_shapefile1 <- readOGR(dsn = ".", layer = "Qdent")
Qilex_shapefile1 <- readOGR(dsn = ".", layer = "Quercus_ilex_ilex_plg_clip")
Qilex_shapefile2 <- readOGR(dsn = ".", layer = "Quercus_ilex_ilex_syn_plg_clip")
Qilex_shapefile3 <- readOGR(dsn = ".", layer = "Quercus_ilex_2_introducido")
Q_lobata_shapefile <- readOGR(dsn = ".", layer = "Quercus_lobata_disuelto")
Qrobur_shapefile1 <- readOGR(dsn = ".", layer = "Quercus_robur_plg_clip")
Qrobur_shapefile2 <- readOGR(dsn = ".", layer = "Quercus_robur_syn_pnt")
Qsuber_shapefile1 <- readOGR(dsn = ".", layer = "Quercus_suber_plg_clip")
Qsuber_shapefile2 <- readOGR(dsn = ".", layer = "Quercus_suber_syn_pnt")
Qvaria_shapefile <- readOGR(dsn = ".", layer = "Qvari")



#####################################   Define UI   ######################################################
##################################### DashboardPage ######################################################
# Define UI
ui <- dashboardPage(    #Shinyboard package function to make an application divided into Header, Sidebar and Body
  title = "Protein Database of plants belonging to the Fagaceae family", 
  skin = "green",     #Style/color of the dashboard
  
  ##################################### DashboardHeader ####################################################  
  dashboardHeader(      #Create the header                                         
    title = "Menu",      
    dropdownMenu(type = "messages",           #Argument to leave a message in the right area of the header
                 messageItem(from = "Rafael Postigo Luque",     
                             "Welcome to my TFM project"),
                 messageItem(from = "Rafael Postigo Luque",
                             "Next update: BLASTp"))
  ),
  
  
  ##################################### DashboardSidebar ####################################################   
  dashboardSidebar(    #Argumento que crea la barra lateral (con respectivos submenús)
    sidebarMenu(       #Argumento que crea  menú dentro de la barra lateral 
      id = "Sidebar menu",   
      menuItem("Information", icon = shiny::icon("computer"), id = "general", tabName = "Info",
               #Name of that specific menu page; ICON; Identity; Name to link the menu to the server
               menuSubItem("General information", tabName = "GI"),
               menuSubItem("Protein database", tabName = "DB"),
               menuSubItem("Protein information", tabName = "PI")),
      menuItem("Phylogeny tree", icon = shiny::icon("tree"), id = "phylogenyid",
               menuSubItem(HTML("All"), tabName = "treeQ"),
               menuSubItem(HTML("<i>Castanea</i>"), tabName = "treeC")),
      menuItem("Blast", icon = shiny::icon("magnifying-glass"), id = "Bla",
               menuSubItem("Protein blast", tabName = "PB"))
    )
  ),
  ##################################### DashboardBody ####################################################   
  dashboardBody(
    tabItems(       #Function to introduce objet into the menus
      ############################################INTERFAZ DE GENERAL INFORMATION####################################################      
      tabItem(tabName = "GI",
              h2(HTML("<div style='font-size: 35px; text-align: center; text-decoration: underline;'>General information</div>")),
              fluidRow(  #Function to occupy the entire available width 
                div(     #The function creates an HTML container (for italics)
                  style = "padding-left: 19px;",  #19 pix space from left
                  tags$style(HTML("#var_species + .selectize-control .selectize-input, #var_species + .selectize-control .selectize-dropdown-content, #var_species + .selectize-control .selectize-dropdown-content .option:hover {font-style: italic;}")),
                  
                  style = "font-size: 18px;", 
                  selectInput(                #Funcion to create a selection box
                    "var_species",            #ID of the selectInput
                    label = "Choose a specie",#Title of the selection box
                    choices = unique(final_dataframe$SpecieII) #Selection options are set (species column of dataframe final_dataframe i.e. the name of the species)
                  )
                ),
                #Funcion para generar contenido dinamico
                uiOutput("info_box"),   ##Render object info_box --> contains the general info of each species 
                uiOutput("css")         #Render object info_box --> Contains the info_box text stucture
              ),
              fluidRow(
                column(6, 
                       div(
                         style = " width: 100%; position: relative; margin-left:10px; margin-top: 20px;", 
                         # Witdh 100%; original position ; margin space left 10 pix ; right 20 pix
                         imageOutput("species_image"), #
                         actionButton("change_image_btn", NULL, icon = icon("arrow-right"), 
                                      
                                      ##Button that when pressed will trigger the event that changes the images.
                                      ##ID of the button, NUL: button has no text and no content; Icon of the button
                                      
                                      style = "position: absolute; top: 36%; margin-left: 462px;")
                         #Button position
                       )
                ),
                column(6,
                       div(
                         style = "width: 115%; position: relative; margin-top: 20px; margin-left: -120px;",  
                         box(width = NULL,
                             solidHeader = TRUE,  #Header of the box is solid
                             title = "Habitat Map",
                             leafletOutput("species_map", width = "100%", height = "400px")
                         )
                       )
                )
              )),
      
      ### 2º submenu Protein database ####
      tabItem(tabName = "DB",
              h2(HTML("<div style='font-size: 35px; text-align: center; margin-bottom: 30px; text-decoration: underline;'>Characterization HSP90 sequences</div>")),
              fluidRow(
                column(6, offset = 4,style = "padding-left: 40px;",
                       box(
                         title = "Options",
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 7,
                         checkboxGroupInput("show_vars_protein", "Columns in Protein Database to show:",
                                            choices = c("Genus","Specie","Chr","Gene", "Protein","Annotation", "AA_lenght","Conserved_domain"),
                                            selected = c("Genus","Specie","Chr","Gene", "Protein","Annotation", "AA_lenght","Conserved_domain"))
                       )
                ),
                fluidPage(
                  DTOutput("proteinTable"),
                  downloadButton('downloadDATABASE', 'Download Data')
                )
              )
      ),
      #### 3º submenu Protein sequence #####
      tabItem(tabName = "PI",
              h2(HTML("<div style='font-size: 35px; text-align: center; margin-bottom: 30px; text-decoration: underline;'>Protein Database of plants belonging to the Fagaceae familiy </div>")),
              sidebarLayout(
                sidebarPanel(
                  div(
                    tags$style(HTML("#Species + .selectize-control .selectize-input, #Species + .selectize-control .selectize-dropdown-content, #Species + .selectize-control .selectize-dropdown-content .option:hover {font-style: italic;}")),
                    selectInput("Species", 
                                label = "Choose a species", 
                                choices = unique(final_dataframe$Specie))
                  ),
                  uiOutput("protein_selection")
                  
                ),
                mainPanel(
                  box(
                    title = "Protein sequence",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    uiOutput("output_sequence")
                    
                  ),
                  div(style = "margin-left: 16px;", 
                      downloadButton('downloadamino', 'Download sequence', class = "btn btn-default"),
                      uiOutput("downloadall_button"))
                )
              )
      ),
      
      ## 4º submenu ####
      tabItem(tabName = "treeQ",
              fluidRow(
                column(width = 12, align = "center",
                       h2("Phylogenetic tree"),  # Title of the page
                       div(style = "margin-top: 20px; text-align: center;",
                           div(style = "display: inline-block;",   #  Centered container
                               plotOutput("gg_image_output", height = "500px", width = "600px",  # Ajustar la altura y el ancho aquí
                                          dblclick = "plot1_dblclick",
                                          brush = brushOpts(
                                            id = "plot1_brush",
                                            resetOnNew = TRUE
                                          ))
                           )
                       ),
                       uiOutput("image_caption"),  # Image caption
                       uiOutput("inforr_box"),       # Additional information
                )
              )
      ),
      ## 5º submenu ####
      tabItem(tabName = "treeC",
              fluidRow(
                column(width = 12, align = "center",
                       h2(tags$i("Castanea Phylogenetic tree")),  # Title of the page with the word Castanea in italics
                       div(style = "margin-top: 20px; text-align: center;",
                           div(style = "display: inline-block;",  # Centered container
                               plotOutput("gg_image_output_castanea", height = "500px", width = "600px",  # Ajustar la altura y el ancho aquí
                                          dblclick = "plot2_dblclick",
                                          brush = brushOpts(
                                            id = "plot2_brush",
                                            resetOnNew = TRUE
                                          ))
                           )
                       ),
                       uiOutput("image_caption_castanea"),   # Image caption
                       uiOutput("info_box_castanea"),       # Additional information
                )
              )
      ),
      
      ## 6º submenu ####
      tabItem(tabName = "PB",
              h2(HTML("<div style='font-size: 35px; text-align: center; text-decoration: underline;'>Protein Blast</div>")),       
              fluidRow(
                div(     #Function creates an HTML container (for italics)
                  tags$style(HTML("#species_selection + .selectize-control .selectize-input, #species_selection + .selectize-control .selectize-dropdown-content, #species_selection + .selectize-control .selectize-dropdown-content .option:hover {font-style: italic;}")),
                  column(4,
                         selectInput("species_selection", label = "Select Species", choices = c("Castanea crenata", "Castanea dentata", 
                                                                                                "Castanea mollissima N11", "Castanea mollissima vanuxem",
                                                                                                "Castanopsis hystrix", "Quercus dentata", 
                                                                                                "Quercus lobata", "Quercus robur", 
                                                                                                "Quercus suber", "Quercus variabilis")),
                         
                         textAreaInput("fasta_sequence",
                                       label = "Enter FASTA sequence",
                                       placeholder = ">Sequence name\nSequence",
                                       rows = 5
                         ),
                         actionButton("run_blast", "Run Blast"),  # Button to start the Blastp 
                         actionButton("clear_button", "Clear"),
                         
                  ),
                  column(6, 
                         tags$div(
                           id = "instructions",
                           style = "width: 110%; padding: 10px;",
                           h3("Instructions for use:"),
                           verbatimTextOutput("blast_instructions")
                         )
                  )
                )),
              tags$div(
                id = "blastResults",
                style = "padding: 10px;",
                DTOutput("Tableprotein"),
                downloadButton('downloadData', 'Download Data'),
                h3("Alignment"), 
                htmlOutput("alignments"),
                downloadButton('downloadSequence', 'Download Subject Sequence')
              )
      )
    )
  )
)