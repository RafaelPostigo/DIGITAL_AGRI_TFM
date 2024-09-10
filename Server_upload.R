############################################### SERVER ########################################################################
server <- function(input, output, session) {
  ###############################################################################################################
  ################################## SERVER Menu General Information ############################################
  
  
  # Set default selection in selectinput (1st line) when starting the app
  observe({
    updateSelectInput(session, "var_species", selected = final_dataframe$SpecieII[1])
  })  
  
  
  ####################################### 1º GI Texto descripTivo ###################################### 
  # Reactive function that filt the information accordin to the selected species 
  especie_seleccionada_info <- reactive({
    filtered_data <- final_dataframe[final_dataframe$SpecieII == input$var_species, ]
    return(filtered_data$Description[1])  # Select the first description of the selected species
  }) 
  
  # Display the unique description of the selected species 
  output$info_box <- renderText({     
    descripcion <- especie_seleccionada_info()
    if (!is.null(descripcion)) {
      paste("<div style='padding: 10px; border: 1px solid #ccc; background-color: #f9f9f9; border-radius: 5px; word-wrap: break-word; margin: 0 20px;'>",
            "<div style='font-size: 17px;'>", descripcion, "</div>",
            "</div>")
    } else {
      "No information available for this species."
    }
  })  
  
  # Mostrar el estilo CSS del texto (cursiva donde corresponda)
  output$css <- renderUI({
    tags$style(
      HTML(".box-body {word-wrap: break-word;}"),
      scoped = TRUE
    )
  })
  
  ############################################# 2º GI Imagenes ##########################################  
  # Store the index of the current image in a reactive value
  current_image_index <- reactiveVal(1)
  
  # Display the images of the specie selected 
  output$species_image <- renderImage({
    list(src = image_paths[[input$var_species]][current_image_index()],
         alt = "Species Image",
         width = 500,
         height = 460)
  }, deleteFile = FALSE)
  
  # Change the image by clicking on the button
  observeEvent(input$change_image_btn, {   # Event is expected to occur
    current_index <- current_image_index()   #image index is shown  
    current_index <- ifelse(current_index == length(image_paths[[input$var_species]]), 1, current_index + 1)
    current_image_index(current_index) #update the index
  })
  # When changing the species in the selectInput, set the image index to 1.
  observeEvent(input$var_species, {
    current_image_index(1)
  })
  
  
  ######################################### 3º GI Map ##################################################  
  
  # Showw the map with the distribution areas of the selected species 
  output$species_map <- renderLeaflet({  #function renderLeaflet creates an interative map
    selected_species <- input$var_species  #input object to select the species by the user
    
    
    # Interative map creation without any coordinate
    my_leaflet <- leaflet() %>%
      addTiles() %>% # Add a base layer to the map 
      setView(lng = mean(C.crentata_shapefile@bbox[1:2])+60, lat = mean(C.dentata_shapefile@bbox[3:4])-50, zoom = 2) 
    
    if (selected_species == "Castanea crenata") {
      # For C.crentata, add red polygon () and yellow polygon ()
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(C.crentata_shapefile@bbox[1:2])+60, lat = mean(C.crentata_shapefile@bbox[3:4])-50, zoom = 2) %>%
        addPolygons(data = C.crentata_shapefile@polygons[[1]], fillColor = "yellow", fillOpacity = 0.2, weight = 1) %>%
        addPolygons(data = C.crentata_shapefile@polygons[[2]], fillColor = "red", fillOpacity = 0.2, weight = 1)
      
    } else if (selected_species == "Castanea dentata") {
      #For C.dentata, add red polygon () and yellow polygon ()
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(C.dentata_shapefile@bbox[1:2])-50, lat = mean(C.dentata_shapefile@bbox[3:4])+50, zoom = 4) %>%
        addPolygons(data = C.dentata_shapefile@polygons[[1]], fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addPolygons(data = C.dentata_shapefile@polygons[[2]], fillColor = "yellow", fillOpacity = 0.2, weight = 1)
      
    } else if (selected_species == "Castanea mollissima") {
      # For C.mollissima, add red polygon () and yellow polygon () 
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(cmoll_shapefile1@bbox[1:4])+80, lat = mean(cmoll_shapefile1@bbox[3:4])-50, zoom = 1) %>%
        addPolygons(data = cmoll_shapefile1@polygons[[1]], fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addPolygons(data = cmoll_shapefile1@polygons[[2]], fillColor = "yellow", fillOpacity = 0.2, weight = 1)
      
    }  else if (selected_species == "Castanea sativa") {
      # For C.sativa, add red polygon () and yellow polygon () 
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(csativa_shapefile1@bbox[1:2]), lat = mean(csativa_shapefile1@bbox[3:4]), zoom = 4) %>%
        addPolygons(data = csativa_shapefile1, fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addPolygons(data = csativa_shapefile2, fillColor = "yellow", fillOpacity = 0.2, weight = 1)
      
    } else if (selected_species == "Castanopsis hystrix") {
      # For C.hystrix, add red polygon () and yellow polygon () 
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Chystrix_shapefile1@bbox[1:2])+60, lat = mean(Chystrix_shapefile1@bbox[3:4])-50, zoom = 3) %>%
        addPolygons(data = Chystrix_shapefile1, fillColor = "red", fillOpacity = 0.2, weight = 1) 
      
    } else if (selected_species == "Quercus dentata") {
      # For C.mollissima, add red polygon () and yellow polygon () 
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Qdentata_shapefile1@bbox[1:4])+40, lat = mean(Qdentata_shapefile1@bbox[3:4])-65, zoom = 2) %>%
        addPolygons(data = Qdentata_shapefile1@polygons[[1]], fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addPolygons(data = Qdentata_shapefile1@polygons[[2]], fillColor = "yellow", fillOpacity = 0.2, weight = 1)
      
    } else if (selected_species == "Quercus lobata") {
      # For Q_lobata, add red polygon () and yellow polygon () 
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Q_lobata_shapefile@bbox[1:4])-80, lat = mean(Qdentata_shapefile1@bbox[3:4])-63, zoom = 5) %>%
        addPolygons(data = Q_lobata_shapefile@polygons[[1]], fillColor = "red", fillOpacity = 0.2, weight = 1)
      
      
      # For Q_ilex, add red polygon () and yellow polygon ()
    } else if (selected_species == "Quercus ilex") {
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Qilex_shapefile1@bbox[1:2]), lat = mean(Qilex_shapefile1@bbox[3:4])+ 7, zoom = 4) %>%
        addPolygons(data = Qilex_shapefile1, fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addPolygons(data = Qilex_shapefile2, fillColor = "yellow", fillOpacity = 0.2, weight = 1) %>%
        addPolygons(data = Qilex_shapefile3, fillColor = "yellow", fillOpacity = 0.2, weight = 1)
      
      # For Q_robur, add red polygon () and yellow polygon ()
    } else if (selected_species == "Quercus robur") {
      # Agregar puntos al mapa
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Qrobur_shapefile2@coords[,1]), lat = mean(Qrobur_shapefile2@coords[,2])-10, zoom = 3) %>%
        addPolygons(data = Qrobur_shapefile1, fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addCircleMarkers(data = Qrobur_shapefile2, radius = 5, color = "yellow", fillColor = "yellow", fillOpacity = 0.8)
      
      
      # For Q_suber, add red polygon () and yellow polygon ()
    } else if (selected_species == "Quercus suber") {
      # Agregar puntos al mapa
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Qsuber_shapefile2@coords[, 1]), lat = mean(Qsuber_shapefile2@coords[, 2]), zoom = 4) %>%
        addPolygons(data = Qsuber_shapefile1, fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addCircleMarkers(data = Qsuber_shapefile2, radius = 5, color = "yellow", fillColor = "yellow", fillOpacity = 0.8)
      
      # For Q_variabilis, add red polygon () and yellow polygon ()
    } else if (selected_species == "Quercus variabilis") {
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Qvaria_shapefile@bbox[1:2]) +65, lat = mean(Qvaria_shapefile@bbox[3:4])- 55, zoom = 3) %>%
        addPolygons(data = Qvaria_shapefile, fillColor = "red", fillOpacity = 0.2, weight = 1) 
      
    }
    #Add a legend showing the col
    my_leaflet <- my_leaflet %>%
      addLegend(position = "bottomright", colors = c("red", "yellow"), labels = c("Native Location", "Introduction"))
    
    my_leaflet
  }) 
  
  ##############################2º SERVER DATABASE  ###################################################
  ############################## Menu database  ###################################################
  # Render the interactive table for “Protein Database”.
  output$proteinTable <- renderDT({
    #Get the columns selected by the user
    selected_columns <- input$show_vars_protein
    
    # Check if “Specie” is among the selected columns
    if ("Specie" %in% selected_columns) {
      # If “Specie” is present, apply CSS formatting to that column
      datatable(
        final_dataframe[, selected_columns, drop = FALSE],
        options = list(
          pageLength = 5,
          search = list(regex = FALSE, smart = FALSE, caseInsensitive = TRUE) # Disable regular expression search and make it case sensitive
        ),
        #  Apply CSS formatting to the “Specie” column to make it appear in italics.
        class = "compact hover",
        extensions = 'Buttons',
        selection = "none" #Disable row selection
      ) %>%
        formatStyle('Specie', fontStyle = 'italic')
    } else {
      # If “Specie” is not present, display the table without applying CSS formatting.
      datatable(
        final_dataframe[, selected_columns, drop = FALSE],
        options = list(
          pageLength = 5,
          search = list(regex = FALSE, smart = FALSE, caseInsensitive = TRUE) # Disable regular expression search and make it case sensitive
        ),
        class = "compact hover",
        extensions = 'Buttons',
        selection = "none" # Disable row selection
      )
    }
  })
  
  output$downloadDATABASE <- downloadHandler(
    filename = function() {
      paste("protein_data", ".csv", sep = "")
    },
    content = function(file) {
      selected_columns <- input$show_vars_protein
      # Check for text in the search box
      if (input$proteinTable_search == "") {
        # If the search box is empty, download all rows
        data_to_download <- final_dataframe
      } else {
        # If there is text in the search box, download only the displayed rows.
        rows_to_download <- input$proteinTable_rows_all
        data_to_download <- final_dataframe[rows_to_download, ]
      }
      # Download selected data
      write.csv(data_to_download[, selected_columns, drop = FALSE], file, row.names = FALSE)
    }
  )
  #### Server 3º PROTEIN SEQUENCE menu ####
  output$protein_selection <- renderUI({
    species <- input$Species
    proteins <- final_dataframe$Protein[final_dataframe$Specie == species]
    selectInput("Protein", 
                label = "Choose a protein", 
                choices = proteins)
  })
  
  output$output_sequence <- renderUI({
    selected_protein <- input$Protein
    selected_sequence <- final_dataframe$Sequence[final_dataframe$Protein == selected_protein]
    
    if (!is.null(selected_sequence)) {
      result <- paste(">", selected_protein, "\n", selected_sequence, sep = "")
      pre(
        style = "font-size: 17px; white-space: pre-wrap;",  # Add white-space for line break
        result
      )
    } else {
      HTML("No information found for the selected protein")
    }
  })
  
  # Función para generar el contenido del archivo FASTA
  generate_fasta_content <- function() {
    selected_protein <- input$Protein
    selected_sequence <- final_dataframe$Sequence[final_dataframe$Protein == selected_protein]
    if (!is.null(selected_sequence)) {
      return(paste(">", selected_protein, "\n", selected_sequence, sep = ""))
    } else {
      return("No information found for the selected protein")
    }
  }
  
  # Function to generate the content of the FASTA file for all the sequences of the selected species.
  generate_all_fasta_content <- function() {
    selected_species <- input$Species
    species_data <- final_dataframe[final_dataframe$Specie == selected_species, ]
    fasta_content <- apply(species_data, 1, function(row) {
      paste(">", row["Protein"], "\n", row["Sequence"], sep = "")
    })
    return(paste(fasta_content, collapse = "\n"))
  }
  
  # Action when pressing the button to download the FASTA file of a sequence
  output$downloadamino <- downloadHandler(
    filename = function() {
      paste("sequence_", input$Protein, ".fasta", sep = "")
    },
    content = function(file) {
      fasta_content <- generate_fasta_content()
      writeLines(fasta_content, file)
    }
  )
  
  # Dynamically generate download button for all sequences of the selected species
  output$downloadall_button <- renderUI({
    species <- input$Species
    if (!is.null(species) && species != "") {
      downloadButton('downloadall', label = HTML(paste('Download all sequences of <i>', species, '</i>')), class = "btn btn-default")
    }
  })
  
  # Action when the button is pressed to download the FASTA file of all sequences of the selected species.
  output$downloadall <- downloadHandler(
    filename = function() {
      paste("all_sequences_", input$Species, ".fasta", sep = "")
    },
    content = function(file) {
      fasta_content <- generate_all_fasta_content()
      writeLines(fasta_content, file)
    }
  )
  ############################## SERVER PHYLOGENETIC MENU    ##################################################
  ranges <- reactiveValues(x = NULL, y = NULL)
  ranges_castanea <- reactiveValues(x = NULL, y = NULL)
  
  output$gg_image_output <- renderPlot({
    gg_image(filo, ranges)
  })
  
  output$gg_image_output_castanea <- renderPlot({
    gg_image(filo2, ranges_castanea)
  })
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observeEvent(input$plot2_dblclick, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges_castanea$x <- c(brush$xmin, brush$xmax)
      ranges_castanea$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_castanea$x <- NULL
      ranges_castanea$y <- NULL
    }
  })
  
  output$image_caption <- renderUI({
    tags$div(style = "text-align: center; width: 600px; margin: 0 auto;",
             tags$i("Cc: Castanea crenata. Cd: Castanea dentata. Cmn-11: Castanea mollisima N-11. Cmv: Castanea mollisima vanuxem. Cs: Castanea sativa. Ch: Castanopisis hystrix. Qd: Quercus dentata. Qi: Quercus ilex. Ql: Quercus lobata. Qr: Quercus robur. Qs: Quercus suber. Qv: Quercus variabilis. Coa: Corylus avellana.")
    )
  })
  
  output$image_caption_castanea <- renderUI({
    tags$div(style = "text-align: center; width: 600px; margin: 0 auto;",
             tags$i("Cc: Castanea crenata. Cd: Castanea dentata. Cmn-11: Castanea mollisima N-11. Cmv: Castanea mollisima vanuxem. Cs: Castanea sativa. Ch: Castanopisis hystrix.")
    )
  })
  
  output$inforr_box <- renderUI({
    tags$div(
      style = "text-align: left; padding: 10px; border: 1px solid #ccc; background-color: #f9f9f9; border-radius: 5px; word-wrap: break-word; margin: 20px 0;",
      tags$div(
        style = "font-size: 17px;",
        "This phylogeny tree has been constructed using the Maximum Likelihood Tree method by generating 1,000 replicates with the Bootstrap method. Additionally, the least reliable nodes were removed from the tree by setting a threshold for bootstrap support values at 50%." )
    )
  })
  
  output$info_box_castanea <- renderUI({
    tags$div(
      style = "text-align: left; padding: 10px; border: 1px solid #ccc; background-color: #f9f9f9; border-radius: 5px; word-wrap: break-word; margin: 20px 0;",
      tags$div(
        style = "font-size: 17px;",
        "This phylogeny tree has been constructed using the Maximum Likelihood Tree method by generating 1,000 replicates with the Bootstrap method. Additionally, the least reliable nodes were removed from the tree by setting a threshold for bootstrap support values at 50%." )
    )
  })
  
  
}

# Launch the shiny app 
shinyApp(ui, server)