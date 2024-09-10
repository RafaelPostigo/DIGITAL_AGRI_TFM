###############################################################################################################################
############################################### SERVER ########################################################################
server <- function(input, output, session) {
  ###############################################################################################################
  ################################## SERVER Menu General Information ############################################
  
  
  # Set default selection in selectinput (1st line) when starting the app
  observe({
    updateSelectInput(session, "var_species", selected = final_dataframe$SpecieII[1])
  })  
  
  
  ####################################### 1º GI Descriptive text ###################################### 
  # Reactive function that filt the information accordin to the selected species
  especie_seleccionada_info <- reactive({
    filtered_data <- final_dataframe[final_dataframe$SpecieII == input$var_species, ]
    return(filtered_data$Description[1])  # Select the first description of the selected species
  }) 
  
  # Mostrar la descripción única de la especie seleccionada
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
  
  # Show CSS styling of text (italics where applicable)
  output$css <- renderUI({
    tags$style(
      HTML(".box-body {word-wrap: break-word;}"),
      scoped = TRUE
    )
  })
  
  ############################################# 2º GI Images ##########################################  
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
  
  
  ######################################### 3º GI Mapa ##################################################  
  
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
      
    } else if (selected_species == "Quercus robur") {
      # For Q_robur, add red polygon () and yellow polygon ()
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Qrobur_shapefile2@coords[,1]), lat = mean(Qrobur_shapefile2@coords[,2])-10, zoom = 3) %>%
        addPolygons(data = Qrobur_shapefile1, fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addCircleMarkers(data = Qrobur_shapefile2, radius = 5, color = "yellow", fillColor = "yellow", fillOpacity = 0.8)
      
    } else if (selected_species == "Quercus suber") {
      # For Q_suber, add red polygon () and yellow polygon ()
      my_leaflet <- my_leaflet %>%
        setView(lng = mean(Qsuber_shapefile2@coords[, 1]), lat = mean(Qsuber_shapefile2@coords[, 2]), zoom = 4) %>%
        addPolygons(data = Qsuber_shapefile1, fillColor = "red", fillOpacity = 0.2, weight = 1) %>%
        addCircleMarkers(data = Qsuber_shapefile2, radius = 5, color = "yellow", fillColor = "yellow", fillOpacity = 0.8)
      
    } else if (selected_species == "Quercus variabilis") {
      # For Q_variabilis, add red polygon () and yellow polygon ()
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
  # Renderizar la tabla interactiva para "Protein Database"
  output$proteinTable <- renderDT({
    # Obtener las columnas seleccionadas por el usuario
    selected_columns <- input$show_vars_protein
    
    # Verificar si "Specie" está entre las columnas seleccionadas
    if ("Specie" %in% selected_columns) {
      # Si "Specie" está presente, aplicar el formato CSS a esa columna
      datatable(
        final_dataframe[, selected_columns, drop = FALSE],
        options = list(
          pageLength = 5,
          search = list(regex = FALSE, smart = FALSE, caseInsensitive = TRUE),# Desactivar la búsqueda con expresiones regulares y hacerla sensible a mayúsculas y minúsculas
          columnDefs = list(list(className = 'dt-center', targets = "_all")) # Centrar el contenido de todas las columnas
        ),
        # Aplicar formato CSS a la columna "Specie" para que aparezca en cursiva
        class = "cell-border stripe",
        extensions = 'Buttons',
        selection = "none" # Deshabilitar la selección de filas
      ) %>%
        formatStyle('Specie', fontStyle = 'italic')
    } else {
      # Si "Specie" no está presente, mostrar la tabla sin aplicar el formato CSS
      datatable(
        final_dataframe[, selected_columns, drop = FALSE],
        options = list(
          pageLength = 5,
          search = list(regex = FALSE, smart = FALSE, caseInsensitive = TRUE), # Desactivar la búsqueda con expresiones regulares y hacerla sensible a mayúsculas y minúsculas
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        class = "cell-border stripe",
        extensions = 'Buttons',
        selection = "none" # Deshabilitar la selección de filas
      )
    }
  })
  
  output$downloadDATABASE <- downloadHandler(
    filename = function() {
      paste("protein_data", ".csv", sep = "")
    },
    content = function(file) {
      selected_columns <- input$show_vars_protein
      # Verificar si hay texto en el cuadro de búsqueda
      if (input$proteinTable_search == "") {
        # Si el cuadro de búsqueda está vacío, descargar todas las filas
        data_to_download <- final_dataframe
      } else {
        # Si hay texto en el cuadro de búsqueda, descargar solo las filas mostradas
        rows_to_download <- input$proteinTable_rows_all
        data_to_download <- final_dataframe[rows_to_download, ]
      }
      # Descargar los datos seleccionados
      write.csv(data_to_download[, selected_columns, drop = FALSE], file, row.names = FALSE)
    }
  )
  #### Server 3º menu PROTEIN SEQUENCE ####
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
  
  # Function to generate the content of the FASTA file
  generate_fasta_content <- function() {
    selected_protein <- input$Protein
    selected_sequence <- final_dataframe$Sequence[final_dataframe$Protein == selected_protein]
    if (!is.null(selected_sequence)) {
      return(paste(">", selected_protein, "\n", selected_sequence, sep = ""))
    } else {
      return("No information found for the selected protein")
    }
  }
  
  #Function to generate the content of the FASTA file for all the sequences of the selected species.
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
  ############################## SERVER PHYLOGENETIC MENU   ##################################################
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
  
  #############################SERVER BLASTP  MENU  #################################################  
  # Output for Blastp Instructions for Use
  output$blast_instructions <- renderText({
    instructions <- "1. Select the species with which you want to compare your protein.
2. Enter your amino acid sequence with the description sequence using the \">\" symbol.
3. To run the Blastp, press the button \"Run Blast\".
4. To visualize the alignments, select the option of your interest in the results table.
5. To download the Blastp results in CSV, press the button \"Download data\". 
6. To download the Subject Sequence, press the button \"Download Subject Sequence\"."
    return(instructions)
  })
  
  
  # Function to execute BLASTP
  run_blast <- eventReactive(input$run_blast, {
    # Path to the FASTA file for consultation
    query_fasta_file <- tempfile(fileext = ".fasta")
    writeLines(input$fasta_sequence, query_fasta_file)
    
    # Construct the path to the subject file based on the selected species.
    subject_fasta_file <- paste0(input$species_selection, ".fasta")
    
    # Normalize the subject file path
    subject_fasta_file <- normalizePath(subject_fasta_file)
    
    # Check if the subject file exists
    if (!file.exists(subject_fasta_file)) {
      return(list(results = NULL, alignments = NULL))  # Return NULL if the file does not exist
    }
    
    # Path to blastp executable
    blastp_executable <- file.path(".", "bin", "blastp.exe")
    
    
    # Execute the blastp command in R
    blast_result <- system(paste(
      shQuote(blastp_executable),
      "-query", shQuote(query_fasta_file),
      "-subject", shQuote(subject_fasta_file),
      "-outfmt 5"  # XML output format
    ), intern = TRUE)
    
    # Parsing the BLAST results in XML format
    blast_xml <- xmlParse(blast_result)
    
    # Function to extract information from the results
    extract_info <- function(node) {
      query_ID <- xpathSApply(node, ".//Iteration_query-def", xmlValue)
      subject_ID <- xpathSApply(node, ".//Hit_def", xmlValue)
      bitscore <- xpathSApply(node, ".//Hsp_bit-score", xmlValue)
      evalue <- xpathSApply(node, ".//Hsp_evalue", xmlValue)
      Hsp_identity <- xpathSApply(node, ".//Hsp_identity", xmlValue)
      Hsp_align <- xpathSApply(node, ".//Hsp_align-len", xmlValue)
      perc_id <- signif((as.numeric(Hsp_identity) / as.numeric(Hsp_align)) * 100, digits = 4)
      
      # To obtain the maximum length of the vectors
      max_length <- max(length(query_ID), length(subject_ID), length(bitscore), length(evalue), length(Hsp_identity), length(Hsp_align), na.rm = TRUE)
      
      # Fill the vectors with NA if necessary
      query_ID <- rep(query_ID, length.out = max_length)
      subject_ID <- rep(subject_ID, length.out = max_length)
      bitscore <- rep(bitscore, length.out = max_length)
      evalue <- rep(evalue, length.out = max_length)
      perc_id <- rep(perc_id, length.out = max_length)
      
      # Get the length of the query sequence
      query_length <- as.numeric(xpathSApply(node, ".//Iteration_query-len", xmlValue))
      
      # Get Query_start and Query_end values
      query_start <- as.numeric(xpathSApply(node, ".//Hsp_query-from", xmlValue))
      query_end <- as.numeric(xpathSApply(node, ".//Hsp_query-to", xmlValue))
      
      # Calculate query cover
      query_cover <- signif(((query_end - query_start + 1) / query_length) * 100, digits = 4)
      
      # Create a column with the name of the selected species
      species_selected <- rep(input$species_selection, length.out = max_length)
      
      data.frame(Query_ID = query_ID, 
                 Subject_ID = subject_ID,
                 Specie_Selected = species_selected,
                 Score = bitscore,
                 Query_Cover = query_cover,
                 E_Value = evalue,
                 Identity = perc_id)
    }
    
    # Extract information from each result
    results <- xpathApply(blast_xml, "//Iteration", extract_info)
    
    # Combine the results in a single data frame
    result_df <- do.call(rbind, results)
    
    # Obtain the list of all alignments
    alignments <- xpathApply(blast_xml, "//Hit_hsps/Hsp", format_alignment)
    
    # Show notification that the search has ended
    shinyalert("Success!", "BLAST complete.", type = "success", animation = TRUE)
    
    list(results = result_df, alignments = unlist(alignments))
  })
  
  # Display the BLASTP results as an interactive table in the user interface.
  output$Tableprotein <- renderDT({
    req(input$run_blast)
    datatable(run_blast()$results, options = list(pageLength = 5), selection = 'single', editable = FALSE)
  })
  
  # Capture the selected row and display the corresponding alignment.
  observeEvent(input$Tableprotein_rows_selected, {
    if (length(input$Tableprotein_rows_selected) > 0) {
      output$alignments <- renderUI({
        req(input$run_blast)
        selected_row <- input$Tableprotein_rows_selected
        alignments <- run_blast()$alignments
        HTML(alignments[selected_row])
      })
    }
  })
  
  # Download BLASTP results as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      "blast_results.csv"
    },
    content = function(file) {
      blast_result <- run_blast()$results
      write.csv(blast_result, file, row.names = FALSE)
    }
  )
  
  output$downloadSequence <- downloadHandler(
    filename = function() {
      selected_subject <- run_blast()$results$Subject_ID[input$Tableprotein_rows_selected]
      paste(selected_subject, ".fasta", sep = "")
    },
    content = function(file) {
      selected_subject <- run_blast()$results$Subject_ID[input$Tableprotein_rows_selected]
      fasta_file <- file.path(paste0(run_blast()$results$Specie_Selected[input$Tableprotein_rows_selected], ".fasta"))
      sequences <- readAAStringSet(fasta_file)
      selected_sequence <- toString(sequences[selected_subject])
      writeLines(paste(">",gsub(" ", "", selected_subject),"\n", selected_sequence), file)
    }
  )
  # Observe the event of clicking on the “Clear” button and delete the TextAreaInput
  observeEvent(input$clear_button, {
    updateTextAreaInput(session, "fasta_sequence", value = "")
  })
  
}

# Launch the shiny app 
shinyApp(ui, server)