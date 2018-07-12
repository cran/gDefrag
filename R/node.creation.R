node.creation <-
function(land_polyg, value_col, plot = TRUE, scale_nodes = 1, col_nodes = "deepskyblue4", cex_labels = 1, shape = FALSE, shape_name_nodes = "shape_nodes"){


  if (!is.projected(land_polyg))  stop ("'land_polyg' must be in a projected coordinate system.")
   
  message("Creating nodes...")
  node_ID <- 1:length(land_polyg)  
  land_polyg@data <- data.frame(land_polyg@data, node_ID = node_ID)

  centroids <- coordinates(gPointOnSurface(land_polyg, byid = TRUE, id = node_ID))  

  nodes_T <- data.frame(node_ID,
                        centroids,
                        land_polyg@data[ , value_col],
                        gArea(land_polyg, byid = TRUE))  
  colnames(nodes_T) <- c("node_ID", "X", "Y", "pol_value", "pol_area")
  
  
  if (plot) {
    plot(land_polyg)
    symbols(nodes_T[ , "X"], nodes_T[ , "Y"], circles = sqrt(nodes_T[ , "pol_value"] / pi) * scale_nodes, fg = col_nodes, add = TRUE, inches = FALSE)
    text(x = centroids, labels = as.character(node_ID), cex = cex_labels)
  }

  nodes <- SpatialPointsDataFrame(coords = nodes_T[ , c("X", "Y")], data = nodes_T)  # new
  nodes@proj4string@projargs <- land_polyg@proj4string@projargs  # new
  
  if (shape == TRUE){
  suppressWarnings(writeOGR(nodes, ".", shape_name_nodes, driver = "ESRI Shapefile", overwrite_layer = TRUE))
  message("Shapefile created! Check the working directory, please.")
  }

  message("Done!")
  return(nodes)
}
