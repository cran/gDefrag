plotgraph <-
function(nodes, edges, land_polyg, scale_nodes = 10, col_nodes = "darkblue", col_labels = "darkblue", cex_labels = 1, main = "Graph"){

  node_T <- nodes@data  # new
  edge_P <- edges@data  # new


  if ("priorization" %in% colnames(edge_P)) { # new
     value_width <- edge_P[, "priorization"]
 value_norm <- (value_width - min(value_width)) / (max(value_width) - min(value_width)) * 10
 }
  else value_norm <- 5  # new

  land_polyg@data <- data.frame(land_polyg@data, nodes@data)

  plot(land_polyg, col="lightblue", main = main)
  plot(edges, lwd=3, col = gray(1-(value_norm/10)), add = TRUE)
  
  symbols(node_T[, "X"], node_T[, "Y"], circles = sqrt(node_T[, "pol_value"] / pi) * scale_nodes, fg = col_nodes, add = TRUE, inches = FALSE)
  text(x = coordinates(nodes), labels = as.character(node_T$node_ID), cex = cex_labels, col = col_labels)
}
