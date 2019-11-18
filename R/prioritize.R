prioritize <-
function(nodes, edges, method, shape = FALSE, shape_name_out = "priorities_shape", shape_name_nodes_edges = "nodes_with_edges"){


  if (!(method %in% c("value", "between", "IIC", "AWM"))) stop("Invalid 'method'.")

  node_T <- slot(nodes, "data")  
  edge_T <- slot(edges, "data") 
  
  if (method == "value"){
    area_sum <- edge_T[, "value_A"] + edge_T[, "value_B"]
    tab1 <- cbind(as.numeric(rownames(edge_T)), area_sum)
    colnames(tab1) <- c("Edge_ID", "Total_Area")
    tab1 <- as.data.frame(tab1)
    tab1 <- tab1[order(tab1[,2]), ]
    t2 <- cbind(tab1[,1],1:nrow(tab1))
    t2 <- t2[ order(t2[,1]), ][,2]
    t2 <- (t2-min(t2))/(max(t2)-min(t2))*100
    result <- cbind(edge_T, t2)
    result <- as.data.frame(result)
  }

  if (method == "between"){
    nd <- node_T  
    mygraph <- graph_from_data_frame(edge_T, directed = FALSE, vertices = nd)
    out_C <- edge.betweenness.estimate(mygraph, e = E(mygraph), directed = FALSE, cutoff = 0)
    out_C <- (out_C-min(out_C))/(max(out_C)-min(out_C))*100
    result <- cbind(edge_T, out_C)
    result <- as.data.frame(result)
  }

  if (method == "IIC"){

    metric <- function(n, e){

      nd <- n
      mygraph <- graph_from_data_frame(e, directed=FALSE, vertices=nd)
      dist_tp <- as.data.frame(distances(mygraph))
      topo_col <- rep(NA, nrow(e))
      for(i in 1:nrow(e)){
        row1 <- e[i,]
        nodeA <- as.numeric(row1[1])
        nodeB <- as.numeric(row1[2])
        dist_tp1 <- dist_tp[as.character(nodeA),as.character(nodeB)]
        topo_col[i] <- dist_tp1
      }
      newT <- cbind(e,topo_col)
      comp1 <- sum((newT[,"value_A"]*newT[,"value_B"])/(1+newT[,ncol(newT)]))
      return(comp1)
    }

    dI <- rep(NA, nrow(edge_T))
	
    I1 <- metric(n=node_T, e=edge_T)

    for(i in 1:nrow(edge_T)){
      edge_T2 <- edge_T[-i,]
      I2 <- metric(n=node_T, e=edge_T2)
      val1 <- ((I1-I2)/I1)*100
      dI[i] <- val1
    }

    dI <- (dI-min(dI))/(max(dI)-min(dI))*100

    result <- cbind(edge_T, dI)
    result <- as.data.frame(result)

  }

  if (method == "AWM"){

	merge.with.order <- function(x,y, ..., sort = TRUE, keep_order)
	{
		add.id.column.to.data <- function(DATA)
		{
			data.frame(DATA, id... = seq_len(nrow(DATA)))
		}
		order.by.id...and.remove.it <- function(DATA)
		{
			if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
 
			ss_r <- order(DATA$id...)
			ss_c <- colnames(DATA) != "id..."
			DATA[ss_r, ss_c]
		}
 

		if(!missing(keep_order))
		{
			if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
			if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
			warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
		} else {return(merge(x=x,y=y,..., sort = sort))}
	}
 
  nodes_areas <- node_T[,c(1,5)]
  nodes_A_prop <- edge_T[,c(1,8)]
  nodes_B_prop <- edge_T[,c(2,9)]
  ndA <- merge.with.order(x=nodes_A_prop, y=nodes_areas, by.x="node_A", 
  by.y="node_ID", keep_order=1)
  ndB <- merge.with.order(x=nodes_B_prop, y=nodes_areas, by.x="node_B", 
  by.y="node_ID", keep_order=1)
  table1 <- cbind(ndA,ndB)
  metric <- (table1[,2]*table1[,6])+(table1[,5]*table1[,3])
  hab_prop <- (metric-min(metric))/(max(metric)-min(metric))*100
  result <- cbind(edge_T, hab_prop)
  result <- as.data.frame(result)
  }

  colnames(result)[ncol(result)] <- "priorization"

  slot(edges, "data") <- data.frame(slot(edges, "data"), priorization = result[ , "priorization"]) 
    
  nodes_w_edges_ID <- unique(c(slot(edges, "data")[,1], slot(edges, "data")[,2]))
  nodes2 <- nodes
  nodes2 <- nodes2[nodes2$node_ID %in% nodes_w_edges_ID,]
    

  if (shape == TRUE){  
  suppressWarnings(writeOGR(edges, ".", shape_name_out, driver="ESRI Shapefile",overwrite_layer=TRUE))
  suppressWarnings(writeOGR(nodes2, ".", shape_name_nodes_edges, driver="ESRI Shapefile",overwrite_layer=TRUE))
  message("Two shapefiles created! Check the working directory, please.")

 }
  
  return(edges)
}