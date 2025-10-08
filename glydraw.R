#' Title Initialize Vertices Coordinate
#'
#' @param structure igraph
#'
#' @returns Initialized Coordinate that Y=0, X are correct
#' @noRd
coor_initialization <- function(structure){
  ver_num <- length(structure)
  coor <- matrix(
    ini_xy <- rep(0,2*ver_num), # 两列值均为0,维度与structure一致的矩阵
    ncol = 2
  )
  colnames(coor) <- c('x','y')
  init_X <- igraph::distances(structure)[length(structure),]*(-1) # 初始位置为length(structure);所有顶点到1号点的最短距离,即横坐标
  coor[,'x'] <- init_X
  for (i in seq(1,length(structure))){
    if (igraph::V(structure)[[i]]$mono == 'Fuc'){
      coor[i,'x'] <- coor[i,'x']+1
    }
  }
  return (coor)
}

#' Title Find Sequence Number of Sub-module
#'
#' @param structure
#' @param ver
#'
#' @returns
#' @export
#'
#' @examples
chil_coor <- function(structure,ver){
  vers <- igraph::bfs(structure, ver, mode='out',unreachable = FALSE)$order
  return(vers)
}

#' Title Offset y-Coordinate of Vertex Sub-module
#'
#' @param structure
#' @param ver
#' @param coor
#' @param offset
#'
#' @returns
#' @export
#'
#' @examples
offset_chil_coor <- function(structure,ver,coor,offset){
  vers <- chil_coor(structure,ver)
  coor[vers,'y'] <- coor[vers,'y']+offset
  return(coor)
}

#' Title Calculate the Amount and Sequence Number of Vertices that Out-degree >= 2 along the Path
#'
#' @param structure
#' @param ver the Sequence Number of Vertex
#'
#' @returns
#' @export
#'
#' @examples out_degree(structure, 1)
out_degree <- function(structure,ver){
  path_vertex <- igraph::shortest_paths(structure,length(structure),ver)$vpath[[1]] # 顶点到起始点的最短路径
  num <- 0
  pos <- c()
  for (i in path_vertex){
    if (length(igraph::neighbors(structure,i)) >= 2 &&
        !('Fuc' %in% igraph::neighbors(structure,i)$mono)){
      num <- num+1
      pos <- c(pos,i)
    }
  }
  return(c(num,pos))
}

#' Title Judge whether the Vertex is in the Middle Position of Sub-module
#'
#' @param structure
#' @param coor
#' @param ver
#' @param par_ver
#'
#' @returns
#' @export
#'
#' @examples mid_pos(structure, coor, 1, 3)
mid_pos <- function(structure,coor,ver,par_ver){
  pos_mid <- FALSE
  vers <- chil_coor(structure,par_ver) # 子模块内顶点的位置
  col_vers <- vers[which(coor[vers,'x']==coor[ver,'x'])] # 子模块内与之同一横坐标的顶点的位置
  ver_y <- coor[ver,'y']
  col_y <- sort(coor[col_vers,'y']) # 纵列上所有点的纵坐标
  if (ver_y != min(col_y) && ver_y != max(col_y)){
    pos_mid <- TRUE
  }
  return(pos_mid)
}

#' Title Judge whether the 'Fucose' Vertex is in the Middle Position of Sub-module
#'
#' @param structure
#' @param coor
#' @param ver
#' @param par_ver
#'
#' @returns
#' @export
#'
#' @examples fuc_mid_pos(structure, coor, 1, 3)
fuc_mid_pos <- function(structure,coor,ver,par_ver){
  vers <- chil_coor(structure,par_ver) # 子模块内顶点的位置
  col_vers <- vers[which(coor[vers,'x']==coor[ver,'x'])] # 子模块内与之同一横坐标的顶点的位置
  ver_y <- coor[ver,'y']
  col_y <- sort(coor[col_vers,'y']) # 子模块中岩藻糖所处纵列上所有点的纵坐标
  ver_index <- which(col_y == ver_y)
  # 判断该点是否与纵列上前后两个点差值小于1
  lag_diff <- abs(col_y - dplyr::lag(col_y)) < 1
  lead_diff <- abs(col_y - dplyr::lead(col_y)) < 1
  result <- lag_diff & lead_diff
  result[is.na(result)] <- FALSE
  return(result[ver_index])
}

#' Title Calculate the Amount and Sequence Number of Vertices that Out-degree >= 2 from Specified 'Fucose' vertex along the Path
#'
#' @param structure
#' @param ver
#' @param coor
#'
#' @returns the Amount and Sequence Number of vertices that Out-degree >= 2
#' @export
#'
#' @examples
fuc_out_degree <- function(structure,ver,coor){
  path_vertex <- igraph::shortest_paths(structure,length(structure),ver)$vpath[[1]] # 顶点到起始点的最短路径
  num <- 0
  pos <- c()
  for (i in path_vertex){
    if (length(igraph::neighbors(structure,i)) %in% c(2,3) &&
        !('Fuc' %in% igraph::neighbors(structure,i)$mono) &&
        fuc_mid_pos(structure,coor,ver,i)){
      num <- num+1
      pos <- c(pos,i)
    }
  }
  return(c(num,pos))
}

#' Title Process the y-Coordinate of 'Fucose' Vertex
#'
#' @param structure
#' @param fuc_pos
#'
#' @returns the Offset of Specified 'Fucose' Vertex
#' @export
#'
#' @examples fuc_offset(structure, 1)
fuc_offset <- function(structure,fuc_pos){
  linkage_str <- igraph::E(structure)[fuc_pos]$linkage
  linkage_pos <- strsplit(linkage_str,'-')[[1]][2]
  offset <- 0.99 # 0.99是为了后续判断岩藻糖是否在中间位置
  if (linkage_pos == '2'){
    offset <- -0.99
  }
  return(offset)
}

#' Title Process the Vertices which Out-degree >=2 along the Path
#'
#' @param coor
#' @param structure
#' @param fuc_pos
#' @param temp_coor
#'
#' @returns Processed Coordinate
#' @export
#'
#' @examples process_fucose_branches(coor, structure, 2, temp_coor)
process_fucose_branches <- function(coor,structure,fuc_pos,temp_coor){
  num <- fuc_out_degree(structure,fuc_pos,temp_coor)[1]
  for (i in seq(1,num)){
    par_pos <- fuc_out_degree(structure,fuc_pos,temp_coor)[i+1]
    neigh_pos <- igraph::neighbors(structure,par_pos)
    if (length(neigh_pos) == 2){
      coor <- offset_chil_coor(structure,neigh_pos[1],coor,1/(2**(num-i+1)))
      coor <- offset_chil_coor(structure,neigh_pos[2],coor,-1/(2**(num-i+1)))
    }
    else if (length(neigh_pos) == 3){
      coor <- offset_chil_coor(structure,neigh_pos[1],coor,1/(2**(num-i+1)))
      coor <- offset_chil_coor(structure,neigh_pos[3],coor,-1/(2**(num-i+1)))
    }
  }
  return(coor)
}

#' Title Process the Vertices which Out-degree >=2 and more than 2 Father Vertices that Out-degree >= 2 along the Path
#'
#' @param coor
#' @param structure
#' @param ver
#'
#' @returns Processed Coordinate
#' @export
#'
#' @examples process_multiple_branches(coor, structure, 3)
process_multiple_branches <- function(coor,structure,ver){
  num <- out_degree(structure,ver)[1] # 路径上出度>1顶点的数量
  # 遍历分支节点到1号顶点路径上所有分支节点
  for(j in seq(1,num-1)){ # 遍历除自身以外的前面所有分支节点
    pos <- out_degree(structure,ver)[j+1] # 路径上出度>1顶点的位置
    pos_max <- out_degree(structure,ver)[num] # 路径上序号第二大的出度>1顶点的位置
    neigh_pos <- igraph::neighbors(structure,pos) # 相邻顶点的位置
    if (length(neigh_pos)==2){
      # 多分支路径上节点偏移满足等比规律,越靠右的分支节点偏移越小,在子模块中间的分支节点还需要加0.25的偏移量
      # 在子模块边缘的分支节点不需要另加0.25的偏移量
      coor <- offset_chil_coor(structure,neigh_pos[1],coor,1/(2**(num-j+1))+0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))
      coor <- offset_chil_coor(structure,neigh_pos[2],coor,-1/(2**(num-j+1))-0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))

    }
    else if(length(neigh_pos)==3){
      # 多分支路径上节点偏移满足等比规律,越靠右的分支节点偏移越小,在子模块中间的分支节点还需要加0.25的偏移量
      coor <- offset_chil_coor(structure,neigh_pos[1],coor,1/(2**(num-j))+0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))
      coor <- offset_chil_coor(structure,neigh_pos[3],coor,-1/(2**(num-j))-0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))
    }
  }
  return(coor)
}

#' Title Process the Vertices which Out-degree =2 along the Path
#'
#' @param coor
#' @param structure
#' @param ver
#'
#' @returns Processed Coordinate
#' @export
#'
#' @examples process_two_neighbors(coor, structure, 5)
process_two_neighbors <- function(coor,structure,ver){
  neigh_pos <- igraph::neighbors(structure,ver) # 相邻顶点的位置
  if (out_degree(structure,ver)[1] == 1){
    coor <- offset_chil_coor(structure,neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,neigh_pos[2],coor,-0.5)
  }
  else if(out_degree(structure,ver)[1] >= 1){ # 顶点到起始点路径上有超过1个分支
    coor <- offset_chil_coor(structure,neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,neigh_pos[2],coor,-0.5)
    coor <- process_multiple_branches(coor,structure,ver)
  }
  return(coor)
}

#' Title Process the Vertices which Out-degree =3 along the Path
#'
#' @param coor
#' @param structure
#' @param ver
#'
#' @returns Processed Coordinate
#' @export
#'
#' @examples process_three_neighbors(coor, structure, 6)
process_three_neighbors <- function(coor,structure,ver){
  neigh_pos <- igraph::neighbors(structure,ver) # 相邻顶点的位置
  if (out_degree(structure,ver)[1] == 1){
    coor <- offset_chil_coor(structure,neigh_pos[1],coor,1)
    coor <- offset_chil_coor(structure,neigh_pos[3],coor,-1)
  }
  else if(out_degree(structure,ver)[1] >= 1){ # 顶点到起始点路径上有超过1个分支
    coor <- offset_chil_coor(structure,neigh_pos[1],coor,1)
    coor <- offset_chil_coor(structure,neigh_pos[3],coor,-1)
    coor <- process_multiple_branches(coor,structure,ver)
  }
  return(coor)
}

#' Title Process the Vertices adjacent to 'Fucose'
#'
#' @param coor
#' @param structure
#' @param ver
#'
#' @returns Processed Coordinate
#' @export
#'
#' @examples process_contain_fucose_neighbors(coor, structure, 7)
process_contain_fucose_neighbors <- function(coor,structure,ver){
  # 获取Fuc的位置
  neigh_pos <- igraph::neighbors(structure,ver)
  fuc_pos <- neigh_pos[which(neigh_pos$mono=='Fuc')]
  # 获取其余邻点的位置
  other_neigh_pos <- neigh_pos[!neigh_pos %in% c(fuc_pos)]
  # 其余邻点按两个邻点的逻辑处理
  if (out_degree(structure,ver)[1] == 1){
    coor <- offset_chil_coor(structure,other_neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,other_neigh_pos[2],coor,-0.5)
  }
  else if(out_degree(structure,ver)[1] >= 1){ # 顶点到起始点路径上有超过1个分支
    coor <- offset_chil_coor(structure,other_neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,other_neigh_pos[2],coor,-0.5)
    coor <- process_multiple_branches(coor,structure,ver)
  }
  return(coor)
}

#' Title Process the Coordinate of all Vertices
#'
#' @param structure
#'
#' @returns Processed Coordinate
#' @export
#'
#' @examples coor_cal(structure)
coor_cal <- function(structure){
  coor <- coor_initialization(structure) # 初始化坐标
  structure_length <- seq(1,length(structure)) # 生成可迭代序列
  # 先处理不含岩藻糖的分支节点
  for (i in structure_length) {
    gly_neighbors <- igraph::neighbors(structure,i)
    if (length(gly_neighbors) == 2 &&
        !('Fuc' %in% gly_neighbors$mono)){
      coor <- process_two_neighbors(coor,structure,i)
    }
    if (length(gly_neighbors) == 3 &&
        !('Fuc' %in% gly_neighbors$mono)){
      coor <- process_three_neighbors(coor,structure,i)
    }
    if (length(gly_neighbors) == 3 &&
        'Fuc' %in% gly_neighbors$mono){
      coor <- process_contain_fucose_neighbors(coor,structure,i)
    }
  }
  # 处理含岩藻糖的分支节点
  fuc_list <- c()
  for (i in structure_length){
    if ('Fuc' %in% igraph::neighbors(structure,i)$mono){
      # 获取Fuc的位置
      neigh_pos <- igraph::neighbors(structure,i)
      fuc_pos <- neigh_pos[which(neigh_pos$mono=='Fuc')]
      fuc_list <- c(fuc_list,fuc_pos)
      # 偏移Fuc节点
      coor[fuc_pos,'y'] <- coor[fuc_pos,'y']+fuc_offset(structure,fuc_pos)
    }
  }
  temp_coor <- coor
  for (i in fuc_list){
    if (fuc_out_degree(structure,i,temp_coor)[1] >=1){
      coor <- process_fucose_branches(coor,structure,i,temp_coor)
    }
  }
  return(coor)
}

#' Title Analysis the Connection Information of all Vertices
#'
#' @param structure
#' @param coor
#'
#' @returns Connection Information
#' @export
#'
#' @examples connect_info(structure, coor)
connect_info <- function(structure,coor){
  start_x <- c()
  end_x <- c()
  start_y <- c()
  end_y <- c()
  structure_length <- seq(1,length(structure))
  for(i in structure_length){
    gly_neighbors <- igraph::neighbors(structure,i)
    neighbors_num <- length(gly_neighbors)
    if (neighbors_num == 1){
      start_x <- append(start_x, coor[i,'x'])
      end_x <- append(end_x, coor[gly_neighbors,'x'])
      start_y <- append(start_y, coor[i,'y'])
      end_y <- append(end_y, coor[gly_neighbors,'y'])
    }
    else if(neighbors_num == 2){
      start_x <- append(start_x, c(coor[i,'x'],coor[i,'x']))
      end_x <- append(end_x, c(coor[gly_neighbors,'x'][1],coor[gly_neighbors,'x'][2]))
      start_y <- append(start_y, c(coor[i,'y'],coor[i,'y']))
      end_y <- append(end_y, c(coor[gly_neighbors,'y'][1],coor[gly_neighbors,'y'][2]))
    }
    else if(neighbors_num == 3){
      start_x <- append(start_x, c(coor[i,'x'],coor[i,'x'],coor[i,'x']))
      end_x <- append(end_x, c(coor[gly_neighbors,'x'][1],coor[gly_neighbors,'x'][2],coor[gly_neighbors,'x'][3]))
      start_y <- append(start_y, c(coor[i,'y'],coor[i,'y'],coor[i,'y']))
      end_y <- append(end_y, c(coor[gly_neighbors,'y'][1],coor[gly_neighbors,'y'][2],coor[gly_neighbors,'y'][3]))
    }
  }
  conn_info <- list(start_x=start_x, start_y=start_y, end_x=end_x, end_y=end_y)
  return(conn_info)
}

# 获取糖型信息用于绘图,主要是Fuc的位置
#' Title
#'
#' @param structure
#'
#' @returns
#' @export
#'
#' @examples
glycoform_info <- function(structure){
  glycoform <- igraph::V(structure)$mono
  for (i in c(which(glycoform == 'Fuc'))){
    if (fuc_offset(structure,i)=='-0.99'){
      glycoform[i] <- 'Fuc_down'
    }
  }
  return(glycoform)
}

# 确定注释字符的坐标
#' Title
#'
#' @param chil_glyx
#' @param chil_glyy
#' @param par_glyx
#' @param par_glyy
#'
#' @returns
#' @export
#'
#' @examples
annotation_coordinate <- function(chil_glyx, chil_glyy, par_glyx, par_glyy){
  chil_direction <- matrix(c(par_glyx-chil_glyx, par_glyy-chil_glyy),ncol = 1, byrow = FALSE)
  par_direction <- matrix(c(chil_glyx-par_glyx, chil_glyy-par_glyy),ncol = 1, byrow = FALSE)
  chil_location <- 0.2*chil_direction/norm(chil_direction, type = '2') # 设定注释字符与顶点的相对距离
  par_location <- 0.2*par_direction/norm(par_direction, type = '2')
  chil_rotate_matrix <- matrix(c(cos(1/9*pi),sin(1/9*pi),
                                 -sin(1/9*pi),cos(1/9*pi)), ncol = 2, byrow = TRUE) # 旋转10度的旋转矩阵
  par_rotate_matrix <- matrix(c(cos(1/9*pi),-sin(1/9*pi),
                                sin(1/9*pi),cos(1/9*pi)), ncol = 2, byrow = TRUE)
  chil_annot_loc <- chil_rotate_matrix %*% chil_location
  par_annot_loc <- par_rotate_matrix %*% par_location
  annot_loc <- list("chil" = chil_annot_loc, "par" = par_annot_loc)
  return(annot_loc)
}

#' Title
#'
#' @param structure
#' @param coor
#'
#' @returns
#' @export
#'
#' @examples
gly_annotation <- function(structure,coor){
  structure_length <- length(structure)
  struc_annot_coor <- data.frame(matrix(nrow = 0, ncol = 4)) # 用于储存各注释坐标
  # 遍历除糖起始点以外的所有顶点
  for (ver in seq(1,structure_length-1)){
    par_ver <- dplyr::nth(as.vector(igraph::shortest_paths(structure,length(structure),ver)$vpath[[1]]),-2) # 相邻母顶点的序号
    # 读取顶点注释信息并计算注释信息与顶点的相对位置
    linkage_str <- igraph::E(structure)[ver]$linkage
    gly_annot_coor <- annotation_coordinate(coor[ver,1],coor[ver,2],coor[par_ver,1],coor[par_ver,2])
    # 得到顶点及母顶点的(annotation,x,y)信息
    chil_annotation <- c(ver, strsplit(linkage_str,'-')[[1]][1])
    chil_annotation <- c(chil_annotation, as.vector(gly_annot_coor$chil)+coor[ver,])
    par_annotation <- c(par_ver,strsplit(linkage_str,'-')[[1]][2])
    par_annotation <- c(par_annotation, as.vector(gly_annot_coor$par)+coor[par_ver,])
    # 合并到dataframe
    struc_annot_coor <- rbind(struc_annot_coor, chil_annotation)
    struc_annot_coor <- rbind(struc_annot_coor, par_annotation)
  }
  colnames(struc_annot_coor) <- c('vertice','annot','x','y')
  struc_annot_coor$annot[struc_annot_coor$annot == 'b1'] <- 'β'
  struc_annot_coor$annot[struc_annot_coor$annot == 'a1'] <- 'α'
  # 转换字符为数值
  struc_annot_coor$x <- as.numeric(struc_annot_coor$x)
  struc_annot_coor$y <- as.numeric(struc_annot_coor$y)
  return(struc_annot_coor)
}

#' Title
#'
#' @param structure
#'
#' @returns
#' @export
#'
#' @examples glydraw(structure)
gly_draw <- function(structure){
  coor <- coor_cal(structure)
  gly_list <- data.frame(coor,'glycoform' = glycoform_info(structure))
  struc_annotation <- gly_annotation(structure,coor)
  # 创建颜色和形状的映射
  color <- c('GlcNAc'= '#3300CC','Man'='#339933','Gal'='#FFCC33','Fuc'='#CC0000','Fuc_down'='#CC0000','GalNAc'='#FFCC33')
  shape <- c('GlcNAc'= 22,'Man'=21,'Gal'=21,'Fuc'=25,'Fuc_down'=24,'GalNAc'=22)

  # 匹配顶点的颜色和形状
  gly_color <- color[as.character(gly_list$glycoform)]
  names(gly_color) = NULL
  gly_shape <- shape[as.character(gly_list$glycoform)]
  names(gly_shape) = NULL

  # 绘图
  gly_graph <- ggplot2::ggplot()+
    ggplot2::xlab("")+
    ggplot2::ylab("")+
    ggplot2::ggtitle("")+
    ggplot2::coord_equal() +
    # ggplot2::theme_bw()+
    ggplot2::geom_segment(ggplot2::aes(x = connect_info(structure,gly_list)$start_x,
                     y = connect_info(structure,gly_list)$start_y,
                     xend = connect_info(structure,gly_list)$end_x,
                     yend = connect_info(structure,gly_list)$end_y,))+
    ggplot2::geom_point(ggplot2::aes(x=gly_list[,'x'],y=gly_list[,'y']),
               shape=gly_shape,fill=gly_color,size=6,color='black',stroke=0.5)+
    ggplot2::geom_text(data = struc_annotation,
                       ggplot2::aes(x = x, y = y, label = annot))+
    ggplot2::theme(text=ggplot2::element_text(family="arial"))+
    ggplot2::theme_void()
  return(gly_graph)
}

test_structure <- c(
  'α-D-Galp-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→?)-β-D-GlcpNAc-(1→3)[α-D-Galp-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→?)-β-D-GlcpNAc-(1→6)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→3)-β-D-Galp-(1→3)[α-D-Galp-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-α-D-GalpNAc-(1→',
  "β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)[β-D-Galp-(1→4)-β-D-GlcpNAc-(1→4)]-α-D-Manp-(1→3)[β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)-α-D-Manp-(1→6)]-β-D-Manp-(1→4)-β-D-GlcpNAc-(1→4)[α-L-Fucp-(1→6)]-β-D-GlcpNAc-(1→",
  'α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→3)[α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→3)-β-D-Galp-(1→3)[α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-α-D-GalpNAc-(1→',
  'α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)[α-D-Galp-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→4)]-α-D-Manp-(1→3)[α-D-Galp-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)[α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-α-D-Manp-(1→6)]-β-D-Manp-(1→4)-β-D-GlcpNAc-(1→4)[α-L-Fucp-(1→6)]-β-D-GlcpNAc-(1→',
  'α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)-α-D-Manp-(1→3)[β-D-GlcpNAc-(1→4)][α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)-α-D-Manp-(1→6)]-β-D-Manp-(1→4)-β-D-GlcpNAc-(1→4)-β-D-GlcpNAc-(1→',
  'α-D-GalpNAc-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)-α-D-Manp-(1→3)[β-D-GlcpNAc-(1→4)][β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)-α-D-Manp-(1→6)]-β-D-Manp-(1→4)-β-D-GlcpNAc-(1→4)[α-L-Fucp-(1→6)]-β-D-GlcpNAc-(1→',
  'α-D-Manp-(1→3)[β-D-Galp-(1→4)-β-D-GlcpNAc-(1→2)[β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-α-D-Manp-(1→6)]-β-D-Manp-(1→4)-β-D-GlcpNAc-(1→4)-β-D-GlcpNAc-(1→',
  'α-L-Fucp-(1→2)-β-D-Galp-(1→?)-β-D-GlcpNAc-(1→3)[α-L-Fucp-(1→2)-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-β-D-Galp-(1→?)-β-D-GlcpNAc-(1→3)[α-D-GalpNAc-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-β-D-Galp-(1→?)-β-D-GlcpNAc-(1→3)-β-D-Galp-(1→3)[α-D-GalpNAc-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-α-D-GalpNAc-(1→',
  'α-D-GalpNAc-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→3)[α-L-Fucp-(1→3)]-β-D-GlcpNAc-(1→3)[α-D-GalpNAc-(1→3)[α-L-Fucp-(1→2)]-β-D-Galp-(1→3)-β-D-GlcpNAc-(1→3)]-β-D-Galp-(1→3)-β-D-GlcpNAc-(1→3)-β-D-Galp-(1→3)[β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-α-D-GalpNAc-(1→'
)
# 'WURCS=2.0/4,18,17/[a2112h-1a_1-5_2*NCC/3=O][a2112h-1b_1-5][a2122h-1b_1-5_2*NCC/3=O][a1221m-1a_1-5]/1-2-3-2-3-2-3-3-2-4-3-2-4-1-3-2-4-1/a3-b1_a6-o1_b3-c1_d3-e1_d6-k1_f3-g1_f6-h1_h4-i1_i2-j1_k4-l1_l2-m1_l3-n1_o4-p1_p2-q1_p3-r1_c?-d1_e?-f1'
for (i in test_structure){
  gly_test <- glyparse::parse_iupac_extended(i)
  structure <- glyrepr::get_structure_graphs(gly_test)
  # gly_test2 <- parse_iupac_condensed(as.character(gly_test))
  draw <- gly_draw(structure)
  print(draw)
}
