#' Provided 'set_grass_env' has been called, function returns upstream
#' basin statistics for a user specified pour point, called from 'run_basin_stats'.
#'
#' This function assumes 'set_grass_env' has been called. This function will
#' for a specified pour point delineate the upstream area from the 
#' DEM present in the GRASS environment using 'r.water.outlet'. Then for
#' all of the GRASS covariate raster defined by the 'covar_rast' basin statistics
#' are calculated using 'r.univar'. For each covariate raster the statistic to
#' return must be specified in the corresponding 'stat_vec' character vector. Returns a 
#' vector with the UID and associated statistics for each covariate raster.
#' Function called by 'run_basin_stats' so need not be called by user. 
#'
#' @param x The X-Coordinate of the pour point in units of DEM.
#' @param y The Y-Coordinate of the pour point in units of DEM.
#' @param uid A unique identifier number for the pour point. 
#' @param covar_rast Character vector of GRASS-GIS covariate
#' raster names (assumed to be present in GRASS env.) 
#' over which to calculate upstream statistics. 
#' @param stat_vec Character vector, order corresponds 
#' to 'covar_rast' and specifies what upstream statistic
#' to return for each covariate raster. Must be one of 
#' 'N', 'MIN', 'MAX', 'RANGE', 'MEAN', 'MAE', 'STDDEV', 
#' 'VAR', 'SUM', 'PRCT' or 'NULL_CELLS'.   
#' @return A vector with upstream basin statistics for 
#' each covarite raster defined by 'covar_rast' and 'stat_vec'.
#' @export
upstream_basin_stats<-function(x,y,uid,covar_rast,stat_vec)
{
  out_str_basin<-paste('basin_',uid,sep="")
  out_str_stats<-paste(tempdir(),'/basin_stat_',uid,'.txt',sep="")
  
  rgrass7::execGRASS(cmd='r.water.outlet',
            parameters = list(input='dir',
                              coordinates=c(x,y),
                              output=out_str_basin),
            flags = c('overwrite','quiet'))
  
  stat_tab<-c(uid)
  
  for(i in c(1:length(covar_rast)))
  {
    rgrass7::execGRASS(cmd='r.univar',
              parameters = list(map=covar_rast[i],
                                zones=out_str_basin,
                                output=out_str_stats,
                                separator='newline'),
              flags=c('g','overwrite'))
    
    stats<-readr::read_lines(out_str_stats)
    
    
    if(stat_vec[i]=='N')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[2],"=")[[1]][2])
    }
    if(stat_vec[i]=='MIN')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[5],"=")[[1]][2])
    }
    if(stat_vec[i]=='MAX')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[6],"=")[[1]][2])
    }
    if(stat_vec[i]=='RANGE')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[7],"=")[[1]][2])
    }
    if(stat_vec[i]=='MEAN')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[8],"=")[[1]][2])
    }
    if(stat_vec[i]=='MAE')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[9],"=")[[1]][2])
    }
    if(stat_vec[i]=='STDDEV')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[10],"=")[[1]][2])
    }
    if(stat_vec[i]=='VAR')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[11],"=")[[1]][2])
    }
    if(stat_vec[i]=='SUM')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[11],"=")[[1]][2])
    }
    if(stat_vec[i]=='PRCT')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[11],"=")[[1]][2])/as.numeric(strsplit(stats[4],"=")[[1]][2])
    }
    if(stat_vec[i]=='NULL_CELLS')
    {
      stat_tab[i+1]<-as.numeric(strsplit(stats[3],"=")[[1]][2])
    }
    
  }
  
  file.remove(out_str_stats)
  
  return(stat_tab)
}


#' Provided 'set_grass_env' has been called, function returns upstream
#' basin statistics for all pour points defined in a pour point data
#' frame. 
#'
#' This function assumes 'set_grass_env' has been called. This function 
#' iterates 'upstream_basin_stats' over a table of pour points
#' in parallel. The table of pour points must be a data.frame with
#' three columns labeled 'X','Y' and 'UID', representing the X, and
#' Y coordinates of the pour point in units matching the DEM, and a 
#' unique identifying number (UID). A this function runs in parallel,
#' the user must specify the number and type of processes to apply.  
#'
#' @param procs Number of concurrent process to use, defaults to 1.
#' @param proc_type Type of 'parallel' cluster to apply, defaults to 'FORK'
#' @param pour_pnts A data.frame with three columns labeled 'X', 'Y' and 'UID', 
#' representing the X, and Y coordinates of the pour point in units matching 
#' the DEM, and a unique identifying number (UID).
#' @inheritDotParams upstream_basin_stats covar_rast stat_vec 
#' @return A data frame with upstream statistics computed for each covarite raster
#' defined in 'covar_rast' over each pour point provided in 'pour_pnts', along with
#' the pour point UID.
#' @export
run_basin_stats<-function(pour_pnts,covar_rast,stat_vect,procs=1,proc_type='FORK')
{

  cl<-parallel::makeCluster(procs,type=proc_type)
  doParallel::registerDoParallel(cl)
  
  basin_stats_lst<-foreach::foreach(i=c(1:nrow(pour_pnts))) %dopar% upstream_basin_stats(pour_pnts$X[i],pour_pnts$Y[i],pour_pnts$UID[i],covar_rast,stat_vect)
  
  basin_stats<-c()
  
  for(basin in c(1:nrow(pour_pnts)))
  {
    basin_stats<-rbind(basin_stats,basin_stats_lst[[basin]])
  }
  
  basin_stats<-as.data.frame(basin_stats)
  
  colnames(basin_stats)<-c('UID',covar_rast)
  
  parallel::stopCluster(cl)
  
  return(basin_stats)
}


