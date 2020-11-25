library(data.table)
library(raster)
library(future.apply)

####################################################################################
saveSummarystat <- function(f, ref, gg){
  cat("processing", f, "\n")
  
  # where to save files
  ofile <- gsub("2ND_ROUND_OUTPUTS", "spatial_output", f)
  odir <- dirname(ofile)
  dir.create(odir, FALSE, TRUE)
  
  # input data
  v <- fread(f)
  # check if Lan/Lat and data exists
  cc <- c("Lon", "Lat")
  check <- sum(names(v) %in% cc)
  
  if(ncol(v) > 2 && check==2){
    # create raster
    if(sum(names(v) %in% "Year")>0){
      rs <- createRasterMultiyear(v, ref, cc)
    } else {
      rs <- createRaster(v, cc, ref)
    }
    # summary values
    dv <- extract(rs, gg, weights = TRUE, 
                  normalizeWeights = FALSE, fun=mean, na.rm=TRUE, sp = TRUE)
    
    writeRaster(rs, gsub(".txt$|.out$",".tif",ofile), overwrite = TRUE)
    saveRDS(names(rs), gsub(".txt$|.out$","_bandnames.rds",ofile))
    fwrite(dv@data, gsub(".txt$|.out$",".csv",ofile))
  } 
  flush.console()
}

createRaster <- function(v, cc, ref){
  rs <- rasterize(v[,cc, with=FALSE], ref, 
                  v[,!(colnames(v) %in% c(cc, "Year")), with=FALSE], fun=mean)
  return(rs)
}

createRasterMultiyear <- function(v, ref, cc){
  rsl <- lapply(unique(v$Year), function(year, v, ref){
    vs <-  v[v$Year==year, ]
    rs <- createRaster(vs, cc, ref)
    names(rs) <- paste0(names(rs), "_", year)
    return(rs)
  }, v, ref)
  rs <- stack(rsl)
  return(rs)
}

createRefraster <- function(fr, resfrac){
  vc <- fread(fr)
  names(vc) <- c("x","y")
  e <- extent(vc[,1:2])
  r <- raster(ext = e, 
              crs=CRS('+init=EPSG:4326'), resolution=1/resfrac, vals=NULL)
  return(r)
}

#########################################################################################
dir <- "C:\\Users\\anibi\\Documents\\work\\bmz_shiny\\2ND_ROUND_OUTPUTS"
datadir <- list.dirs(dir, full.names = TRUE, recursive = FALSE)
g1 <- getData("GADM", country="KEN", level=1, path=dir)
g2 <- getData("GADM", country="ETH", level=1, path=dir)
gg <- bind(g1,g2)

# create a folder for saving raster, shapefile, and csv output, keep the same folder structure as the input
ff <- list.files(datadir, pattern = ".out$|.txt$",full.names = TRUE, recursive = TRUE)
# only 

# create reference raster at two resolution
fr10 <- grep("gridlist_in_EA_10min", ff, value=TRUE)
ref10 <- createRefraster(fr10, resfrac=5.85)

fr05 <- grep("gridlist_in_EA_0p5", ff, value=TRUE)
ref05 <- createRefraster(fr05, resfrac=2)

# get summary two set of resolution
ff1 <- grep("10min", ff, value=TRUE)

# single run
# saveSummarystat(ff1[4], ref10, gg)
# 10 min
plan(multisession, workers = 6)
future_lapply(ff1, saveSummarystat, ref10, gg)
future:::ClusterRegistry("stop") 

# 0.5 deg
ff2 <- grep("10min", ff, value=TRUE, invert=TRUE)
plan(multisession, workers = 8)
future_lapply(ff2, saveSummarystat, ref05, gg)
future:::ClusterRegistry("stop") 


# check results
# xx <- list.files("C:\\Users\\anibi\\Documents\\work\\bmz_shiny\\spatial_output",
#                   pattern = ".tif$", full.names = TRUE, recursive = TRUE)
# x <- stack(xx[700])
# plot(x[[1]])
