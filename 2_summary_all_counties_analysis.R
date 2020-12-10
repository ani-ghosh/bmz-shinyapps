# summary stat

library(raster)

readRaster <- function(f){
  fname <- gsub(".tif", "_bandnames.rds", f)
  r <- stack(f)
  nm <- readRDS(fname)
  names(r) <- nm
  return(r)
}

# get crop layer
getCropRaster <- function(f, crop){
  rr <- readRaster(f)
  rr <- subset(rr, grep(crop, names(rr)))
  return(rr)
} 

# estimate for average average
getMeanRaster <- function(f, years){
  rr <- readRaster(f)
  rr <- subset(rr, grep(paste(years,collapse="|"), names(rr)))
  rr <- calc(rr, fun = mean, na.rm = TRUE)
  return(rr)
} 

# crop-specific
getMeanRasterCrop <- function(f, crop, years){
  rr <- getCropRaster(f, crop)
  rr <- subset(rr, grep(paste(years,collapse="|"), names(rr)))
  rr <- calc(rr, fun = mean, na.rm = TRUE)
  return(rr)
} 

# get input

##############################################################################
getModelStat <- function(model, f1, years, cropname, gg, dir){
  cat("processing", model, "\n")
  
  # input
  # crop fractions
  r0 <- grep("lu_2014_luh2_EA_10min", f1, value=TRUE)
  r0 <- getCropRaster(r0, "CROPLAND_2014")
  names(r0) <- "cropland_fracs_2014"
  
  # manure 
  r1 <- grep("Manurefrac_2014_EA", f1, value=TRUE)
  r1 <- getCropRaster(r1, cropname)
  names(r1) <- paste0("manurefrac_", cropname) 
  
  # total fertilization (unit kg-N/m^2)
  r2 <- grep("Nfert_total_2014_EA", f1, value=TRUE)
  r2 <- getCropRaster(r2, cropname)
  names(r2) <- paste0("nfert_", cropname)
  
  # convert fert from kg-n/m^2 to kg-N/ha
  r2 <- r2/0.0001
  
  # total manure applied
  r12 <- r1*r2
  names(r12) <- paste0("tman_kg_ha_", cropname)
  
  # total fertlizer
  r21 <- (1-r1)*r2
  names(r21) <- paste0("tfert_kg_ha_", cropname)
  
  # for economic analysis
  r121 <- r12/(0.0175) 
  names(r121) <- paste0("tman_dry_matter_kg_ha_", cropname)
  
  r211 <- r21/23
  names(r211) <- paste0("tfert_total_bags_ha_", cropname)
  
  # irrigation
  r3 <- grep("CRUJRA_PURE_MAIZE_10min/A1-irri/irrigation.tif", f1, value=TRUE)
  r3 <- getMeanRaster(r3, years)
  names(r3) <- paste0("mean_wat_demand_", min(years), "_", max(years), "_", cropname)
  
  ##############################################################################
  # output
  # model specifics stat
  f2 <- grep("CRUJRA_PURE_MAIZE_10min", f1, value=TRUE)
  
  # yield
  r4 <- grep(file.path(model, "yield"), f2, value=TRUE)
  r4 <- getMeanRasterCrop(r4, "TeCo", years)
  names(r4) <- paste0("yield_", min(years), "_", max(years), "_", cropname)
  # for pure maize no need to multiply the crop-specific fraction on cropland to get cropland averaged yield annually
  r4 <- r4*10 # convert to ton/ha
  
  # soc
  r5 <- grep(file.path(model,"cpool_cropland"), f2, value=TRUE)
  r5 <- getMeanRasterCrop(r5, "SoilC", years)
  # cpool_cropland.out is up-scaled results at each gridcell by taking cropland fraction into account, 
  # in order to get the real and original ‘SoilC’, PLEASE keep in mind to divide by cropland fraction at each gridcell every year
  r5 <- r5/r0
  r5[is.infinite(r5)] <- NA
  names(r5) <- paste0("soc_", min(years), "_", max(years), "_", cropname)
  
  # nflux
  # "leach_2100",  "flux_2100"
  r6 <- grep(file.path(model, "nflux_cropland"), f2, value=TRUE)
  
  r61 <- getMeanRasterCrop(r6, "leach", years)
  r61 <- r61/r0
  r61[is.infinite(r61)] <- NA
  names(r61) <- paste0("leach_", min(years), "_", max(years), "_", cropname)
  
  r62 <- getMeanRasterCrop(r6, "flux", years)
  r62 <- r62/r0
  r62[is.infinite(r62)] <- NA
  names(r62) <- paste0("flux_", min(years), "_", max(years), "_", cropname)
  
  # final stack
  rs <- stack(r1, r2, r12, r21, r121, r211, r3, r4, r5, r61, r62)
  names(rs)
  
  # extract for each county
  dv <- extract(rs, gg, weights = TRUE, 
                normalizeWeights = FALSE, fun=mean, na.rm=TRUE, sp = TRUE)
  
  out <- dv[, names(dv) %in% c("NAME_0","NAME_1", names(rs))]
  
  odir <- file.path(dir, "spatial_output/all")
  dir.create(odir, FALSE, TRUE)
  saveRDS(out, file.path(odir, paste0(model, "_summary_stat_vector.rds")))
  
  writeRaster(rs, file.path(odir, paste0(model, "_stat_grid.grd")), overwrite = TRUE)
}

#########################################################################################
dir <- "C:\\Users\\anibi\\Documents\\work\\bmz_shiny"
g1 <- getData("GADM", country="KEN", level=1, path=file.path(dir, "vector"))
g2 <- getData("GADM", country="ETH", level=1, path=file.path(dir, "vector"))
gg <- bind(g1,g2)

ff <- list.files(file.path(dir, "spatial_output"), pattern = "*.tif$",
                 recursive = TRUE, full.names = TRUE)

# analysis at 10 min
f1 <- grep("10min", ff, value=TRUE)

# models 
mods <- list.dirs(file.path(dir, "spatial_output/Modelled_output/CRUJRA_PURE_MAIZE_10min"), recursive = FALSE)
mods <- basename(mods)

years <- 2020:2049 # corresponding to GCM 30 year period
cropname <- "TeCo_2014"
# getModelStat(mods[1], f1, years, cropname, gg, dir)
lapply(mods, getModelStat, f1, years, cropname, gg, dir)
