# CBA analysis
# A. Input
# 1.	Manure and fertilizer (for maize (TeCo)): To get to the amounts of manure applied per grid cell 
#     multiply Manurefrac_2014_EA.txt with Nfert_2014_EA.txt. 
#     The fertilizer amounts will be (1-Manurefrac) * amount_of_fertilizer in the Nfert file. 
# 2.	Irrigation: irrigation.out  file and similar to the manure and fertilizer 
# we also need averages for these values per county and per grid cell.
# B. output use the CRUJRA_PURE_MAIZE simulations only.  
# Need average  SOC values and maize yield values for the A1 scenarios in this directory 
# per county values and also per grid cell values for Vihiga, Kakamega and Siaya.

# Average the last 10 years of output (2091-2100)

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

# estimate last 10 year average
getMeanRaster <- function(f, years){
  rr <- readRaster(f)
  rr <- subset(rr, grep(paste(years,collapse="|"), names(rr)))
  rr <- calc(rr, fun = mean)
  return(rr)
} 

# crop-specific
getMeanRasterCrop <- function(f, crop, years){
  rr <- getCropRaster(f, crop)
  rr <- subset(rr, grep(paste(years,collapse="|"), names(rr)))
  rr <- calc(rr, fun = mean)
  return(rr)
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

##############################################################################
getModelStat <- function(model, f1){
  cat("processing", model, "\n")
  # input
  # crop fractions
  r0 <- grep("lu_2014_luh2_EA_10min", f1, value=TRUE)
  r0 <- getCropRaster(r0, "lu_2014_luh2_EA_10min")
  
  # manure 
  r1 <- grep("Manurefrac_2014_EA", f1, value=TRUE)
  r1 <- getCropRaster(r1, "TeCo_2014")
  names(r1) <- "manurefrac" 
  
  # total fertilization (unit kg-N/m^2)
  r2 <- grep("Nfert_total_2014_EA", f1, value=TRUE)
  r2 <- getCropRaster(r2, "TeCo_2014")
  names(r2) <- "nfert"
  
  # convert fert from kg-n/m^2 to kg-N/ha
  r2 <- r2/0.0001
  
  # total manure applied
  r12 <- r1*r2
  names(r12) <- "tman"
  
  # total fertlizer
  r21 <- (1-r1)*r2
  names(r21) <- "tfert"
  
  # model specifics
  
  # irrigation
  r3 <- grep("CRUJRA_PURE_MAIZE_10min/A1-irri/irrigation.tif", f1, value=TRUE)
  r3 <- getMeanRaster(r3, 2091:2100)
  names(r3) <- "wat_demand"
  
  # crop fractions
  # r4 <- grep("cropfracs_for_EA_run_100maize_EA", f1, value=TRUE)
  # fracs <- getCropRaster(r4, "TeCo")
  
  # maize fractions
  # r5 <- grep("cropfracs_for_EA_run_allcrops_EA_10min", f1, value=TRUE)
  # mfracs <- getCropRaster(r5, "TeCo")
  # plot(mfracs)
  
  ##############################################################################
  # output
  f2 <- grep("CRUJRA_PURE_MAIZE_10min", f1, value=TRUE)
  
  # yield
  r4 <- grep("yield", f2, value=TRUE)
  r4 <- getMeanRasterCrop(r4[9], "TeCo", 2091:2100)
  r4 <- r4*10 # convert to ton/ha
  names(r4) <- "yield"
  
  # soc
  r5 <- grep("cpool_cropland", f2, value=TRUE)
  # yld <- getCropRaster(r6[1], "TeCo")
  r5 <- getMeanRasterCrop(r5[9], "SoilC", 2091:2100)
  names(r5) <- "soc"
  
  # nflux
  # "leach_2100",  "flux_2100"
  r6 <- grep("nflux_cropland", f2, value=TRUE)
  r61 <- getMeanRasterCrop(r6[9], "leach", 2091:2100)
  r62 <- getMeanRasterCrop(r6[9], "flux", 2091:2100)
  names(r61) <- "leach"
  names(r62) <- "flux"
  
  # final stack
  rs <- stack(r1,r2,r12,r21,r3,r4,r5,r61,r62)
  names(rs)
  
  dv <- extract(rs, g1, weights = TRUE, 
                normalizeWeights = FALSE, fun=mean, na.rm=TRUE, sp = TRUE)
  
  out <- dv[, names(dv) %in% c("NAME_0","NAME_1", names(rs))]
  out <- data.frame(out, tman_dm = out$tman*0.0175*1000, 
                    tfert_bags_ha = (out*tfert/23)*10000)
  data.table::fwrite(out@data, file.path(dir, "a2_county_ken.csv"))
  
  
  g11 <- g1[g1$NAME_1 %in% c("Vihiga", "Kakamega", "Siaya"),]
  cc <- extract(rs, g11, cellnumbers=TRUE)
  cc <- do.call(rbind, cc)
  cc <- cbind(xyFromCell(rs, cc[,1]), cc)
  cc <- data.frame(county=c(rep("Kakamega", 5), rep("Siaya", 11), rep("Vihiga",1)),cc,
                   stringsAsFactors = FALSE)
  
  data.table::fwrite(cc, file.path(dir, "a2_grid_3county_ken.csv"))
  
  out <- dv[, names(dv) %in% c("NAME_0","NAME_1", names(rs))]
  data.table::fwrite(out@data, file.path(dir, "a2_county_ken.csv"))
}
