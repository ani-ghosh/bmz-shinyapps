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
getMeanRaster <- function(rr, years){
  rr <- subset(rr, grep(paste(years,collapse="|"), names(rr)))
  rr <- calc(rr, fun = mean)
  return(rr)
} 

# crop-specific
getMeanRasterCrop <- function(f, crop, years){
  rr <- getCropRaster(f, crop)
  rr <- getMeanRaster(rr, years)
  return(rr)
} 

dir <- "C:\\Users\\anibi\\Documents\\work\\bmz_shiny"
g1 <- getData("GADM", country="KEN", level=1, path=file.path(dir, "vector"))
g2 <- getData("GADM", country="ETH", level=1, path=file.path(dir, "vector"))
gg <- bind(g1,g2)

ff <- list.files(file.path(dir, "spatial_output"), pattern = "*.tif$",
                 recursive = TRUE, full.names = TRUE)

# analysis at 10 min
f1 <- grep("10min", ff, value=TRUE)

##############################################################################
# input
# manure 
r1 <- grep("Manurefrac_2014_EA", f1, value=TRUE)
man <- getCropRaster(r1, "TeCo")

# total fertilization (unit kg-N/m^2)
r2 <- grep("Nfert_total_2014_EA", f1, value=TRUE)
fert <- getCropRaster(r2, "TeCo")

# convert fert from kg-n/m^2 to kg-N/ha
fert <- fert/0.0001

# total manure applied
tman <- man*fert

# irrigation
r3 <- grep("CRUJRA_PURE_MAIZE_10min/A1-irri/irrigation.tif", f1, value=TRUE)
irr <- getMeanRaster(r3, 2091:2100)

# crop fractions
r4 <- grep("cropfracs_for_EA_run_100maize_EA", f1, value=TRUE)
getCropRaster(r1, "TeCo")

##############################################################################
# output
f2 <- grep("CRUJRA_PURE_MAIZE_10min", f1, value=TRUE)

# yield and soc
r4 <- grep("yield", f2, value=TRUE)
yld <- getMeanRasterCrop(r4[1], 2091:2100, "TeCo")
