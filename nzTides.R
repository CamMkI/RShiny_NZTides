source("./init.R")

dylt <- NULL
if (file.exists(file.path(DB_PATH, "nz_daylight_savings.csv"))) {
  dylt <- read.csv(file.path(DB_PATH, "nz_daylight_savings.csv"), stringsAsFactors = F)
}

tides <- readRDS(file.path(DB_PATH, "nztide_2023_2026.rds"))
mainPorts <- readRDS(file.path(DB_PATH, "nzmainport_2023_2026.rds"))
mainPorts <- mainPorts[mainPorts$port != "Scott Base",]

mainPorts$latitude_dec <- toDecLat(mainPorts$latitude)
mainPorts$longitude_dec <- toDecLong(mainPorts$longitude)

portsList <- mainPorts$port
portsList <- portsList[order(portsList)]
