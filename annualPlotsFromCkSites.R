library(sf)
library(tidyverse)
library(raster)

shpdir <- "Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\EasternJarrahForest\\field_datasets\\"

shp1 <- st_read(paste0(shpdir, "ejf_check_pt_random50_control.shp"), stringsAsFactors = FALSE)
shp2 <- st_read(paste0(shpdir, "ejf_check_pt_random50_decline.shp"), stringsAsFactors = FALSE)
shp2 <- dplyr::select(shp2, -name)
colnames(shp2)[3] <- "name"

shp1 <- mutate(shp1, siteNum = paste0("C", str_sub(name, 11, 13)))
shp2 <- mutate(shp2, siteNum = paste0("D", str_sub(name, 11, 13)))
shp <- rbind(shp1, shp2)

stk <- stack("Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\EasternJarrahForest\\data\\out_rst\\fmp_i35_stack_njf_1988to2018_2020-05-25.tif")
ex <- raster::extract(stk, shp)
df <- as.data.frame(ex)
colnames(df) <- paste0("c", seq(1988, 2018, by = 1))
df$site <- shp$siteNum
dfg <- df %>% gather(key = yearc, value = index, 1:31) %>%
  mutate(year = as.numeric(str_sub(yearc, start = 2))) %>%
  dplyr::select(-yearc)

sites <- unique(dfg$site)

for (i in 1:length(sites)){
  dfi <- filter(dfg, site == sites[1])
  
  ggplot(dfi, aes(index, year))+
    geom_point()+
    geom_line()
}
