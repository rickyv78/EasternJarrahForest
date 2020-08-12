library(sf)
library(tidyverse)
library(raster)
library(lubridate)

shpdir <- "Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\EasternJarrahForest\\field_datasets\\"
mga50 <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"
cal <- read_csv("Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\calibration\\Jarrah Forest\\coefficients\\i35_linear.csv")

# Districts:  MER GLD PIL ESP SWC MRA MOR DON BWD PHL WEL FRK ALB GTN EXM SHK WTN GSN PHS CWB
fireAll <- st_read(dsn="V:\\GIS1-Corporate\\data\\GDB\\Fire\\Burn_data\\Fire_Data_Burns.gdb", layer = "CPT_FIRE_HISTORY",  
                   query = "SELECT * FROM \"CPT_FIRE_HISTORY\" WHERE FIH_DISTRICT IN ('PHS', 'PHL', 'SWC')", 
                   quiet = TRUE)
fireAll <- st_transform(fireAll, mga50)
plot(fireAll[, 1])

shp1 <- st_read(paste0(shpdir, "ejf_check_pt_random50_control.shp"), stringsAsFactors = FALSE)
shp2 <- st_read(paste0(shpdir, "ejf_check_pt_random50_decline.shp"), stringsAsFactors = FALSE)
shp2 <- dplyr::select(shp2, -name)
colnames(shp2)[3] <- "name"

shp1 <- mutate(shp1, siteNum = paste0("C", str_sub(name, 11, 13)))
shp2 <- mutate(shp2, siteNum = paste0("D", str_sub(name, 11, 13)))
shp <- rbind(shp1, shp2)
shp <- st_transform(shp, mga50)

stk <- stack("Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\EasternJarrahForest\\data\\out_rst\\fmp_i35_stack_njf_1988to2018_2020-05-25.tif")
ex <- raster::extract(stk, shp)
df <- as.data.frame(ex)
colnames(df) <- paste0("c", seq(1988, 2018, by = 1))
df$site <- shp$siteNum
dfg <- df %>% gather(key = yearc, value = index, 1:31) %>%
  mutate(year = ymd(paste0(str_sub(yearc, start = 2), "-03-01")), 
         cc = (index*cal[2,2, drop = TRUE]) + cal[1,2, drop = TRUE]) %>%
  dplyr::select(-yearc)

sites <- unique(dfg$site)

for (i in 1:length(sites)){
  dfi <- filter(dfg, site == sites[i])
  
  ply <- filter(shp, siteNum == sites[i] )
  pti <- st_intersection(ply, fireAll)
  pti <- pti %>% arrange(desc(FIH_YEAR1)) %>%
    filter(FIH_DATE1 > ymd("1985-01-01")) 
  pti$FIH_DATE1 <- as.Date(pti$FIH_DATE1)
  
  ggplot(dfi, aes(year, cc))+
    geom_point()+
    geom_line()+
    geom_vline(xintercept = pti$FIH_DATE1[1], colour = "red", linetype = 2)+  
    annotate("text", x= (pti$FIH_DATE1[1] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[1]," : ",pti$FIH_DATE1[1] ), 
             colour = "red", angle  = 90, size = 3.5)+
    geom_vline(xintercept = pti$FIH_DATE1[2], colour = "red", linetype = 2)+
    annotate("text", x= (pti$FIH_DATE1[2] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[2]," : ",pti$FIH_DATE1[2] ), 
             colour = "red", angle  = 90, size = 3.5)+
    geom_vline(xintercept = pti$FIH_DATE1[3], colour = "red", linetype = 2)+  
    annotate("text", x= (pti$FIH_DATE1[3] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[3]," : ",pti$FIH_DATE1[3] ), 
             colour = "red", angle  = 90, size = 3.5)+
    geom_vline(xintercept = pti$FIH_DATE1[4], colour = "red", linetype = 2)+
    annotate("text", x= (pti$FIH_DATE1[4] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[4]," : ",pti$FIH_DATE1[4] ), 
             colour = "red", angle  = 90, size = 3.5)+
    coord_cartesian(ylim = c(0, 70))+
    labs(
      title = paste0("Trend check site: ", sites[i]),
      x = "Date",
      y = "Vegetation Cover %", 
      caption = paste0("Index = i35")
    )+
    theme_bw()
  ggsave(paste0(shpdir, "\\graphs\\",sites[i], "_i35.png"), width = 7, height = 5)
    
}
