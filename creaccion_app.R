create_app(
  app_name    = "R215", 
  app_dir     = "F:\Proyecto PM10 y PM2.5",
  pkgs        = c('dplyr', 'ggplot2', 'mfp', 'plotly','leaps','MASS','beepr','stringr','lubridate','raster','rgdal','shiny','shinydashboard','shinyjs'),
  include_R   = TRUE    
)
create_app(
  app_name    = "R215", 
  app_dir     = "D:/Proyecto PM10 y PM2.5",
  pkgs        = c('dplyr', 'ggplot2', 'mfp', 'plotly','leaps','MASS','beepr','stringr','lubridate','raster','rgdal','shiny','shinydashboard','shinyjs'),  # CRAN-like repo packages
  include_R   = TRUE,     # Download R and install it with your app, if necessary
  R_version   = "3.5.1")
compile_iss()
1
