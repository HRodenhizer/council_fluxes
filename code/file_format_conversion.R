################################################################################
###               Council Tower AVIRIS Map File Conversion                   ###
###                         Code by HGR 2/2023                               ###
################################################################################

### Load Libraries #############################################################
library(terra)
################################################################################


# nitrogen map
nitrogen <- rectify(rast('/home/hrodenhizer/Documents/permafrost_pathways/tower_network/council_tower/vegetation_maps/NGA273/ang20190709t195734_nitrogen/ang20190709t195734_nitrogen'))
writeRaster(nitrogen,
            '/home/hrodenhizer/Documents/permafrost_pathways/tower_network/council_tower/vegetation_maps/NGA273/ang20190709t195734_nitrogen.tif')

# albedo map
albedo <- rectify(rast('/home/hrodenhizer/Documents/permafrost_pathways/tower_network/council_tower/vegetation_maps/NGA273/ang20190709t195734_albedo/ang20190709t195734_albedo'))
writeRaster(albedo,
            '/home/hrodenhizer/Documents/permafrost_pathways/tower_network/council_tower/vegetation_maps/NGA273/ang20190709t195734_albedo.tif')

# pft map
pft <- rectify(rast('/home/hrodenhizer/Documents/permafrost_pathways/tower_network/council_tower/vegetation_maps/NGA273/ang20190709t195734_pft/ang20190709t195734_pft'))
writeRaster(pft,
            '/home/hrodenhizer/Documents/permafrost_pathways/tower_network/council_tower/vegetation_maps/NGA273/ang20190709t195734_pft.tif')
