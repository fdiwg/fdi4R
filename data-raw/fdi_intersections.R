require(ows4R)
require(usethis)

#FAO-NFI intersection spatial layers
#ows4R connector for FAO/NFI intersections
WFS_UNFAO_NFI <- ows4R::WFSClient$new(
  url = "https://www.fao.org/fishery/geoserver/int/wfs",
  serviceVersion = "1.0.0",
  logger = "INFO"
)

int_layers = WFS_UNFAO_NFI$getFeatureTypes(T)$name

intersections = do.call(rbind, lapply(int_layers, function(int_layer){
  WFS_UNFAO_NFI$getFeatures(typeName = int_layer, propertyName = "layer1,code1,surface1,layer2,code2,surface2,surface,surface1_percent,surface2_percent")
}))
intersections$gml_id = NULL
intersections$geom = NULL
usethis::use_data(intersections, overwrite = TRUE)