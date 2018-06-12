#* @apiTitle SeBMS API
#* @apiDescription Swedish Butterflies - Data and Plots
#* @apiVersion 0.1

# apiTOS
# apiContact
# apiLicense
# apiHost
# apiBasePath
# apiSchemes
# apiConsumes
# apiProduces

#* Precipitation and temperature plot for 2015
#* @get /sebms_precip_temp_2015_plot
#* @serializer contentType list(type='image/png')
function() {
  img <- sebms_precip_temp_2015_plot()
  #sebms_ggsave(p, filename = "out.png")
  magick::image_write(img, "out.png")
  readBin("out.png", "raw", n = file.info("out.png")$size)
}

#* Data sites used in the Swedish Butterfly Monitoring scheme
#* @get /sebms_data_sites
function() {
  sebms_data_sites
}

#* Species per site and sitetype plot
#* @get /sebms_species_per_site_sitetype_plot
#* @png
function() {
  p <- sebms_species_per_site_sitetype_plot()
  print(p)
}
