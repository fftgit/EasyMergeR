
data_demo_beaver <- list(
  beaver1 = datasets::beaver1,
  beaver2 = datasets::beaver2
)

data_demo_mini_mimic <- loadexcelbook("inst/app/www/miniMIMIC.xlsx")

usethis::use_data(data_demo_beaver,overwrite = T)
# usethis::use_data(data_demo_fever,overwrite = T)
usethis::use_data(data_demo_mini_mimic,overwrite = T)
