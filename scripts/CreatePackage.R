#Create a package and add functions to the package
usethis::create_package(".")
usethis::use_roxygen_md()

usethis::use_package("lubridate", type = "Imports")
devtools::document()
devtools::install()

#run this after updating functions
devtools::document()
devtools::load_all()

# might need to run this to udate git
system('git config --global --add safe.directory "E:/CNFAIC/Projects/MachineLearning/Rcode/cnfacML"')

# first push
system('git push -u origin main')
