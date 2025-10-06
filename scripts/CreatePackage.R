

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

# set main as default (you already did, but harmless to repeat)
system('git branch -M main')

# add your remote (edit USERNAME + repo)
system('git remote add origin https://github.com/AndrewSchauer/cnfacML.git')


# Do this to update: ----------------------------------------------------------------------------
# 1. Save your local changes in a commit
system('git add -A')
system('git commit -m "Save local edits before pull"')

# 2. Pull remote updates and merge them
system('git pull --rebase')
# finish the rebase
system("git rebase --continue")

# push your rebased branch to GitHub
system("git push -u origin main")


# 3. Push merged version to GitHub
system('git push -u origin main')
