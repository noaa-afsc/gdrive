# function_test_script
# This script is meant to be a testing ground for functions of this package. Should be able to run through all of these
# without throwing any errors (unlss that is what we want)!

# Load the development version of the package functions
devtools::load_all()

#======================================================================================================================#
## Set Dribble ----

# Set the dribble to our generic data folder
test_data_dribble <- gdrive_set_dribble("Data/")

# List the files in the dribble
gdrive_ls(test_data_dribble)

# See what happens when you set the dribble to one of these files. It should not work!
gdrive_set_dribble("Data/species_conversion_table.xlsx/")
gdrive_set_dribble("Data/ak_shp")

### Accessing another shared google drive ----
# Testing accessing a shared google drive that you do not have full access to
# (only Geoff and Craig can test this as Debra Duarte granted them access to this folder)
gdrive_set_dribble(folder_id = "1x0UjP5tEyTCgUdMq_43PoN2ww7zFrjaA")

#======================================================================================================================#
## Upload / Download ----

# Save and upload a dummy dataset
dummy_dat <- cars
local_dat_filepath <- "test/test_data/dummy_dat.Rdata"
save(dummy_dat, file = local_dat_filepath)
gdrive_upload(local_dat_filepath, test_data_dribble)
#   local_path <- local_dat_filepath; gdrive_dribble <- test_data_dribble; ver = NULL

# Make an edit (but save it locally somewhere else) so that the gdrive is 'ahead' of the local
#   local_path <- "test_data/edit/dummy_dat.Rdata"; gdrive_dribble <- test_data_dribble; ver = NULL
dummy_dat <- rbind(dummy_dat, dummy_dat[1, ])
save(dummy_dat, file = "test/test_data/edit/dummy_dat.Rdata")
gdrive_upload("test/test_data/edit/dummy_dat.Rdata", test_data_dribble)

# Try uploading our local out-of-date version. Should get a warning.
# Cancel! Don't upload!
gdrive_upload(local_dat_filepath, test_data_dribble)

# Try downloading. Should see that the local is behind the gdrive.
# Yes, download the most recent version, bringing the local up-to-date
gdrive_download(local_dat_filepath, test_data_dribble)
#   local_path <- local_dat_filepath; gdrive_dribble <- test_data_dribble; ver = NULL

# Now that we are up to date, check the messages when you're up-to-date
#   local_path <- "test_data/dummy_dat.Rdata"; gdrive_dribble <- test_data_dribble; ver = NULL
gdrive_upload(local_dat_filepath, test_data_dribble)
gdrive_download(local_dat_filepath, test_data_dribble)

### Save temporary files and load them ----
# Download a gdrive file to a temporary local folder and load it. Works with older versions as well
(load(gdrive_download(local_dat_filepath, test_data_dribble, ver = 1, temp = T)))
(load(gdrive_download("test_data/edit/dummy_dat.Rdata", test_data_dribble, temp = T)))


#======================================================================================================================#

### Check file versions ----
local_dat_filepath <- "test_data/dummy_dat.Rdata"
gdrive_versions(local_dat_filepath, test_data_dribble)
# Should also work if you have other arguments, like if you're just changing a call to gdrive_download()
gdrive_versions(local_dat_filepath, test_data_dribble, ver = 3, temp = T)

#======================================================================================================================#