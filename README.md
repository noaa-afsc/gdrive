# Description
Within NOAA, we are directed to not upload any data that may contain confidential information to a GitHub repository. This complicates how we manage projects as it requires us to separate our code from our data, and this issue compounds as the number of collaborators working on the same project increases. Every collaborator has to maintain local copies of their data inputs and outputs, and making sure everyone is up-to-date all the time is time-consuming and annoying. By leveraging Shared Google Drives and the `googledrive` R package, this package supports a workflow where:
- all data is securely stored and versioned in the Google Shared Drive
- collaborators can conveniently upload and download their data from the same scripts
- checks are performed to ensure all collaborators are up-to-date
- compatibiltiy with google cloud workstations

These functions are built such that data files (currently only `.Rdata` and `.Rds` formats are fully-supported) cannot be deleted, moved, or renamed from the shared drives, but file versions can be updated. Therefore, any contributor within a Google Shared drive with 'contributor' level access can utilize this package without fear of accidental losses. There is no risk of losing anything on the shared drive by uploading, as all prior versions are retained forever and easily accessible.

Although the package was built with collaboration in mind, it also streamlines data management for solo projects. This package was also set up to work on Google Cloud Workstations. 

Note: Due to the nature of how Shared and non-shared drives differ in their structure and protections, this package was built to only be compatible with Shared Google Drives.

## Developers

Geoff Mayhew [geoff.mayhew@noaa.gov](geoff.mayhew@noaa.gov)

# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
