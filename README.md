# Description
Within NOAA, we are directed to not upload any data that may contain confidential information to a GitHub repository. This complicates how we manage projects as it requires us to separate our code from our data, and this issue compounds as the number of collaborators working on the same project increases. Every collaborator has to maintain local copies of their data inputs and outputs, and making sure everyone is up-to-date all the time is time-consuming and annoying. By leveraging Shared Google Drives and the `googledrive` R package, this package supports a workflow where all data is securely stored and versioned on the Google cloud, collaborators can conveniently upload and download their data from the same scripts, all while checks are performed to ensure all collaborators are up-to-date. 

These functions are built such that data files (currently only `.Rdata` and `.Rds` formats are supported) cannot be deleted, moved, or renamed from the shared drives, but file versions can be updated. Therefore, any contributor within a Google Shared drive with 'contributor' level access can utilize this package without fear of accidental losses. Moreover, all older versions of data files are specified to be kept forever, avoiding the default standard limitations of Google drives where only 100 versions of a file are retained. Although the package was built with collaboration in mind, it also streamlines data management for solo projects as it removes any need to access a shared 

Due to the nature of how Shared and non-shared drives differ in their structure and protections, this package was built to only be compatible with Shared Google Drives.

## Developers

Geoff Mayhew [geoff.mayhew@noaa.gov](geoff.mayhew@noaa.gov)

If you have NMFS collaborators (i.e. have noaa.gov email) on your repository who are not members of the NMFS GitHub Enterprise Cloud account, have them complete a user request. If they are not NOAA FTEs or Affiliates, contact an administrator to help you add them to your repository or to transfer in your repository with outside collaborators. 

For repositories migrated to noaa-afsc organization
1. Update your README.md file to include the disclaimer and an open access license. See below for a description of licenses.
2. Add a description and info on who created the content (otherwise the org managers will not know who to contact).
3. Add tags (far right side on repo) to help users find repositories. See the other repositories for examples.
4. Add an open LICENSE file. For government work, we are required to use an open LICENSE. If non-government FTEs were contributors and the repository does not yet have an open license on it, make sure all parties agree before applying an open source license.
5. Add the file .github/workflows/secretScan.yml. This will check for token and keys that are accidentally committed to a repository.

# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
