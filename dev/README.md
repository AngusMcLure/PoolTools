# Developer notes  

## Managing the renv environment

```R
# Creating the environment
renv::init()
# Check status of environment
renv::status()
# Update packages in lockfile
renv::update()
# Update lockfile to use latest versions of packages
renv::snapshot()
```

## Installing Pool-Box programs
To make sure the renv lockfile includes both `CRAN` and `angusmclure` as 
Repositories, update the global options:

```R
options(repos = c(CRAN = "https://cloud.r-project.org",
                   angusmclure = "https://angusmclure.r-universe.dev") )
```

To install PoolTestR and PoolPoweR:

```R
# Install PoolTestR from R-universe
renv::install("PoolTestR", repos = c("https://angusmclure.r-universe.dev") )

# Install PoolPoweR: github dev version
devtools::install_github("AngusMcLure/PoolPoweR")
```

# Developer checklist
## Specifying credentials

Install `rsconnect`:

```R
install.packages('rsconnect')
```

Authorise the account using the information from shinyapps.io:

```R
rsconnect::setAccountInfo(name = "", token = "", secret = "")
```

## Pre-push  

Optional linting:  
```R
styler::style_dir(".")  
``` 


## Pre-deployment  

Updating the app version:  
- In `global.R`, update the `appVersion`  

Update the Changelog:  
- Edit `CHANGELOG.md`  

Update the documentation (currently GH wiki):  
- https://github.com/AngusMcLure/PoolTools/wiki


## Deployment  

Deploying the app to shinyapps.io:  
```R
rsconnect::deployApp()  
```

