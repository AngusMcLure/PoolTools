# Developer checklist  

## Managing the renv environment
```
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
```
# Install PoolTestR from R-universe
options( repos = c(CRAN = "https://cloud.r-project.org",
                   angusmclure = "https://angusmclure.r-universe.dev") )
renv::install("PoolTestR", repos = c("https://angusmclure.r-universe.dev") )

# Install PoolPoweR: github dev version
devtools::install_github("AngusMcLure/PoolPoweR")
```

# Developer checklist
## Pre-push  

Optional linting:  
```r
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
```r
rsconnect::deployApp()  
```

