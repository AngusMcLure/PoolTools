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
