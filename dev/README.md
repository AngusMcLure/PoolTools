# Developer checklist  

## Pre-push  

Linting:  
```r
styler::style_dir(".")  
``` 

## Pre-deployment  

Updating the app version:  
- In `global.R`, update the `appVersion`  

Update the Changelog:  
- Edit `CHANGELOG.md`  

Deploying the app to shinyapps.io:  
```r
rsconnect::deployApp()  
``` 
