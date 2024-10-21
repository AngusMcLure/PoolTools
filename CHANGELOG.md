# Changelog  

All notable changes to this project will be documented in this file.  

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).  

## [0.1.5] - 2024-10-18

### Added  
- `ICC` branch added and used to create v0.1.5
- Updated to use `PoolTestR@ICC` version (includes ICC columns)
- `PoolTools` output now includes formatted ICC columns
- Updated prevalence columns names to explicitly state prevalence per units by
appending "%" (N=100) or "per N units" (N != 100) in output data frame
  - ICC columns are not multiplied by N

### Changed
- Refactored `run_pooltestr()` to use output of `which_pooltestr()` 

## [0.1.4] - 2024-07-19

### Added  
- Separate `reactiveValues()` objects to store UI values to prevent values
resetting when UI does.  
- `dev` branch to store all WIP i.e. PoolPoweR power and sample_size functions.  

### Changed
- Design inputs do not reset when upstream settings are changed for:
    - Fixed sample size  
	- Fixed sampling period  
- Design back-end overhaul; UI and values are decoupled.  
- Renaming cluster/hierarchical sampling in UI.  

## [0.1.3] - 2024-05-10  

Important UI changes to "Analyse" mode and cleaning up back-end reactivity. 

### Changed
- Dropdown arrow for html details appears on browser.  
- Download button no longer disappears when `hierarchy_valid()` changes.  
- "Real-time" formatting of analyse datatable output i.e. does not require
re-analysis with button click. Options include rounding and displaying
prevalence per value.  
- Moved datatable formatting options from Advanced settings to Display options. 
- Datatable output remains stable when settings change.  
- Remove 'max pools per cluster' option when not used.  
- Rename files to be sort-of-compliant with `golem` standards.  

## [0.1.2] - 2024-04-22  

### Added  
- Populated Help tabs with links to wiki docs.  

### Changed  
- Upload data label and tooltip.  

## [0.1.1] - 2024-04-16  

### Added  
- Roxygen comments to R util files.  
- `utils_dt_display.R` to handle post-pooltestr processing.  
- Prevalence multiply option (default 1*2000).  
- CITATION file.  

### Changed  
- Refactored pooltestr-related code.  
- Analyse prevalence rounding now part of `utils_dt_display.R`.  
- About page loads from a .md.  

### Removed  
- Redundant analyse datatable UI.  

## [0.1.0] - 2024-04-08  

### Added  
- Changelog and versioning.  
- Version is displayed in app title.  
- Dev files.  
