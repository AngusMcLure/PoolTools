## About PoolTools

PoolTools is an easy-to-use application for the analysis and design of pooled testing surveys. PoolTools is an advancement of traditional tools for analysis of pooled data, such as PoolScreen®, offering an improved user interface, and support for analysis of data collected with cluster/hierarchical design.  

Some key features of PoolTools:  
- Estimation of disease (marker) prevalence from pooled samples  
- Identify the survey design (e.g. pool size and number of pools per cluster) that optimises cost-effectiveness of the survey for your setting and objectives  
- Power and sample size calculations (coming soon!)  
- Supports cluster sampling designs  

> [See the docs](https://github.com/AngusMcLure/PoolTools/wiki) to get started with PoolTools!  


### Under the hood

PoolTools is an open-source interface to two R packages.

Analysis of pooled data (estimation of prevalence) is achieved by interfacing with [PoolTestR](https://github.com/AngusMcLure/PoolTestR?tab=readme-ov-file#pooltestr). For more information about the package read see this article in [Environmental Modelling and Software](https://doi.org/10.1016/j.envsoft.2021.105158).

Design of surveys (sample size calculations, power calculations, optimisation of designs) is done by interfacing with [PoolPoweR](https://github.com/AngusMcLure/PoolTestR?tab=readme-ov-file#pooltestr). More details on this package will be published soon.

The [PoolTools interface](https://github.com/AngusMcLure/PoolTools) is powered by R shiny.


### Privacy

PoolTools does not save any data or results. Once the session has ended (i.e. the browser or tab has been closed), all data and results are cleared. Therefore, users are encouraged to save their results before exiting the browser/tab. 

PoolTools does not track any user data. Shinyapps.io collects general usage statistics but does not identify individual users.


## How-to cite PoolTools

General citation for PoolTools:
> Jaya, F., Ward, S., Mayfield, H., Cherryh, C., McLure, A., PoolTools [Computer software]. v0.1.5. https://github.com/AngusMcLure/PoolTools

If you used the "Analyse" mode, please cite:
> McLure, A., O'Neill, B., Mayfield, H., Lau, C. and McPherson, B., 2021. PoolTestR: An R package for estimating prevalence and regression modelling for molecular xenomonitoring and other applications with pooled samples. _Environmental Modelling & Software_, _145_, p.105158.

If you used the "Design" mode, please cite:
> McLure, A., & Jaya, F. PoolPoweR [Computer software]. https://github.com/AngusMcLure/PoolPoweR


## Contact

For questions, feature requests, and bug reports, please post an [issue on GitHub](https://github.com/AngusMcLure/PoolTools/issues).

Alternatively, contact Angus on angus[dot]mclure[at]anu[dot]edu[dot]au.


## Credits and acknowledgements

This work was funded by a seed grant from [ACE-NTDs](https://www.acentds.org/), an [NHMRC](https://nhmrc.gov.au/) Centre for Research Excellence.

We are very thankful for the survey respondents and workshop participants for helping to define the key requirements of the tool and providing feedback on the tool as it has evolved.


## Contributors 
[Frederick Jaya](https://orcid.org/0000-0002-4019-7026) - Creator, author, testing, and documentation  
[Selina Ward](https://orcid.org/0000-0002-7776-8419) - Testing and documentation  
[Helen Mayfield](https://orcid.org/0000-0003-3462-4324) - End-user engagement  
[Caitlin Cherryh](https://orcid.org/0000-0001-6146-4376) - Development, testing and documentation
[Angus McLure](https://orcid.org/0000-0003-2890-2703) - Project lead
