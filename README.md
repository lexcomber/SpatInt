# Spatial interpolation: a review of methods and opportunities using new forms of data with coded illustrations
Coded examples of the different Spatial Interpolation approaches in R described in the paper

Alexis Comber<sup>1</sup>, Wen Zeng<sup>1</sup>,<sup>2</sup>

<sup>1</sup>School of Geography, University of Leeds, Leeds, LS2 9JT, UK. Email: a.comber@leeds.ac.uk

<sup>2</sup>Shandong University of Science and Technology, Qingdao, P.R. China. Email: w.zeng1@leeds.ac.uk, alvin_z@163.com

## Abstract 
This paper reviews different approaches to spatial interpolation of areal data. The review groups these into methods that use ancillary data to constrain or guide the interpolation (dasymetric, statistical, street weighted and point-based), and those do not but instead seek to develop and refine allocation procedures (area to point, pycnophylactic and areal weighting). Each of these approaches is illustrated using a coded spatial disaggregation case study. The review also considers the opportunities arising from the many new forms of data being generated by the everyday activities of citizens and businesses (for example, through social media check-ins, websites offering services, micro-blogging sites, social sensing, etc), and more active VGI activities, all supported by ubiquitous web- and GPS-enabled technologies. The case study is extended to use data from a property website to constrain the spatial disaggregation and the results showed that new forms of data can perform as well as traditional data. The results indicate opportunities afforded by the many new forms of data as ancillary information for spatial interpolation and for support spatial analysis more generally.

## Code 
The code and data used to illustrate a forthcoming submission to Geography Compass. You can download the `SpatInt.R` file (`.Rmd` file to come...perhaps!) and run this in R or RStudio. It loads the RData files and provides links to the R files which describe how the data were created and assembled. You may need to install some of the packages but the code checks and does this. The script will load up the data (and different forms of ancillary data) and illustrate each of spatial interpolation approaches described in the text. Please contact me if you have queries! Lex (a.comber@leeds.ac.uk)
