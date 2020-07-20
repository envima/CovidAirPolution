# SARS-CoV-2 infections in Germany are not correlated to local long- and short-term particulate matter concentrations

Scripts for compiling the baseline dataset, statistical analysis and figure compilation.

## Requirements
The [envimaR package](https://github.com/envima/envimaR) is used for setting up the working environment. You can install the package using 

```r
devtools::install_github("envima/envimaR")
```

Please adjust lines three to nine in the [000_setup.R](https://github.com/envima/CovidAirPolution/blob/master/src/functions/000_setup.R) script to your environment.


## Usage

1. [control_dataset_compilation_DE.R](https://github.com/envima/CovidAirPolution/blob/master/src/control_dataset_compilation_DE.R) handles the initial compilation of the baseline dataset.

1. [control_model_compilation_DE.R]( https://github.com/envima/CovidAirPolution/blob/master/src/control_model_compilation_DE.R) handles the statistical analysis.

1. [control_media_compilation_DE.R]( https://github.com/envima/CovidAirPolution/blob/master/src/control_media_compilation_DE.R) compiles the raw version of the figures and provides information for the statistical tables in the manuscript.

1. [publication_media_DE.Rmd]( https://github.com/envima/CovidAirPolution/blob/master/src/publication_media_DE.Rmd) compiles the final figures of the manuscript.

