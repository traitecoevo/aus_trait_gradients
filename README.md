# Paper about the Austraits compilation

This is the R project used to analyse the data and produce figures for the manuscript titled "Revisiting the role of mean annual precipitation in shaping functional trait distributions at a continental scale".

Running the analysis pre-packaged

To run the analysis, run the file Aus_traits_gradients.Rproj. Then, open code.Rmd to view the script, run the analysis and produce figures and tibles.

The script is mostly self-contained, and will reproduce the results in the manuscript because the traits and climate data are stored as .RDS files, which are small in size and can be transferred easily. Moreover, the austraits package allows Austraits to be downloaded easily for use.

The only thing that needs to be done manually is to include the Bioclim raster layers into the folder climate_data/wc2.1_30s_bio to ensure that Figure 1 and certain supplementary figures can run.

Modifying the analysis

However, if you do want to modify the analysis, it may be necessary to download a number of files:

- Trait data is obtained from Austraits: this data is automatically downloaded when running the script code.Rmd using the `austraits` package using the function austraits::load_austraits(version = "4.1.0").

- Climate data must also be downloaded due the large file sizes, but cannot be downloaded via R.
Information about where to access climate files is available in the code.Rmd but is also detailed here.
	-The standard Bioclim variables are availalbe here: (https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip). This link directly downloads the zip file containing all 19 variables at 30sec resolution. Unzip the downloaded folder, then drag the files into the empty folder climate_data/wc2.1_30s_bio
	
	-The ENVIREM variables are available here (https://deepblue.lib.umich.edu/data/concern/data_sets/gt54kn05f). Download the .zip folder titled Australia_current_30arcsec_geotiff_set1.zip then drag the following file: current_30arcsec_annualPET.tif, into the empty folder climate_data/Envirem
	
	 
	-The CHELSA monthly VPD climatologies (1981-2010) are available here: https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2F. Navigate to climatologies/1981-2010/vpd/ and download all contained files. Then drag the files into the empty folder climate_data/VPD_Chelsa. 
	
	-The boundary file for Australia is downloaded from here: https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files, under `Australia - 2021 - Shapefile`. 
	
	
Contents:
-Aus_traits_gradients.Rproj: Project file for R projects

-code.Rmd: Prepares data, runs analysis, produces figures and tables	

-data/: contains data for analysis including austraits data, climate data, as well as prepared spreadsheets for use in analysis

 	-austraits\ : Contains trait data, is downloaded at start of analysis
	-climate_data\:  Contains climate data and climate_data.RDS
	-erroneous_values\:
		-erroneous_values.csv: Spreadsheet including details about points identified to be erroneous in Austraits to be removed
	-leaf_compound_filter\: 
		-datasets_with_leaf_area.csv: A blank spreadsheet listing datasets that have leaf area and whether they have compound species, to be filled in with whether they measured leaf or leaflet area
		-datasets_with_leaf_area_edited.csv: A spreadsheet listing datasets that have leaf area, filled in with whether they have measured compound leaves at leaf or leaflet scale. 

-documents/: contains documents related to the present analysis. Not produced by R.

-output/: 
	-manuscript\ : Contains figures, tables, supps figures and supps tables for manuscript. Produced by R project. 	
	-traits_by_dataset\:  Contains output of comparison of each dataset trait data against global trait data to inspect for erroneous values. Produced by R but not used in manuscript.
	

The directory structure of the repository is as follows:	
├── %
├── Aus_trait_gradients.Rproj
├── DESCRIPTION
├── R
│   ├── climate_extraction_functions
│   │   └── load_climate_data.R
│   ├── error_checking_functions
│   │   └── error_checking_function.R
│   ├── ggpairs2.R
│   └── plotting_functions
│       ├── plot_climate.R
│       ├── plot_trait_by_dataset.R
│       ├── plot_whittaker_biomes.R
│       └── plotting_functions.R
├── README.md
├── code.Rmd
├── data
│   ├── AUS_2021_AUST_SHP_GDA2020
│   │   ├── AUS_2021_AUST_GDA2020.dbf
│   │   ├── AUS_2021_AUST_GDA2020.prj
│   │   ├── AUS_2021_AUST_GDA2020.shp
│   │   ├── AUS_2021_AUST_GDA2020.shx
│   │   └── AUS_2021_AUST_GDA2020.xml
│   ├── austraits
│   │   ├── austraits-4.1.0.rds
│   │   └── austraits.json
│   ├── australia.tif
│   ├── climate_data
│   │   ├── Envirem
│   │   │   ├── current_30arcsec_annualPET.tif
│   │   │   ├── current_30arcsec_aridityIndexThornthwaite.tif
│   │   │   ├── current_30arcsec_growingDegDays0.tif
│   │   │   └── current_30arcsec_growingDegDays5.tif
│   │   ├── VPD_Chelsa
│   │   │   ├── "CHELSA 1981-2010 monthly VPD layers"
│   │   └── wc2.1_30s_bio
│   │       ├── "bioclim raster layers 30s res"
│   ├── climate_data.RDS
│   ├── erroneous_values
│   │   └── erroneous_values.csv
│   ├── leaf_compound_filter
│   │   ├── datasets_with_leaf_area.csv
│   │   └── datasets_with_leaf_area_edited.csv
├── documents
│   ├── cover_letter_New_Phyt.docx
│   └── cover_letter_New_Phyt_submission.docx
└── output
    ├── manuscript
    └── traits_by_dataset	