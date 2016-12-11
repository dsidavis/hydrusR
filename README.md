#Some utility functions to run the HYDRUS-1D soil-water flow model through R (variably saturated flow through soils).
###NOTE: This package requires the open-source HYDRUS-1D software installed in your system, which can be downloaded from https://www.pc-progress.com/en/Default.aspx?H1d-downloads. 

hydrusR came out of my own need to run the soil-water flow model HYDRUS-1D (Simunek et al., 1988) for relatively longer periods than typically allowed (in one simulation) by the HYDRUS-1D software. For example, running simulations for a year and printing outputs at 15 minute intervals. Hydrus GUI currently allows only 250 print times while you can set up upto 1000 print times in the 'selector.in' if run from the command line. 

Model set-up utilities include the functions for writing out soil parameters, initial conditions, root distribution, and boundary conditions (top and bottom) to the main input file 'selector.in'. 
One can run the HYDRUS simulation right from within R.
The package also allows for reading the raw simulation output files into R in a more analysis-friendly formats.


