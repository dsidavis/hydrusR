This is a fork of the [hydrusR package](git@github.com:dsidavis/hydrusR.git).
We have added some functionality to manipulate the SELECTOR.IN file.

These additions are not entirely general. There are too many conditional
formats in the input files that depend on the values of one or more flags,
e.g. which model is being used, whether hysterisis is > 0.

The functionality in selector.R does provide somewhat general/abstract
facilities for setting scalar values, replacing matrices/data.frames,
setting rows, and setting individual columns.
One can use this to build additional  features for creating the input files.


We have also created the [RHydrus package]()
which allows the hydrus1D FORTRAN code to be called directly from R,
i.e. avoiding an executable.
We originally intended to avoid using the input files and setting 
the data and options directly from R. However, the FORTRAN code 
combines reading the input files with computations which makes
this problematic.

RHydrus  does however allow one to specify the file names of the input files
directly, rather than using the same file names in different directories
and using the LABEL_01.dir file to identify that directory.
  
