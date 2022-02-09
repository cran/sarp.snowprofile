
# sarp.snowprofile 1.2.0

 * New functions 
   - to compute structural stability threshold sums/ lemons (i.e., `computeTSA`, `computeRTA`)
   - to flexibly search for specific or generic weak layers and other layers of interest in the profiles (see, `findPWL`)
   - new and improved functionality in `plot.snowprofile` and `plot.snowprofileSet`

 * Restructured snowprofile object classes to better cope with top-down measured manual profiles
 that potentially have unknown total snow heights.
   - Main change: Insert mandatory snowprofile field `maxObservedDepth` and precisely distinguish that from `hs`.
   - The existing constructors from snowprofile and snowprofileLayers are set up to auto-fill the fields if possible.
   - Highlight profiles with unknown hs in `plot.snowprofile`, add `TopDown = "auto"`
   - include strategy to handle unobserved basal layers
   
  * Speed up several bottlenecks that slowed down long computations


# sarp.snowprofile 1.1.0

 * v1.1.0 contains a variety of minor improvements listed below. The version should be free of major bugs, 
 but several improvements are to be done in future versions.

 * Improvements of `snowprofileCaaml`:
   - Make routine compatible with caaml files from snowpilot.org 
   (fix bugs related to bottom up profiles and workaround namespace issue)
   - Originally compatible with caaml files from niviz.org
   - Convert undefined grain types
   
 * Handle grain types that are undefined in package with `validate_snowprofile` and `reformat_snowprofile`
   
 * Enable plotting of a temperature profile that is independent of the snow layer grid
 
 * `writeSmet` routine for exporting smet files
 
 * Add object classes for `snowprofileTests` and `snowprofileInstabilitySigns`
 
 * Add new read routine for advanced csv files containing a comprehensive set of profile information, see `?snowprofileCsv_advanced`.
 
 * Add few new field names to layer structure.
 
 * A `deriveDatetag` function calculates `bdate`s and `datetag`s for snowprofile layers.
 
 * A `simplifyGtypes` function trims unsupported IACS gtype subclasses into their supported main classes (e.g., PPsd --> PP)
 

# sarp.snowprofile 1.0.0

 * initial release of `sarp.snowprofile`
