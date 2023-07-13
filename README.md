# CASPIANII
This is a fork for testing (and potentially documentation and developement) of CASPIAN II.

Things to remember: 
- Several functions are *paralellised*. Currently, _*half*_ of the cores available will be used to prevent overload of concurrent-use servers. You might want to change that in case of a single-user machine, or a dedicated node on a server. 
- *Paths* seem to be an issue. If your code does fail (and maybe not gracefully), check the function responsible for paths, check if the paths exist, and check if the error goes away with an _absolute path_ instead of a _relative path_.
- package _{raster}_ is going to be phased out. Some functions already use _{terra}_, but this needs to be thoroughly tested and adapted. If your run fails, check if _{sf}_ or _{terra}_ might be the solution.
- *Plotting* is sometimes an issue, especially when changeing platforms. If your run fails, _always_ remember to check if the plotting devices which failed are closed! Furthermore, check if you can use another plotting device. Check if the data you want to plot is suitable for the method you chose (e.g., choropleths with a single value in the var seem to fail non-gracefully). If all fails, save your data, transfer it to a different platform, and try plotting it there.
- Environments can be an issue. *Variable _names_* are often recycled inside functions. Since these use their own environment, check if you need to define the environment where your variable is called from (and maybe exported to). This is especially improtant for nested functions, which may end up in recursive calls. (See, e.g., [this discussion](https://stackoverflow.com/questions/4357101/promise-already-under-evaluation-recursive-default-argument-reference-or-earlie/49385368#49385368) on SO.)


Bucket list:

- Why are iNaturalist and OBIS-results reported as delivering no records in cases when they _clearly_ have data?
- Create a (rarefied) layer with (main) transport infrastructure for plotting.
- Create a (rarefied) layer with Copernicus / Sentinel INDEX data ([Imperviousness Density, Impervious Build-up, Imperviousness Change Classification? ](https://land.copernicus.eu/pan-european/high-resolution-layers/imperviousness/status-maps)) for plotting.
- Could including FloraWeb, VegetWeb, Re:survey, SMon data improve the model results? 
- Would  IMD and IMCC time series inclusion  be possible to improve the model results?
- Would including [Climate Change Velocity](https://www.nature.com/articles/s41598-019-38720-6) be possible to improve the model results?



