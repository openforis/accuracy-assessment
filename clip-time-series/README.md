# Clip time series

## About 

This module allows the user to download as a .pdf an auto generated time series from 2005 to 2020. 
Each year will be represented in a square of 2x2km around the point of interest using the band combination selected by the user. 

Colors are stretched using [histogram equalization](https://en.wikipedia.org/wiki/Histogram_equalization).

To produce this image the software will use the most recent satellite with a cloudless mosaic using the following priority order :
- Sentinel 2
- Landsat 8
- landsat 5
- landsat 7

The user can manually swithc of the usage of sentinel or landsat data. 

![full_app](./doc/img/full_app.png)

for more information about usage please read the [documentation](./doc/doc.md)

## contribute
to install the project on your SEPAL account 
```
$ git clone https://github.com/12rambau/gfc_wrapper_python.git
```

please retreive the develop branch where all our development live
```
$ git checkout --track origin/develop
```

please follow the contributing [guidelines](CONTRIBUTING.md).
