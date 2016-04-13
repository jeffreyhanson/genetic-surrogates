Global Rural-Urban Mapping Project, Version 1 (GRUMPv1): Population Density Grid


Contents:

DESCRIPTION
CITATION
DATA FORMAT
ATTRIBUTE FIELDS
DATA RESTRICTIONS
DATA ERRORS, CORRECTIONS AND QUALITY ASSESSMENT
NO WARRANTY OR LIABILITY


DESCRIPTION

This zip file contains data from Center for International Earth Science Information 
Network (CIESIN) and colleagues' (see "citation", below) Global Rural-Urban Mapping 
Project, Version 1 (GRUMPv1): Population Density Grid data product.  
Documentation is available on the GRUMP web site at:
http://sedac.ciesin.columbia.edu/data/dataset/grump-v1-population-density/docs

This is the release of version 1 of the GRUMP product. See the GRUMP home 
page for additional information on the product.  If you should discover any 
problems or errors, please inform us at:

grump@ciesin.columbia.edu


CITATION

We recommend the following for citing the database:

Center for International Earth Science Information Network (CIESIN), 
Columbia University; International Food Policy Research Institute (IFPRI); 
the World Bank; and Centro Internacional de Agricultura Tropical (CIAT). 2011. 
Global Rural-Urban Mapping Project, Version 1 (GRUMPv1): Population Density Grid. 
Palisades, NY: Socioeconomic Data and Applications Center (SEDAC), Columbia University. 
Available at http://sedac.ciesin.columbia.edu/data/dataset/grump-v1-population-density. 
(Date of download)


DATA FORMAT

This archive contains the population densities for all three years in ESRI raster (GRID), 
Band Interleaved by Line (BIL), or ASCII (text) format. The raster data are at 0.00833 degrees 
(30 arc seconds) resolution and contain the following data:

udsxxg		population density in persons per square kilometer in year xx*, unadjusted
udsxxag		population density in persons per square kilometer in year xx*, adjusted to match UN country totals

GRID
------
The raster format files are an ESRI format that work best with ArcInfo 
or ArcGIS. These files are GRIDs, not vector coverage data. 

BIL
------
The BIL format is stores the data as binary files with associated 
information stored in world, header, and statistics files. The set 
of BIL files and what they contain are described below. The XML files
in the archive are used by ArcGIS to store statistics of the raster data.

.bil	Binary format data

.hdr	Header file with the following lines:
		Byte order (motorola or intel, M or I)
		Layout (BIL or Band Interleaved by Line)
		NROWS (number of rows)
		NCOLS (number of columns)
		NBANDS (number of bands)
		NBITS (number of bits -- 4, 8, 24, or 32)
		BANDROWBYTES (number of byes per band, per row)
		TOTALROWBYES (total byes of data per row)
		PIXELTYPE (FLOAT or INT)
                ULXMAP x-coordinate of the center of the upper-left cell
                ULYMAP y-coordinate of the center of the upper-left cell
		XDIM resolution in the x-dimension
		YDIM resolution in the y-dimension
                NODATA no data value

.stx	Statistics file with the follow items:
		Number of bands (always 1)
		Minimum value
		Maximum value
		Mean value
		Standard deviation

.prj	Coordinate system definition in well-known text.

ASCII
------
The ASCII format stores the data as a two-dimensional array of numbers
of the same size (rows and columns) as the raster dataset. The array is
preceded by a six-line header as follows:

ncols         number of columns
nrows         number of rows
xllcorner     x coordinate of the lower-left corner
yllcorner     y coordinate of the lower-left corner
cellsize      resolution (decimal degrees)
NODATA_value  No data value (-9999)

The file names are prefixed by the three letter ISO code for the country 
or a two letter code for the continent or globe.

The data are stored in geographic coordinates of decimal degrees based 
on the World Geodetic System spheroid of 1984 (WGS84).


ATTRIBUTE FIELDS

*uds grids
     VALUE	Represents population density in persons per square kilometers


DATA RESTRICTIONS

These data are for noncommercial use only. No third party distribution of all, 
or parts, of the electronic files is authorized. The data used in the creation of 
this dataset were provided to CIESIN by the organizations identified in the source data.


DATA ERRORS, CORRECTIONS AND QUALITY ASSESSMENT

CIESIN follows procedures designed to ensure that data disseminated by CIESIN 
are of reasonable quality. If, despite these procedures, users encounter 
apparent errors or misstatements in the data, please contact CIESIN 
Customer Services at 845/365-8920 or via Internet e-mail at 
ciesin.info@ciesin.columbia.edu. Neither CIESIN nor NASA verifies or guarantees 
the accuracy, reliability, or completeness of any data provided.


NO WARRANTY OR LIABILITY

CIESIN provides this data without any warranty of any kind whatsoever, either 
express or implied. CIESIN shall not be liable for incidental, consequential, 
or special damages arising out of the use of any data provided by CIESIN.