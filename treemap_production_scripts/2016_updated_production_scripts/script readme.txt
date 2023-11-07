Treemap script readme:

Assinging the zone number:
There are two conventions, cur.zone and cur.zone.zero. cur.zone you'd input as "z8", "z9", "z10", etc. cur.zone.zero has a leading zero, so:"z08", "z09", "z10"

To run the script, you need 4 sets of inputs. I've commented the script where they appear. 
Needed files: 
1) Tifs of raster bands
2) X table with location in meters
3) X table with year vintage 
4) EVG remap files (crosswalk between EVG numbers and factor levels (1,2,3,4,5 etc))

At the beginning, you want to assign the number of cores that you'll be using on your machine (the 'ncores' variable)

You'll need to go through and change the 'setwd' commands to work with your files. I could have used the 'here' convention but was lazy.


