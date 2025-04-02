// Set which band to download
var band = 'srad';

// Filter that band from Daymet
var imageCollection = ee.ImageCollection("NASA/ORNL/DAYMET_V4").select(band);

// Set years to include (inclusive)
var startYear = 1981;
var stopYear = 2010;

// Set months to include (inclusive)
var startMonth = 1;
var stopMonth = 12;

// Set Year-Month-Day strings to filter image collection by
var startStr = '';
var stopStr = '';
var startStr = startYear.toString() + '-0' + startMonth.toString() + '-01';
var stopStr = stopYear.toString() + '-' + stopMonth.toString() + '-31';

// Filter image collection by those dates
var Col = imageCollection.filterDate(startStr, stopStr);

// Calculate pixel sums 
var sum = Col.sum();
var sum = sum.toDouble();

// Then divide by the number of years included to get annual sum
// Then divide by 365 to get daily average 
// (Note: Daymet has 365 days in all years, including leap years)
var mean = sum.divide((stopYear - startYear + 1)).divide(365);    

// Export the image    
Export.image.toDrive
({
  image: mean,
  description: 'srad_normal_' + startYear + 'to' + stopYear,
  folder: 'daymet',
  scale: 1000,
  maxPixels:1e12
});
