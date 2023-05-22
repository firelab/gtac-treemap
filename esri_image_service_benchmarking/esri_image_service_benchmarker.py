import os,sys,requests,numpy,time
import urllib.request
def getRandomN(n,min,max):return numpy.sort((max-min)*numpy.random.random_sample(n)+min)

# image_services = {'MosaicMRF_Albers':'https://ntcfsxopsx1284:6443/arcgis/rest/services/Utilities2/TM2016_CONUS_StandHt/ImageServer',
# 'MRF_Albers':'https://ntcfsxopsx1284:6443/arcgis/rest/services/Utilities2/TreeMap2016_CONUS_StandHt_mrf/ImageServer',
# 'MosaicMRF_WebMercator':'https://ntcfsxopsx1284:6443/arcgis/rest/services/Utilities2/TM2016_CONUS_StandHt_WMmosaic/ImageServer',
# 'MRF_WebMercator':'https://ntcfsxopsx1284:6443/arcgis/rest/services/Utilities2/TreeMap2016_CONUS_StandHt_WM_mrf/ImageServer'
# }
image_services = {'LCMS_Albers':'https://apps.fs.usda.gov/fsgisx01/rest/services/RDW_LandscapeAndWildlife/LCMS_CONUS_Most_Recent_Year_Of_Fast_Loss/ImageServer',
                  'LCMS_WM':'https://apps.fs.usda.gov/fsgisx01/rest/services/RDW_LandscapeAndWildlife/LCMS_CONUS_Annual_Landcover/ImageServer'}

service_bounds = [-1.42463587702E7,2604056.207800001,-7264068.770199999,6736796.207800001]

projections = {
    'Albers':102008,
    'WM':3857
}
nTests = 500
exportImage_output_folder = r'X:\03_Outputs\03_Image_Service_Benchmarking\exportImage'
exportImageTable = os.path.join(exportImage_output_folder,'ExportImage_Benchmarking_Table_{}.csv'.format(nTests))

def getBenchmarkTable(overwrite=False):
    if not os.path.exists(exportImage_output_folder):os.makedirs(exportImage_output_folder)

    if not os.path.exists(exportImageTable) or overwrite:
        out_table = 'Iteration,Bbox'
        for service_name in list(image_services.keys()):
            for projection_name in projections.keys():
                out_table+=',{} Projection-{} Time'.format(service_name,projection_name)
        out_table+='\n'

        for i in range(1,nTests+1):
            xs = getRandomN(2,service_bounds[0],service_bounds[2])
            ys = getRandomN(2,service_bounds[1],service_bounds[3])
            bbox = [str(i) for i in [xs[0],ys[0],xs[1],ys[1]]]
            bbox_comma_delimited = ','.join(bbox)
            bbox_underscore_delimited = '_'.join(bbox)
            # print(bbox)
            time_list = []
            for service_name in list(image_services.keys()):
                service_path = image_services[service_name]
                
                for projection_name in projections.keys():
                    projection_code = projections[projection_name]
                    

                    exportImageCall = '{}/exportImage?f=image&bbox={}&imageSR={}&bboxSR=102100'.format(service_path,bbox_comma_delimited,projection_code)
                    exportImageName = os.path.join(exportImage_output_folder,'iteration{}_{}_callProj{}.png'.format(i,service_name,projection_name))
                    print(exportImageCall)
                    print(exportImageName)
                    startTime = time.time()
                    urllib.request.urlretrieve(exportImageCall, exportImageName)
                    endTime = time.time()
                    time_list.append(str(endTime-startTime))
            out_table+= '{},{},{}\n'.format(i,bbox_underscore_delimited,','.join(time_list))
            
            print(out_table)
            o = open(exportImageTable,'w')
            o.write(out_table)
            o.close()
# https://ntcfsxopsx1284:6443/arcgis/rest/services/Utilities2/TM2016_CONUS_StandHt/ImageServer/exportImage?f=image&bbox=-13575296.172956001,5343579.411157997,-10691479.969813637,6126294.5807979945&imageSR=102100&bboxSR=102100&size=1179,320