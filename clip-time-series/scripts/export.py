import shutil
import os
from pathlib import Path
import time
from glob import glob

import pandas as pd
import ee 
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt
import rasterio as rio
from rasterio.mask import mask
from rasterio.merge import merge
from rasterio.profiles import DefaultGTiffProfile
import numpy as np
from shapely.geometry import box
from PyPDF2 import PdfFileMerger, PdfFileReader
from sepal_ui import gdal as sgdal


from utils import gdrive
from utils import utils
from utils import parameters as pm

ee.Initialize()

def getNDVI(sources, satellite, bands, mask, year):
    
    start = str(year) + '-01-01'
    end = str(year) + '-12-31'
    
    #select the images from the appropriate satellite
    image = ee.ImageCollection(pm.getSatellites(sources)[satellite]) \
            .filterDate(start, end) \
            .filterBounds(mask) \
            .map(pm.getCloudMask(satellite)) \
            .mosaic()
    
    nir = image.select(pm.getAvailableBands()['ndvi'][satellite][0])
    red = image.select(pm.getAvailableBands()['ndvi'][satellite][1])
    
    ndvi = nir.subtract(red).divide(nir.add(red)).rename('NDVI')
    
    reducer = ndvi.reduceRegion(**{
        'reducer': ee.Reducer.mean(),
        'geometry': mask,
        'scale': 30
    }).getInfo()
    
    return reducer['NDVI']
    
def getNDWI(sources, satellite, bands, mask, year):
    
    start = str(year) + '-01-01'
    end = str(year) + '-12-31'
    
    #select the images from the appropriate satellite
    image = ee.ImageCollection(pm.getSatellites(sources)[satellite]) \
            .filterDate(start, end) \
            .filterBounds(mask) \
            .map(pm.getCloudMask(satellite)) \
            .mosaic()
    
    nir = image.select(pm.getAvailableBands()['ndwi'][satellite][0])
    swir = image.select(pm.getAvailableBands()['ndwi'][satellite][1])
    
    ndwi = nir.subtract(swir).divide(nir.add(swir)).rename('NDWI')
    
    reducer = ndwi.reduceRegion(**{
        'reducer': ee.Reducer.mean(),
        'geometry': mask,
        'scale': 30
    }).getInfo()
    
    return reducer['NDWI']
    

def getImage(sources, bands, mask, year):
    
    start = str(year) + '-01-01'
    end = str(year) + '-12-31'
    
    #priority selector for satellites
    for satelliteId in pm.getSatellites(sources):
        dataset = ee.ImageCollection(pm.getSatellites(sources)[satelliteId]) \
            .filterDate(start, end) \
            .filterBounds(mask) \
            .map(pm.getCloudMask(satelliteId))
        
        if dataset.size().getInfo() > 0:
            satellite = satelliteId
            break
            
    clip = dataset.median().clip(mask).select(pm.getAvailableBands()[bands][satelliteId])
    
    return (clip, satelliteId)
    

def run(file, pts, bands, sources, output):
    
    #get the filename
    filename = Path(file).stem
    
    #extract the bands to use them in names 
    name_bands = '_'.join(bands.split(', '))
    
    #pdf name 
    pdf_file = pm.getResultDir() + '{}_{}.pdf'.format(filename, name_bands)
    
    if os.path.isfile(pdf_file):
        output.add_live_msg('Pdf already exist', 'success')
        return pdf_file
    
    #start the drive handler 
    drive_handler = gdrive.gdrive()
    
    #transform them in ee points 
    ee_pts = {pts.iloc[i].name: ee.Geometry.Point(pts.iloc[i]['lng'], pts.iloc[i]['lat']) for i in range(len(pts))}
    
    #create the buffers 
    ee_buffers = {i: ee_pts[i].buffer(2000).bounds() for i in ee_pts}
    
    #create a multipolygon mask 
    ee_multiPolygon = ee.Geometry.MultiPolygon([ee_buffers[i] for i in ee_buffers]).dissolve(maxError=100)  
    
    #create intelligent cliping multipolygons
    buffers = [ee_pts[i].buffer(10000).bounds() for i in ee_pts]
    geometries = ee_multiPolygon.geometries()
    ee_polygons = [geometries.get(i) for i in range(geometries.length().getInfo())]         
            
    #create a filename list 
    descriptions = {}
    for year in range(pm.start_year, pm.end_year + 1):
        descriptions[year] = '{}_{}_{}'.format(filename, name_bands, year)
        
    #check if all the multipolygons have bee downloaded in gdrive
    
    
    #load all the data in gdrive 
    satellites = {} #contain the names of the used satellites
    task_list = []
    for year in range(pm.start_year, pm.end_year + 1):
            
        image, satellites[year] = getImage(sources, bands, ee_multiPolygon, year)
        
        for i, polygon in enumerate(ee_polygons):
            
            description = descriptions[year] + "_{}".format(i)
        
            task_config = {
                'image':image,
                'description': description,
                'scale': pm.getScale(satellites[year]),
                'region': ee.Geometry(polygon),
                'maxPixels': 10e12
            }
            
            task = ee.batch.Export.image.toDrive(**task_config)
            task.start()
            task_list.append(description)
            
        output.add_live_msg('exporting year: {}'.format(year))
    
    #check the exportation evolution     
    state = utils.custom_wait_for_completion(task_list, output)
    output.add_live_msg('Export to drive finished', 'success')
    time.sleep(2)
    
    output.add_live_msg('Retreive to sepal')
    #retreive all the file ids 
    filesId = []
    for description in task_list:
        filesId += drive_handler.get_files(description)
    
    #download the files   
    output.add_live_msg('Download files')
    drive_handler.download_files(filesId, pm.getTmpDir())  
    
    #remove the files from gdrive 
    output.add_live_msg('Remove from gdrive')
    drive_handler.delete_files(filesId)     
    
    #merge them into a single file per year
    output.add_live_msg('merge the files')
    for year in range(pm.start_year, pm.end_year + 1):
        files = [file for file in glob(pm.getTmpDir() + descriptions[year] + '*.tif')]
        io = sgdal.merge(files, out_filename=pm.getTmpDir() + descriptions[year] + '.tif', v=True, output=output, co="COMPRESS=LZW")
    
    pdf_tmps = []
    for index, row in pts.iterrows():
        
        pdf_tmp = pm.getTmpDir() + '{}_{}_tmp_pts_{}.pdf'.format(filename, name_bands, row['id'])
        pdf_tmps.append(pdf_tmp)
    
        #create the resulting pdf
        with PdfPages(pdf_tmp) as pdf:        
            
            page_title = "Pt_{} (lat:{:.5f}, lng:{:.5f})".format(
                int(row['id']), 
                row['lat'], 
                row['lng']
            )
            
            output.add_live_msg('Creating page for pt {}'.format(int(row['id'])))
                  
            fig, axes = plt.subplots(pm.nb_line, pm.nb_col, figsize=(11.69,8.27), dpi=500)
            fig.suptitle(page_title, fontsize=16, fontweight ="bold")
            fig.set_tight_layout(True) 
            #display the images in a fig and export it as a pdf page
            for year in range(pm.start_year, pm.end_year + 1):
                
                #laod the file 
                file = pm.getTmpDir() + descriptions[year] + '.tif'
                
                #extract the buffer bounds 
                coords = ee_buffers[index].coordinates().get(0).getInfo()
                bl, tr = coords[0], coords[2]

                # Create the bounding box
                shape = box(bl[0], bl[1], tr[0], tr[1])
            
                with rio.open(file) as f:
                    data, _ = mask(f, [shape], crop=True, all_touched=True)
                
                bands = [] 
                for i in range(3):
                    band = data[i]
                    #remove the NaN from the analysis
                    h_, bin_ = np.histogram(band[np.isfinite(band)].flatten(), 3000, density=True) 
    
                    cdf = h_.cumsum() # cumulative distribution function
                    cdf = 3000 * cdf / cdf[-1] # normalize
    
                    # use linear interpolation of cdf to find new pixel values
                    band_equalized = np.interp(band.flatten(), bin_[:-1], cdf)
                    band_equalized = band_equalized.reshape(band.shape)
        
                    bands.append(band_equalized)
    
                data = np.stack( bands, axis=0 )

                data = data/3000
                data = data.clip(0, 1)
                data = np.transpose(data,[1,2,0])
            
                i = year - pm.start_year
                ax = axes[pm.getPositionPdf(i)[0], pm.getPositionPdf(i)[1]]
                ax.imshow(data, interpolation='nearest')
                ax.set_title(str(year) + ' ' + pm.getShortname(satellites[year]), x=.0, y=.9, fontsize='small', backgroundcolor='white', ha='left')
                ax.axis('off')
                ax.set_aspect('equal', 'box')            
            
            #finish the line with empty plots 
            start = pm.end_year - pm.start_year
            for i in range(5-(start+1)%5):
                index = start + 1 + i
                ax = axes[pm.getPositionPdf(index)[0], pm.getPositionPdf(index)[1]]
                ax.axis('off')
                ax.set_aspect('equal', 'box')
            
            #save the page
            pdf.savefig(fig)
            plt.close('all')
            
    #flush the tmp repository 
    shutil.rmtree(pm.getTmpDir())
    
    #merge all the pdf files 
    mergedObject = PdfFileMerger()
    for pdf in pdf_tmps:
        mergedObject.append(PdfFileReader(pdf, 'rb'))
        os.remove(pdf)
    mergedObject.write(pdf_file)
    
    output.add_live_msg('PDF output finished', 'success')
    
    return pdf_file
    
    
    