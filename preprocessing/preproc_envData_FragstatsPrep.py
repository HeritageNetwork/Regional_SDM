#-------------------------------------------------------------------------------
# Name:        SDM_FragstatsPrep.py
# Purpose:     Provides functions used to prepare raster datasets for input into
#              into FRAGSTATS and to format FRAGSTATS output for further comparison
#              and processing.
# Author:      Molly Moore
# Created:     2016-08-25
# Updated:     2017-08-29
#
# To Do List/Future ideas:
#-------------------------------------------------------------------------------

# import packages
import os
import arcpy

# set environmental variables
arcpy.env.overwriteOutput = True

################################################################################
# Set variables and paths below
################################################################################
raster = [r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\Reclass_2011_Riparian120m']
zones = [r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\Delaware', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\EErie', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\Erie', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\Monongahala', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\Ohio', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\Ontario', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\Potomac', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\Susquehanna', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Landcover.gdb\HUC4\SWOntario']
zone_field = "FEATUREID"
outFol = [r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\DelawareTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\EErieTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\ErieTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\MonongahelaTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\OhioTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\OntarioTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\PotomacTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\SusquehannaTiffs2011_riparian', r'W:\Heritage\Heritage_Projects\1362 Pearlshell\SDM\Fragstats\SWOntarioTiffs2011_riparian']

inCSV = []
lu_table = []

################################################################################
# Define functions below
################################################################################

def raster_clip(raster, zones, zone_field, outFol):
    ''' function that clips raster by polygon feature class and names output tif
    files by the attribute contained in the zone_field. Output tif files will be
    written to an already existing folder, the path of which is contained in the
    outFol variable.'''

    # create list of zone attributes
    with arcpy.da.SearchCursor(zones, zone_field) as cursor:
        features = sorted({row[0] for row in cursor})

    # make feature layer of zone layer to prep for selections
    zone_layer = arcpy.MakeFeatureLayer_management(zones, "zone_lyr")

    # loop through zones, select each zone, clip raster to zone
    for feature in features:
        selection = arcpy.SelectLayerByAttribute_management(zone_layer, "NEW_SELECTION", '%s = %d'%(zone_field, feature))
        export = arcpy.CopyFeatures_management(selection, os.path.join("in_memory", "f" + str(feature)))
        desc = arcpy.Describe(export)
        extent = str(desc.extent)
        clipRaster = arcpy.Clip_management(raster, extent, os.path.join(outFol, str(feature) + ".tif"), export, "", "ClippingGeometry", "NO_MAINTAIN_EXTENT")
        arcpy.Delete_management("in_memory")

def create_batch(zones, zone_field, outFol):
    ''' creates import batch txt file that can be used to import several .tif
    files into Fragstats. Zones is the feature class used to determine zones,
    zone_field is the unique field used to determine zones, outFol is the folder
    path that contains the .tif files.'''

    # set textfile to folder name
    textfile = os.path.basename(outFol)

    # create list of zone attributes
    with arcpy.da.SearchCursor(zones, zone_field) as cursor:
        ids = sorted({str(row[0]) for row in cursor})

    # open textfile and write batch file in FRAGSTATS batch format
    f = open(textfile + ".fbt", "w+")
    for i in ids:
        f.write(os.path.join(outFol, i) + ".tif" + ", x, 999, x, x, 1, x, IDF_GeoTIFF\n")
    f.close()

def pivotFields(inCSV, lu_table):
    ''' creates temporary pivot table based on .csv files, renames fields, and
    joins fields to a lookup table that is already created.'''

    tableTEMP = arcpy.TableToTable_conversion(inCSV, "in_memory", "tableTEMP")

    # strip path from ID field to preserve original zone field to prepare for join
    try:
        arcpy.CalculateField_management(tableTEMP, "LID", '!LID!.rsplit(".",1)[0]', "PYTHON_9.3")
        arcpy.CalculateField_management(tableTEMP, "LID", '!LID!.rsplit("\\\\",1)[1]', "PYTHON_9.3")
    except:
        IndexError

    # create list of all double fields to capture all FRAGSTATS fields
    fields = [f.name for f in arcpy.ListFields(tableTEMP, "", "DOUBLE")]

    # pivot fields, add FRAGSTATS metric name to field name, and join to lookup table
    pivotFields = ["Agriculture", "Forest", "Urban"]
    for field in fields:
        fieldTable = os.path.join("in_memory", field)
        arcpy.PivotTable_management(tableTEMP, "LID", "TYPE", field, fieldTable)
        for pivot in pivotFields:
            fieldName = pivot+"_"+field
            arcpy.AlterField_management(fieldTable, pivot, fieldName, fieldName)
            arcpy.JoinField_management(lu_table, "LID", fieldTable, "LID", fieldName)

################################################################################
# Begin script below
################################################################################

frag_step = raw_input("Are you performing FRAGSTATS pre-processing or post-processing? (enter 'pre' or 'post'): ")

if frag_step.lower() == 'pre':
    if len(raster) > 1 and len(zones) == 1:
        for ras, fol in zip(raster, outFol):
            raster_clip(ras, zones, zone_field, fol)
            create_batch(zones, zone_field, outFol)
    elif len(raster) == 1 and len(zones) > 1:
        for zone, fol in zip(zones, outFol):
            raster_clip(raster, zone, zone_field, fol)
            create_batch(zone, zone_field, outFol)
    else:
        raster_clip(raster, zones, zone_field, outFol)
        create_batch(zones, zone_field, outFol)

elif frag_step.lower() == 'post':
    for csv in inCSV:
        pivotFields(csv, lu_table)