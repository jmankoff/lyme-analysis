from shapely.geometry import Polygon, Point
from fastkml import kml
import os.path

## USEFUL TOOL: http://www.gmapgis.com/

def loadPolygonsFromKML(fpath):
    listOfPolygons = []
    doc = file(fpath).read()
    k = kml.KML()
    k.from_string(doc)
    generators = [f.features() for f in k.features()]
    row = 0 # any not None value
    for g in generators:
        while row is not None:
            try:
                row = g.next()
            except StopIteration:
                row = None
                pass
            if row is not None:
                listOfPolygons.append(row.geometry)
    return listOfPolygons

def loadPolygonsFromKMLList(dpath, fnamelist):

    PolysFromFilesDict = {}
    for fname in fnamelist:
        if ".kml" not in fname:
            fnamewithext = fname+".kml"
        else:
            fnamewithext = fname
        fpath = os.path.join(dpath, fnamewithext)
        print ("loading "+str(fpath))
        listOfPolygons = loadPolygonsFromKML(fpath)
        PolysFromFilesDict[fname] = listOfPolygons
    return PolysFromFilesDict

def isPtInAnyPolysInList(pt, listOfPolys):
    isInsideAny = False
    for poly in listOfPolys:
        isInside = pt.intersects(poly)
        if isInside is True:
            isInsideAny = True
            break
    return isInsideAny

def getPointLabel(pt, PolysFromFilesDict, orderedFnamesForElifs, orderedLabels):
    pt = Point(pt) # long, lat
    finalLabel = 100
    for ofi in range(0, len(orderedFnamesForElifs)):
        ofname = orderedFnamesForElifs[ofi]
        listOfPolys = PolysFromFilesDict[ofname]
        isInsideAny = isPtInAnyPolysInList(pt, listOfPolys)
        if isInsideAny is True:
            finalLabel = orderedLabels[ofi]
            break # break is very important here
    return finalLabel


# orderedFnamesForElifs = ["greekparty2"]
# orderedLabels = [1]
# orderedFnamesForElifs = ["greekparty2", "greekparty3", "greekhouses", "apartments", "halls", "athletic", "greens", "campus_outer"]
# orderedLabels = [1, 2, 3, 4, 5, 6, 7, 8]
# PolysFromFilesDict = loadPolygonsFromKMLList("../map_polygons/", orderedFnamesForElifs)
# pt = Point(tuple([-79.942776, 40.443279]))
# print getPointLabel(pt, PolysFromFilesDict, orderedFnamesForElifs, orderedLabels)
#
