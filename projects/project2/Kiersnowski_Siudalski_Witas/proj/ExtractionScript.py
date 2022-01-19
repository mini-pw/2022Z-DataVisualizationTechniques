#!/usr/bin/env python
# coding: utf-8


import sys
import copy
import json
import pandas as pd


def sqdistance(X1, Y1, X2, Y2):
    return (X1 - X2) ** 2 + (Y1 - Y2) ** 2


def locationToLongLat(location):
    return location['longitudeE7'], location['latitudeE7']


def durationToTimeStampsMS(duration):
    return duration['startTimestampMs'], duration['endTimestampMs']


def niceASDict(ActivitySegmentObj):
    t = {}

    t['StartingLongitude'], t['StartingLatitude'] = locationToLongLat(ActivitySegmentObj['startLocation'])
    t['EndingLongitude'], t['EndingLatitude'] = locationToLongLat(ActivitySegmentObj['endLocation'])
    t['StartingtimeStampInMS'], t['EndtimeStampInMS'] = durationToTimeStampsMS(ActivitySegmentObj['duration'])
    t['ActivityType'] = ActivitySegmentObj['activityType']
    t['Distance'] = ActivitySegmentObj['distance']
    t['User'] = ActivitySegmentObj['User']

    return t


def addNiceDictToBigNiceDict(bigDict, smallDict):
    ND = copy.deepcopy(bigDict)
    for key in bigDict.keys():
        if key in ND.keys():
            ND[key].append(smallDict[key])
        else:
            raise Exception('keys in bigDict and smallDict dont match')
    return ND


def points(Object):
    returnDict = {key: [] for key in POINTS_KEYS}
    keys = Object.keys()
    if 'simplifiedRawPath' in keys:
        for point in Object['simplifiedRawPath']['points']:
            returnDict['Longitude'].append(point['lngE7'])
            returnDict['Latitude'].append(point['latE7'])
            returnDict['TimeStampInMS'].append(point['timestampMs'])
            returnDict['User'].append(Object['User'])
    if 'waypointPath' in keys:
        pointnum = len(Object['waypointPath']['waypoints'])
        idx = .5
        for point in Object['waypointPath']['waypoints']:
            returnDict['Longitude'].append(point['lngE7'])
            returnDict['Latitude'].append(point['latE7'])
            returnDict['TimeStampInMS'].append((
                int(Object['duration']['startTimestampMs']) * (pointnum - idx)
                + int(Object['duration']['endTimestampMs']) * idx)/pointnum)
            returnDict['User'].append(Object['User'])
            idx+=1
    if "startLocation" in keys:
        returnDict['Longitude'].append(Object['startLocation']['longitudeE7'])
        returnDict['Latitude'].append(Object['startLocation']['latitudeE7'])
        returnDict['TimeStampInMS'].append(Object['duration']['startTimestampMs'])
        returnDict['User'].append(Object['User'])
        returnDict['Longitude'].append(Object['endLocation']['longitudeE7'])
        returnDict['Latitude'].append(Object['endLocation']['latitudeE7'])
        returnDict['TimeStampInMS'].append(Object['duration']['endTimestampMs'])
        returnDict['User'].append(Object['User'])
    else:
        returnDict['Longitude'].append(Object['location']['longitudeE7'])
        returnDict['Latitude'].append(Object['location']['latitudeE7'])
        returnDict['TimeStampInMS'].append(Object['duration']['startTimestampMs'])
        returnDict['User'].append(Object['User'])
    return returnDict


def concatBigDicts(dict1, dict2):
    newDict = {key: [] for key in dict1.keys()}
    for key in newDict.keys():
        try:
            newDict[key] = dict1[key] + dict2[key]
        except KeyError:
            raise Exception('Keys in dicts dont match')
    return newDict


def nicePVDict(PlaceVisitObj):
    d = {}
    d['Longitude'], d['Latitude'] = locationToLongLat(PlaceVisitObj['location'])
    d['PlaceId'] = PlaceVisitObj['location']['placeId']
    d['Name'] = PlaceVisitObj['location']['name']
    d['StartTimeStampInMS'], d['EndTimeStampInMS'] = durationToTimeStampsMS(PlaceVisitObj['duration'])
    d['User'] = PlaceVisitObj['User']
    return d


AS_KEYS = ['StartingLongitude',
           'StartingLatitude',
           'EndingLongitude',
           'EndingLatitude',
           'StartingtimeStampInMS',
           'EndtimeStampInMS',
           'ActivityType',
           'Distance',
           'User']  # kolumny w as.csv

POINTS_KEYS = ['Longitude',
               'Latitude',
               'TimeStampInMS',
               'User']

PV_KEYS = ['PlaceId',
           'Longitude',
           'Latitude',
           'StartTimeStampInMS',
           'EndTimeStampInMS',
           'Name',
           'User']

if __name__ == '__main__':
    if len(sys.argv) < 4:
        raise Exception('too few args')

    targetloc = sys.argv[1]
    CharacteristicFileName = sys.argv[2]
    JSONlocs = sys.argv[3:]
    AS, PV = [], []

    for JSONloc in JSONlocs:
        User = JSONloc.split('/')[-1][:5]
        with open(JSONloc, encoding='utf-8') as data:
            load = json.load(data)
        for timelineObject in load['timelineObjects']:
            if 'activitySegment' in timelineObject.keys():
                AS.append(timelineObject['activitySegment'])
                AS[len(AS) - 1]['User'] = User
            else:
                PV.append(timelineObject['placeVisit'])
                PV[len(PV) - 1]['User'] = User

    AS = [ASobj for ASobj in AS if ASobj['startLocation'] != {} and 'distance' in ASobj.keys()]
    AS = [ASobj for ASobj in AS if int(ASobj['duration']['startTimestampMs']) >= 1639782000000]
    PV = [PVobj for PVobj in PV if int(PVobj['duration']['startTimestampMs']) >= 1639782000000]
    PointsDict = {key: [] for key in POINTS_KEYS}
    ASDict = {key: [] for key in AS_KEYS}
    for ASObject in AS:
        ASDict = addNiceDictToBigNiceDict(ASDict, niceASDict(ASObject))
        PointsDict = concatBigDicts(PointsDict, points(ASObject))
    ASdf = pd.DataFrame(ASDict)
    ASdf.to_csv(targetloc + '/ActivitySegment' + CharacteristicFileName + '.csv')

    PVDict = {key: [] for key in PV_KEYS}
    for PVObject in PV:
        PVDict = addNiceDictToBigNiceDict(PVDict, nicePVDict(PVObject))
        PointsDict = concatBigDicts(PointsDict, points(PVObject))
    PVdf = pd.DataFrame(PVDict)
    PVdf.to_csv(targetloc + '/PlacesVisited' + CharacteristicFileName + '.csv')

    Pointsdf = pd.DataFrame(PointsDict)
    Pointsdf.to_csv(targetloc + '/Points' + CharacteristicFileName + '.csv')
