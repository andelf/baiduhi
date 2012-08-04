#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import urllib
import urllib2
import json
import socket
socket.setdefaulttimeout(12.0)
from erlport import Port, Protocol, String, Atom, BitBinary
from pyamf.remoting.client import RemotingService


__noah_password__ = os.path.join(os.path.dirname(__file__), 'noah_password')

MINUTE = 60
HOUR = 3600
DAY = 24 * HOUR

class Data(dict):
    def __init__(self, *args, **kwargs):
        super(Data, self).__init__(*args, **kwargs)
        self.__dict__ = self

__cache__ = dict()
def memorize(func):
    name = func.func_name
    def wrapper(*args, **kwargs):
        kwargs_tup = tuple(kwargs.items())
        if (name, args, kwargs_tup) in __cache__:
            return __cache__[(name, args, kwargs_tup)]
        ret = func(*args, **kwargs)
        __cache__[(name, args, kwargs_tup)] = ret
        return ret
    wrapper.name = name
    return wrapper

def timestamp():
    return int(time.time())


def mkAMFClient():
    import baidu
    uuap = baidu.UUAP.fromPasswordFile(__noah_password__)
    uuap.login()
    uuap.initNoah()
    uuap.registerGlobalOpener()
    url = "http://infoquery.noah.baidu.com/noah/index.php?r=GetMonitorData"
    refer = "http://noah.baidu.com/ui/swf/noah.swf?t=20100902"
    client = RemotingService(url)
    client.addHTTPHeader('Referer',  refer)
    client.addHTTPHeader('Connection', "close")
    client.addHTTPHeader('Cookie', uuap.getCookieForUrl(url))
    return client

#    p = client.getService('MonitorDataAccessor')
#    print p.getTrendData(['ai-video-bd3-1.ai01'], [28965], '01313664528', '01313668128')


def get_trend_data(hosts, monitor_items, start_time, end_time):
    return dataServ.getTrendData(hosts, [mon['item_id']], '0%d' %  (timestamp() - 2 * HOUR), '0%d' % (timestamp() ))

def noahUrlopen(url, data=None, method='GET', retPath=['data']):
    if method == 'GET':
        if isinstance(data, (dict, list, tuple)):
            query = urllib.urlencode(data)
            if '?' in url:
                url += ('&%s' % query)
            else:
                url += ('?%s' % query)
        req = urllib2.Request(url)
    else:                       # POST/ PUT
        if isinstance(data, (dict, list, tuple)):
            body = urllib.urlencode(data)
        elif isinstance(data, (str, unicode)):
            body = data
        req = urllib2.Request(url, data=body)
    resp = urllib2.urlopen(req)
    ret = json.load(resp, object_hook=Data)
    path = retPath[::-1]
    if 'success' not in ret or ret.success:
        while path:
            ret = ret[path.pop()]
        return ret

def buildUrl(route, seg='noah'):
    url = "http://noah.baidu.com/%s/?r=%s" % (seg, route)
    return url

def noahRequest(controller, action, **params):
    url = "http://noah.baidu.com/noah/?r=%s/%s" % (controller, action)
    if params:
        query = urllib.urlencode(params)
        url = '%s&%s' % (url, query)
    req = urllib2.Request(url)
    res = urllib2.urlopen(req)
    return res

# APIs
@memorize
def getChildrenNodes(node_id):
    url = buildUrl('Tree/Info/GetChildrenNode', seg='olive')
    return noahUrlopen(url, dict(treeId=1, nodeId=node_id), retPath=[])

def searchNodes(path):
    url = buildUrl('Apply/Privilege/SearchNodes', seg='olive')
    # 找不到的时候会返回<未找到>, 是中文
    ret = noahUrlopen(url, dict(key=path, type='node'))
    ret = filter(lambda n: not n.startswith('<'), ret)
    return ret

@memorize
def searchMonitorItems(node_id, query):
    url = buildUrl('monitorItem/ListDataTypeItem')
    return noahUrlopen(url, dict(token=query,
                                 node_id=node_id), retPath=[])

# Inherit custom protocol from erlport.Protocol
class NoahProtocol(Protocol):

    def initialize(self):
        self.amfclient = client = mkAMFClient()
        self.dataSrv = client.getService('MonitorDataAccessor')

    def handle_lookup_monitor_item(self, node_id, name):
        name = String(name)
        resp = searchMonitorItems(node_id, name)
        for item in resp:
            if str(item['item_name']) == name:
                return (item['item_name'],
                        int(item['item_id']))
        return None

    def handle_search_nodes(self, query):
        query = String(query)
        resp = searchNodes(query)
        return resp or None

    def handle_lookup_node(self, path):
        path = String(path)
        nodes = searchNodes(path)
        if len(nodes) != 1:
            return u"can't find path or path name ambiguous"
        node_name = nodes[0]
        current_id = 1
        target = None
        leafs = getChildrenNodes(current_id)
        while target is None:
            for leaf in leafs:
                if node_name == leaf.path:
                    target = leaf
                    break
                if node_name.startswith(leaf.path):
                    leafs = getChildrenNodes(leaf.id)
                    break
            if len(leafs) == 0:
                target = u"can't find path"
        return (target.path, int(target.id))

    def handle_exit(self, Reason):
        raise SystemExit

    # Function handle_NAME will be called for incoming tuple {NAME, ...}
    def handle_hello(self, hosts, monitor_items, start_time, end_time):
        # String wrapper forces name to be a string instead of a list
        return "Hello, %s" % String(str(hosts))

if __name__ == "__main__":
    proto = NoahProtocol()
    proto.initialize()
    #print getChildrenNodes(1)
    #print searchNodes("BAIDU_VS_VSFL_VIDEO_FRONTLEVEL_SE_bs_db")
    # print proto.handle_lookup_node(u"BAIDU_VS_VSFL_VIDEO")
    # print proto.handle_search_nodes(u"appac")
    # print proto.handle_lookup_node(u"BAIDU_VS_VSFL_VIDEO_FRONTLEVEL_SE_bs_db")
    # print proto.handle_lookup_node(u"BAIDU_VS_VSFL_VIDEO_FRONTLEVEL_SE_bs_db")
    #print proto.handle_lookup_node(u"BAIDU_VS_VSFL_VIDEO_FRONTLEVEL_SE_bs_db")
    #print proto.handle_lookup_monitor_item(15072, u"as_pv")
    # Run protocol with port open on STDIO
    proto.run(Port(use_stdio=True))
