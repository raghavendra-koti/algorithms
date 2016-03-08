from __future__ import division
from time import sleep
import json

import httplib2


h = httplib2.Http()

url = 'http://localhost:5000/catalog'

req_per_minute = 200.0
interval = (60.0 / req_per_minute)


def SendRequests(url, req_per_minute):
    requests = 0
    while requests < req_per_minute:
        result = json.loads(h.request(url, 'GET')[1].decode())
        # result = h.request(url,'GET')[1]
        # print(result
        if result.get('error') is not None:
            print("Error #%s : %s" % (result.get('error'), result.get('data')))
            print("Hit rate limit. Waiting 5 seconds and trying again...")
            sleep(5)
            SendRequests(url, req_per_minute)
        else:
            print("Number of Requests: ", requests + 1)
            print(result)
            sleep(interval)
        requests += 1



print("Sending Requests...")
SendRequests(url, req_per_minute)
