__author__ = 'Raghavendrai'
import json
import sys

from httplib2 import Http

address = 'http://localhost:5000'

# GET AUTH CODE
client_url = address + "/clientOAuth"
print("Visit %s in your browser" % client_url)
auth_code = "4/OEENCzapLmo0pfWkJ6ae5mboABZH8L8COe0SD98jzG4"
while auth_code == "":
    auth_code = ""

# TEST ONE GET TOKEN
try:
    h = Http()
    url = address + "/oauth/google"
    data = dict(auth_code=auth_code)
    data = json.dumps(data)
    resp, content = h.request(url, 'POST', body=data, headers={"Content-Type": "application/json"})
    if resp['status'] != '200':
        raise Exception('Received an unsuccessful status code of %s' % resp['status'])
    new_content = json.loads(content.decode())
    if not new_content['token']:
        raise Exception('No Token Received!')
    token = new_content['token']
except Exception as err:
    print("Test 1 FAILED: Could not exchange auth code for a token")
    print(err.args)
    sys.exit()
else:
    print("Test 1 PASS: Succesfully obtained token! ")


    # ADD TO DB WITH TOKEN

    # READ FROM DB WITH TOKEN