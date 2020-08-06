###################################################################
# IBM Confidential                                                #
#                                                                 #
# OCO Source Materials                                            #
#                                                                 #
# 5725-Y42                                                        #
#                                                                 #
# Copyright IBM Corp. 2015-2016                                   #
#                                                                 #
# The source code for this program is not published or otherwise  #
# divested of its trade secrets, irrespective of what has         #
# been deposited with the U.S. Copyright Office.                  #
###################################################################

# HTTP server that acts as gateway

from BaseHTTPServer import BaseHTTPRequestHandler,HTTPServer
from SocketServer import ThreadingMixIn
#from test import findCondPop
from imputer import imputer
from glmout import glmout
import threading
import argparse
import re
import cgi
import json
import pandas as pd

      
class LocalData(object):
 records = {}
 
class HTTPRequestHandler(BaseHTTPRequestHandler):
 #define copyright
 OCO = "\n\nLicensed Materials - Property of IBM\n\n(C) Copyright IBM Corp. 2015, 2016  All Rights Reserved\n\n US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.\n\n"
 def do_POST(self):
     print("I am in Post")
     patient_data = ""
     rs =""
     if None != re.search('/api/v1/addrecord/*', self.path):
         print("I am here 1")
         ctype, pdict = cgi.parse_header(self.headers.getheader('content-type'))
         print(ctype)
         if ctype == 'application/json':
             length = int(self.headers.getheader('content-length'))
             json_string = self.rfile.read(length)
             print(json_string)          
             
             #data = cgi.parse_qs(self.rfile.read(length), keep_blank_values=1)
             json_object = json.loads (json_string)
             
             print ("" % json_object)
             #print(json_object['age'])
             
             patient_data = json_object['patient_data']
             
             #imp1 = imputer() # generate imputer class
             #patient = [1, 1, 0, 0, 0, 0, 1, 0, 0, 1, None, None, None, None, None, None, None, None]
             # call sunhwan script here
             imp = imputer()
             rs1 = imp.getNextList(patient_data)
             imp_val = imp.getImputedValue(patient_data)
             #print imp_val
             # call Frank's script here 
             patout = glmout()
             #imputer_out = pd.read_csv("imputer_out.csv")
             #imp = imputer_out.iloc[0]
             #print imp
             rs = patout.compute_model(imp_val)
             print patient_data
             #for d1 in json_object:
             #    print json_object[d1]
                 #print("" % val1)
        
            # print(j['name'])
             #recordID = self.path.split('/')[-1]
             #LocalData.records[recordID] = data
             print ("record %s is added successfully")  
         else:
             print ("Iam here ")
             length = int(self.headers.getheader('content-length'))
             print(self.rfile.read(length))
             data = cgi.parse_qs(self.rfile.read(length), keep_blank_values=1)
             print ("Data = " % data)
             data = {}
         
        
         self.send_response(200)
         self.end_headers()
         print patient_data
         self.wfile.write(rs1+"\n")         
         self.wfile.write(rs)
     else:
         self.send_response(403)
         self.send_header('Content-Type', 'application/json')
         self.end_headers() 
         return
 
 def do_GET(self):
     print("Here")
     if None != re.search('/api/v1/getrecord/*', self.path):
         recordID = self.path.split('/')[-1]
         if LocalData.records.has_key(recordID):
             self.send_response(200)
             self.send_header('Content-Type', 'application/json')
             self.end_headers()
             self.wfile.write(LocalData.records[recordID])
         else:
             self.send_response(400, 'Bad Request: record does not exist')
             self.send_header('Content-Type', 'application/json')
             self.end_headers()
     else:
         self.send_response(403)
         self.send_header('Content-Type', 'application/json')
         self.end_headers()
 
     return
 
class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
 allow_reuse_address = True
 
 def shutdown(self):
     self.socket.close()
     HTTPServer.shutdown(self)
 
class SimpleHttpServer():
 def __init__(self, ip, port):
     self.server = ThreadedHTTPServer((ip,port), HTTPRequestHandler)
 
 def start(self):
     self.server_thread = threading.Thread(target=self.server.serve_forever)
     self.server_thread.daemon = True
     self.server_thread.start()
 
 def waitForThread(self):
     self.server_thread.join()
 
 def addRecord(self, recordID, jsonEncodedRecord):
     LocalData.records[recordID] = jsonEncodedRecord
 
 def stop(self):
     self.server.shutdown()
     self.waitForThread()
 
if __name__=='__main__':
 parser = argparse.ArgumentParser(description='HTTP Server')
 parser.add_argument('port', type=int, help='Listening port for HTTP Server')
 parser.add_argument('ip', help='HTTP Server IP')
 args = parser.parse_args()
 
 server = SimpleHttpServer(args.ip, args.port)
 print ('HTTP Server Running...........')
 server.start()
 server.waitForThread()
 
