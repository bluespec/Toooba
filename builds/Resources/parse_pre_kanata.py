#! /usr/bin/env python3
#
#  Copyright (c) 2024 Franz Fuchs
#  All rights reserved.
# 
#  This software was developed by the University of  Cambridge
#  Department of Computer Science and Technology under the
#  SIPP (Secure IoT Processor Platform with Remote Attestation)
#  project funded by EPSRC: EP/S030868/1
# 
#  @BERI_LICENSE_HEADER_START@
# 
#  Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
#  license agreements.  See the NOTICE file distributed with this work for
#  additional information regarding copyright ownership.  BERI licenses this
#  file to you under the BERI Hardware-Software License, Version 1.0 (the
#  "License"); you may not use this file except in compliance with the
#  License.  You may obtain a copy of the License at:
# 
#    http://www.beri-open-systems.org/legal/license-1-0.txt
# 
#  Unless required by applicable law or agreed to in writing, Work distributed
#  under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
#  CONDITIONS OF ANY KIND, either express or implied.  See the License for the
#  specific language governing permissions and limitations under the License.
# 
#  @BERI_LICENSE_HEADER_END@
#

import argparse

def parse(filename):
    with open(filename, "r") as f:
        cur_cycle = 0
        print("Kanata\t0004")
        print("C=\t0")
        for line in f:
            v = line.split()
            c = int(v[1])
            if(c > cur_cycle):
                print("C\t{0:0d}".format((c - cur_cycle)))
                cur_cycle = c
            print("{0}\t{1}\t{2}\t{3}".format(v[0], v[2], v[3], v[4]))

def main():
    parser = argparse.ArgumentParser(description='''
        Generate Kanata 0004 log file from bluespec implementation output
        ''')
    
    parser.add_argument('logfile', type=str, help='path to logging output')

    args = parser.parse_args()

    if args.logfile:
        parse(args.logfile)
    else:
        sys.exit("Must specify the logging output of the implementation")

if __name__ == "__main__":
    main()