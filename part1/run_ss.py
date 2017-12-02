#!/usr/bin/env python2

import sys
import subprocess

from subprocess import call

def gen_syn_script(M, N, P, bits, clk):
    with open('runsynth_template.tcl', 'r') as file :
        filedata = file.read()

    filedata = filedata.replace('{ARG0}', str(M))
    filedata = filedata.replace('{ARG1}', str(N))
    filedata = filedata.replace('{ARG2}', str(P))
    filedata = filedata.replace('{ARG3}', str(bits))
    filedata = filedata.replace('{CLK}' , str(clk))

    with open('runsynth_layer.tcl', 'w') as file:
        file.write(filedata)

    return "layer_" + str(M) + "_" + str(N) + "_" + str(P) + "_" + str(bits) + ".txt"

def run_synth(M, N, P, bits, clk):
    lname = gen_syn_script(8, 8, 1,  8, clk)
    p1 = subprocess.Popen(["dc_shell", "-f", "runsynth_layer.tcl"], stdout=subprocess.PIPE)
    p2 = subprocess.Popen(["tee", lname], stdin=p1.stdout, stdout=subprocess.PIPE)
    p1.stdout.close()
    p2.communicate()

    with open(lname) as f:
        for line in f:
            if "Error" in line:
                print line
            elif "/CK" in line:
                print line
            elif "Warning" in line and "signed" not in line and "tran" not in line:
                print line
            elif "Total" in line and "undefined" not in line and "Dynamic" not in line:
                print line
            elif "slack" in line:
                print line

def run_sim(M, N, P, bits):
    lname =  "layer_" + str(M) + "_" + str(N) + "_" + str(P) + "_" + str(bits) + ".log"
    p1 = subprocess.Popen(["./testmodeone", str(M), str(N), str(P), str(bits)], stdout=subprocess.PIPE)
    p2 = subprocess.Popen(["tee", lname], stdin=p1.stdout, stdout=subprocess.PIPE)
    p1.stdout.close()
    p2.communicate()

    with open(lname) as f:
        for line in f:
            if "errors" in line:
                print line

run_sim(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
run_synth(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])

