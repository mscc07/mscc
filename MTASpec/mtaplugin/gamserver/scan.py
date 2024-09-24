import os,string

fp = open ("gamconstants.py","r")
data=fp.readlines()
fp.close()

enc = 0
for lines in data:
    if lines.find("PLUGIN_ROOT")>=0:
        lines= lines.replace(" ","")
        print lines
        print lines[len(lines)-2],len(lines)
        enc=enc +1
    if lines.find("OUTPUT_PREFIX")>=0: 
        print lines.strip()
        enc=enc +1
    if enc==2:break
