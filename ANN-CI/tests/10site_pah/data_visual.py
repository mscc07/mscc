import numpy as np
import sys

inp = str(sys.argv[1])
inputfile = open(inp,"r")
lines = inputfile.readlines()

for i in range(len(lines)):
	tokes = (lines[i].strip()).split()
	if len(tokes)>0:
		if tokes[0] == "Breakup":
			st = i
		if tokes[0] == "Reach":
			end = i
		if tokes[0] == "Iteration":
			end = i
out = inp+".gnu.dat"
fout = open(out,"w")
for i in range(st+2,st+7):
	tokes = (lines[i].strip()).split()
	#print(tokes[1], tokes[7], tokes[15])
	newline = ("%d\t%f\t%f\n")%(int(tokes[1]), float(tokes[7]), float(tokes[15]))
	fout.write(newline)
for i in range(st+12,end-1):
	tokes = (lines[i].strip()).split()
	#print(tokes[1], tokes[7], tokes[15])
	newline = ("%d\t%f\t%f\n")%(int(tokes[1]), float(tokes[7]), float(tokes[15]))
	fout.write(newline)
fout.close()
print("### The File is Ready###.\nPlease type the follwing command to visualize the outputs.\n")
print("gnuplot\nset terminal x11")
print(f"plot \"{out}\" u 1:2 w lp pt 2 lw 4 title \"Energy\"")
print(f"plot \"{out}\" u 1:3 w lp pt 2 lw 4 title \"Spin\"")


inp2 = inp+".accVsPreTest.dat"
print(f"plot \"{inp2}\" u 1:2 pt 7 ps 2 title \"accVSpred\"")

inp3 =  inp+".error.dat"
print(f"plot \"{inp3}\" u 3 w lp pt 2 lw 4 title \"Train Error\",  \"{inp3}\"  u 7 w lp pt 2 lw 4 title \"Test Error\"")

print("exit()")
