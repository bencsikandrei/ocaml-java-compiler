import sys, os, io
import subprocess

def printError(f):
	print f + ' \033[91m'+"FAIL"+'\033[0m'
	os.system("cat ./out.txt")

	

def check(path,fail=False):
	for dir,folder,files in os.walk(path):
		if dir==path:
			for i in files:
				#os.system ("ocamlbuild -quiet -use-ocamlfind Main.byte -- "+path+"/"+i)
				out = io.open("./out.txt","w")
				output = subprocess.call("ocamlbuild -quiet -use-ocamlfind Main.byte -- "+path+"/"+i, shell=True, stdout=out, stderr=out)
				out.close()
				if (not fail and output != 0):
					printError(i)
				elif (fail and output == 0):
					printError(i)
				else:
					print i + ' \033[92m'+"OK"+'\033[0m'

goodPath = "./tests/Good"
failPath = "./tests/Fail"

print "*******************"
print "This files should pass\n"
check(goodPath)

print

print "*******************"
print "This files should not\n"
check(failPath,fail=True)

