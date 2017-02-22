import sys, os, io
import subprocess

def printError(f):
	print f + ' \033[91m'+"FAIL"+'\033[0m'
	os.system("cat ./out.txt")
	os.system("cat ./err.txt")

	

def check(path,fail=False):
	for dir,folder,files in os.walk(path):
		if dir==path:
			for i in files:
				out = io.open("./out.txt","w")
				err = io.open("./err.txt","w")
				output = subprocess.call("ocamlbuild -quiet -use-ocamlfind Main.byte -- "+path+"/"+i, shell=True, stdout=out, stderr=err)
				out.close()
				err.close()
				if (not fail and output != 0):
					printError(i)
				elif (fail and output == 0):
					printError(i)
				else:
					print i + ' \033[92m'+"OK"+'\033[0m'
					if fail:
						os.system("cat ./err.txt")
					

goodPath = "./tests/Good"
failPath = "./tests/Fail"

print "*******************"
print "These files should pass\n"
check(goodPath)

print

print "*******************"
print "These files should not\n"
check(failPath,fail=True)

