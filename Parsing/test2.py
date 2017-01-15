import os
import sys

def main(files_dir):
	"""iterates over the folders and calls the main for each file
	the mode is decided acording to the name of the folder in wich the file
	contained following the pattern *_mode"""
	for dir,folder,files in os.walk(files_dir):
		if dir!=files_dir:
			mode = dir.split("/")[-1].split("_")[-1]
			ok_nok = dir.split("/")[-1].split("_")[0]
			if ok_nok == "bad":
				ok_nok = " -b"
			else:
				ok_nok = ""	
			for file in files:
				file_name= dir+"/"+file
				if len(sys.argv)==1:
					os.system("./build/main -m "+mode+" -f "+file_name+ok_nok)
				else:
					os.system("./build/main -m "+mode+" -f "+file_name+ " -v "+ok_nok)

files_dir=("./classes_testing/","./expression_testing/")
for i in files_dir:
	main(i)