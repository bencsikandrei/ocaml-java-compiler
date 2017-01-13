import os
import sys

def main(files_dir):
	"""iterates over the folders and calls the main for each file
	the mode is decided acording to the name of the folder in wich the file
	contained following the pattern *_mode"""
	for dir,folder,files in os.walk(files_dir):
		if dir!=files_dir:
			mode = dir.split("/")[-1].split("_")[-1]
			for file in files:
				file_name= dir+"/"+file
				if len(sys.argv)==1:
					os.system("./build/main "+mode+" "+file_name)
				else:
					os.system("./build/main "+mode+" "+file_name+ " -vervose")

files_dir=("./classes_testing/",)#"./expression_testing/")
for i in files_dir:
	main(i)