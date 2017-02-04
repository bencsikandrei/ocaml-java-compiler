import os
import sys

def main(files_dir):
	"""iterates over the folders and calls the main for each file
	the mode is decided acording to the name of the folder in wich the file
	contained following the pattern *_mode"""
	## the number of test files
	test_count = 0
	test_failed = 0
	for dir,folder,files in os.walk(files_dir):
		if dir!=files_dir:
			## from the folder name extract the MODE 
			mode = dir.split("/")[-1].split("_")[-1]
			## from the folder name extract the BAD
			ok_nok = dir.split("/")[-1].split("_")[0]
			## set BAD if we expect a failed test
			if ok_nok == "bad":
				ok_nok = " -b"
			else:
				ok_nok = ""	
			## loop through all files
			for file in files:
				## increment test count
				test_count += 1
				file_name= dir+"/"+file
				if len(sys.argv)==1:
					result = os.popen("./build/main -m "+mode+" -f "+file_name+ok_nok).read()
					print result
					# os.system()
				else:
					# os.system("./build/main -m "+mode+" -f "+file_name+ " -v "+ok_nok)
					result = os.popen("./build/main -m "+mode+" -f "+file_name+" -v "+ok_nok).read()
					print result
				## search for BAD in the output
				if result.find(" BAD") != -1:
					test_failed += 1
					print "A test failed"

	## return the nb of failed and total
	return test_count, test_failed
	
## MAIN ##

files_dir=("./classes_testing/","./expression_testing/")
test_count = 0
test_failed = 0
for i in files_dir:
	(tc, tf) = main(i)
	print tc
	print tf
	test_count += tc
	test_failed += tf
print "========="
print "Finished!"
print "========="
print "Tests run: " + str(test_count) 
print "Tests successful: " + str(test_count - test_failed),
print " Rate: " + str((float((test_count - test_failed))/(test_count) * 100)) + "%"