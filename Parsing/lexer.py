#__author__ = "	Javier Alejandro ATADIA Florencia ALVAREZ Paulina ALVAREZ Andrei-Florin BENCSIK"
#__copyright__ = "Copyright 2017, IMT Atlantique"
#__version__ = "0.0.5"
#__status__ = "Test"
import os
import sys
import argparse
#################################################################################################
def main():
	parser = argparse.ArgumentParser()

	parser.add_argument("--file", help=".java class file")

	args = parser.parse_args()
	
 	if args.file:
 		file_name = args.file
		os.system("./build/main_lexer " + file_name)

	else:
		os.system("python " + sys.argv[0] + " --help")
  		exit()
#################################################################################################
#  MAIN  #
#################################################################################################
if __name__ == "__main__":
	main()