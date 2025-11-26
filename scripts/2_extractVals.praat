####################################
# Praat script to extract values   #
# Created by                       #
# Joseph V. Casillas 04/06/2022    #
#                                  #
# This file will:                  #
#  - extract vot, f1, f2 and       #
#  - save output to ./data dir     #
####################################


#
# Set some parameters ---------------------------------------------------
#

# Which participant?
form Select a participant
	sentence fileID bi01
endform

# Where to save data
outputDir$ = "../data/"

# Choose name for .csv file
outFile$ = fileID$+".csv"

# Where are the .wav and textgrid files located?
filePath$ = "../recordings/"+fileID$+"/wavs/"

# -----------------------------------------------------------------------






#
# Set up data file ------------------------------------------------------
#

# Delete current file if it exists
filedelete 'outputDir$'/'outFile$'

# Create newfile with header
fileappend 'outputDir$'/'outFile$' fileID,f1,f2,vot,notes'newline$'

# -----------------------------------------------------------------------







#
# Prepare loop ----------------------------------------------------------
#

Create Strings as file list... dirFiles 'filePath$'/*.wav
select Strings dirFiles
numberOfFiles = Get number of strings

# -----------------------------------------------------------------------





#
# Start loop ------------------------------------------------------------
#

for file to numberOfFiles
	select Strings dirFiles
	fileName$ = Get string: file
	prefix$ = fileName$ - ".wav"
	Read from file... 'filePath$'/'prefix$'.wav
	Read from file... 'filePath$'/'prefix$'.TextGrid
	points = Get number of points... 1
	labels = Count labels: 4, "exclude"
	labID$ = Get label of interval: 4, 1

	if labels = 0

		# Calculate vot 
		if points = 1
		voicing = Get time of point... 2 1
		release = Get time of point... 1 1
		vot = (voicing - release) * 1000
		window = release + 0.025

		# Calculate mid-point of vowel 
		vowelStart = Get start point: 3, 2
		vowelEnd  = Get end point: 3, 3
		durationV =  vowelEnd - vowelStart
		mp = vowelStart + (durationV * 0.50)

		# Get formants
		select Sound 'prefix$'
		do ("To Formant (burg)...", 0, 5, 5500, 0.025, 50)
		f1 = do ("Get value at time...", 1, mp, "Hertz", "Linear")
		f2 = do ("Get value at time...", 2, mp, "Hertz", "Linear")

	endif

	# Append data to output 
	fileappend 'outputDir$'/'fileID$'.csv 'prefix$','f1:2','f2:2','vot:2','labID$''newline$'

	# Printline for bug fixes
	printline 'prefix$','f1:2','f2:2','vot:2','labID$'

	# Clean up
	select all
	minus Strings dirFiles
	Remove
	endif
endfor

# -----------------------------------------------------------------------


# Clean up
select all
Remove
