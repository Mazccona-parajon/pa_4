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
fileappend 'outputDir$'/'outFile$' fileID,vowel,f1,f2,vot,notes'newline$'

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

# Extract vowel from filename (e.g., bi01_kaka.wav -> "a")
# The vowel is the 3rd character in the CVCV pattern after the underscore
underscore_pos = index(prefix$, "_")
if underscore_pos > 0
    after_underscore$ = mid$(prefix$, underscore_pos + 1, length(prefix$))
    # Get the second character (first vowel in CVCV)
    if length(after_underscore$) >= 2
        vowel$ = mid$(after_underscore$, 2, 1)
    else
        vowel$ = "unknown"
    endif
else
    vowel$ = "unknown"
endif

Read from file... 'filePath$'/'prefix$'.wav
Read from file... 'filePath$'/'prefix$'.TextGrid

# Add vowel label to tier 4
select TextGrid 'prefix$'
intervals = Get number of intervals: 4
if intervals > 0
    # Label the first interval in tier 4 with the vowel
    Set interval text: 4, 1, vowel$
    # Save the modified TextGrid
    Save as text file: "'filePath$'/'prefix$'.TextGrid"
endif

points = Get number of points... 1

# Calculate vot 
if points >= 1
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

    # Append data to output 
    fileappend 'outputDir$'/'fileID$'.csv 'prefix$','vowel$','f1:2','f2:2','vot:2',''newline$'

    # Printline for bug fixes
    printline 'prefix$','vowel$','f1:2','f2:2','vot:2'
endif

# Clean up
select all
minus Strings dirFiles
Remove

endfor

# -----------------------------------------------------------------------


# Clean up
select all
Remove