#!/bin/sed -Ef
#
# Bach's prelude in C major from WTC book I, written in sed
# Obtained from https://github.com/laserbat/bach.sed
#
# MIT License
#
# Copyright (c) 2025 Olga Ustiuzhanina
#
# echo | ./bach.sed | aplay -r44100
# or
# echo | ./bach.sed | play -r 44100 -e unsigned -b 8 -c 1 -t raw -

# This line contains list of pitches, each letter representing a single pitch and a group of 5 letters representing a chord, to be arpeggiated
s/.*/ZBpqwACoqvACZBpswBEpquwAoqvADoZmZgmquykoqvxknsvzjmqwAjlqtxikpvyijmptgjmptdgkotfikpsfknpsbjmpscfmprejopqdjkoqdikpsdgkptdgkotdhmpudikpvdgkptdgkotafkns/

# Z is used as a placeholder for a common note sequence, used to save a bit of space
s/Z/psvy/g

# This line repeats notes in a way that produces the correct arpeggios
s/..(...)/&\1&\1/g

# Here, non-arpeggiated ending is added and after ';', there's a data table that contains letters and pitches corresponding to them
# Format of the data table is [letter][value] where value is the wavelength of pitch corresponding to the letter, written in base 9 with space char (' ') representing value of 1
# All wavelengths are pre-calculated with assumption that the output is treated as 44.1khz, single-channel audio
# Also, each value in this table must contain exactly 3 digits
# Yes, this is a really weird way to store values, but there are reasons for it

# And right after the pitch table (after the last [letter][3 digits] sequence) there's an addition table for base 9
# E.g. 844 basically means 8 == 4 + 4 and 523 means 5 == 2 + 3
# Yes, this is also really weird but trust me, it's necessary for a computation further in the code
s/$/afjmptpmpmjmjgjgafvxACAxAxvxqtsqafsvyy;a828b62 c580d550e522f4 4g363h344i327j3  k270l255m242n230o2 8p207q 76r 67s 58t 50u 42v 35w 2 x 08y 03z088A083B074C070D062E05584473463352342232 2  /

# Here we replace each letter with a matching value from a table, Z is used as a separator
# This is done in a loop that only ends when there's no more replacement possible
:a
    s/([a-zA-X])(.*\1(...))/Z\3\2/
ta

# This expression adds a '!' for each multiplication by 3 you're supposed to do on the corresponding digit before adding everything up to get the number value
# That's the reason for using base 9: it can be easily converted to unary by tripling each digit value a certain number of times
s/(Z.)(.)(.)/\1!!!!\2!!\3Z/g

# Now addition table is used to convert each digit into the unary value of this digit using the addition table
# Space characters are used to represent the resulting unary value, that's why 1 is replaced with a space in the tables above
# Again, done in a loop
:b
    s/([2-8])(.*\1(..))/\3\2/
tb

# Now triple every string that has an exclamation mark after it
:c
    s/( +)!/\1\1\1/g
tc

# Get rid of useless remaining characters, leaving only spaces (which represent the wave length) and separators ('Z')
s/[^ Z]//g

# For each wave, find its midpoint and mark it with a '~' character
# Note that a space has ASCII value of 32, lowest of any printable character, while '~' has the value of 126, highest of any printable characters
s/( +) ( ?\1)/\1~\2/g

# Now we turn any space that happens to be after a tilde into another tilde
# This is done to produce a square waveform, with spaces being the low points of it, and tildes being the high
:d
    s/~ /~~/g
td

# Now some extra magic happens: since we only created one cycle of each wave so far, we need to repeat them so that each note plays for a certain amount of time
# I chose, arbitrarily, 0.25 of a second or exactly 11025 samples
# Now, sed is pretty slow at repeating a string many many times and it took me a while to come up with a relatively fast way of doing it:
# First regex in this loop replaces any string longer or equal to 11025 characters between two separators with the first 11025 characters of it
# The next string repeats any string between two separators thrice
# So basically each note gets exponentially longer until it hits the required limit
# This way we avoid doing expensive matching too often and hence save on computation time
:e
    s/Z([ ~]{11025})\W+Z/\1/g
    s/Z(\W+)Z/Z\1\1\1Z/g
te

# Done! Now as all computation is done, sed will print the resulting string
