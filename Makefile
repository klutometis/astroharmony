SRC := $(wildcard *.scm) Makefile
RAW := test.raw
WAV := $(RAW:.raw=.wav)

all: $(RAW) $(WAV)

$(RAW): $(SRC)
	csi -s chord.scm

od: $(RAW)
	od -A d -t u1 $(RAW)

play: $(RAW)
	play -c 2 -r 22050 -u -b $(RAW)

$(WAV): $(RAW)
	sox -c 2 -r 22050 -u -b $(RAW) $(WAV)

clean:
	rm -fv $(RAW) $(WAV)
