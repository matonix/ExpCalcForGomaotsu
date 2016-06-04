all: gomaotsu.hs
	# hastec -v --opt-all gomaotsu.hs
	hastec gomaotsu.hs

clean:
	rm -r main gomaotsu.js gomaotsu.o gomaotsu.hi