SHELL=/bin/bash

all: 	clean assembly
	mkdir bin
	mkdir bin/libraries
	cp target/scala-2.11/dapa-assembly-1.0.0.jar bin/dapa.jar
	cp main/resources/libraries/* bin/libraries
	cp README.md bin/README.md
	echo "Everything is ready in the bin/ folder."


assembly:
		sbt assembly

clean:
	        rm -rf bin

.PHONY: clean assembly all update dist
