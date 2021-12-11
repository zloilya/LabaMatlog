#HC=/opt/ghc/8.10.5/bin/ghc
HC=ghc
SOURCES=src/Main.hs
PACKAGE=hw1.zip

.PHONY: pack all run clean

all: parser

run: parser
	./parser

clean:
	rm -rf src/*.o src/*.hi
	rm -f parser

parser: $(SOURCES)
	$(HC) -i./src -tmpdir . ./src/Main.hs -o parser

pack: $(SOURCES)
	zip $(PACKAGE) -r Makefile src 
