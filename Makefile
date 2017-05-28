.PHONY: clean build utop async

all: build

clean:
	-rm -rf _build
	-rm -rf bin/.merlin
	-rm -rf src/.merlin
	-rm -rf *.install

build: async

async:
	jbuilder build --only-packages=mlvin,mlvin-async @install

utop:
	jbuilder exec utop
