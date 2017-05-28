.PHONY: clean build utop async lwt

all: build

clean:
	-rm -rf _build
	-rm -rf bin/.merlin
	-rm -rf src/.merlin
	-rm -rf *.install

build:
	jbuilder build --only-packages=mlvin,mlvin-async,mlvin-lwt @install

async:
	jbuilder build --only-packages=mlvin,mlvin-async @install

lwt:
	jbuilder build --only-packages=mlvin,mlvin-lwt @install

utop:
	jbuilder exec utop
