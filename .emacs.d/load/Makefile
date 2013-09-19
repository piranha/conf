SRC = $(wildcard *.el)
DST = $(SRC:%.el=%.elc)

all: $(DST)

%.elc: %.el
	emacs --batch --eval '(byte-compile-file "$<")'
