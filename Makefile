ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: ert ecukes

ert:
	carton exec ./test/php+-mode-test

ecukes:
	carton exec ${ECUKES} features


