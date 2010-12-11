.SUFFIXES: .erl .beam .yrl

SOURCE_DIR=src
EBIN_DIR=ebin
MODS=$(wildcard $(SOURCE_DIR)/*.erl)

%.beam: %.erl
	erlc -o $(EBIN_DIR) -W $<

all: beam

beam: ${MODS:%.erl=%.beam}

clean:
	rm -rf $(EBIN_DIR)/*.beam $(EBIN_DIR)/erl_crash.dump

