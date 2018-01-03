.SUFFIXES: .erl .beam .yrl

SOURCE_DIR=src
EBIN_DIR=ebin
MODS=$(wildcard $(SOURCE_DIR)/*.erl)

%.beam: %.erl
	erlc -o $(EBIN_DIR) -W $<

all: beam

beam: ${MODS:%.erl=%.beam}

test:
	erl -noshell -pa ${EBIN_DIR} \
		-eval 'eunit:test("${EBIN_DIR}",[verbose])' \
		-s init stop

clean:
	rm -rf $(EBIN_DIR)/*.beam $(EBIN_DIR)/erl_crash.dump

