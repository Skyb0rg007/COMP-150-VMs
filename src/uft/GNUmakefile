
SWIPL := swipl

ifeq (run, $(firstword $(MAKECMDGOALS)))
	RUN_ARGS := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
	$(eval $(RUN_ARGS):;@:)
endif

run:
	@$(SWIPL) ./uft/main.pl $(RUN_ARGS)

uft-prog:
	$(SWIPL) --stand_alone=true -o $@ -g true -c uft/main.pl

uft:
	$(SWIPL) --stand_alone=true -o ../../bin/uft -g true -c uft/main.pl

