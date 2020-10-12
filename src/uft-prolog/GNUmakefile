
SWIPL := swipl
.PHONY: uft uft-prog

# Commit this so that Norman can run it
uft-prog:
	$(SWIPL) --stand_alone=true -o $@ -g true -c uft/main.pl

uft:
	$(SWIPL) -o ../../bin/uft -g true -c uft/main.pl

