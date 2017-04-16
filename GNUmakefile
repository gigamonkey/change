PATH := $(PATH):/Users/peter/.local/bin/

W =

hlint		:= /Users/peter/Library/Haskell/bin/hlint
libs		:= simple memo1 memo2 memo2state lazy
mains		:= benchmark trace count fib lazy-hardwired lazy-semi-hardwired countstate
binaries	:= $(addsuffix .o,$(libs))

all: $(mains)

benchmark: $(binaries)

lint:
	$(hlint) $(addsuffix .hs,$(libs) $(mains))

clean: tidy
	rm -f $(mains)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%.o: %.hs
	stack ghc -- -O2 -W$(W) $<

%: %.hs
	stack ghc -- -O2 -W$(W) $<
