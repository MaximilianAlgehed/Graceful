SOURCES=$(wildcard *.lhs)
OBJECTS=$(SOURCES:.lhs=.pdf)

all: $(OBJECTS)

%.pdf: %.lhs
	pandoc --template template.tex $< -o $@

clean:
	rm -f $(OBJECTS)
