PANDOC = pandoc
PANDOC_OPTS += -t revealjs
PANDOC_OPTS += -s
PANDOC_OPTS += --template template.html
PANDOC_OPTS += --no-highlight
PANDOC_OPTS += --variable hlss=zenburn
PANDOC_OPTS += --slide-level=2
PANDOC_OPTS += --mathjax="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
# PANDOC_OPTS += --css slides.css
PANDOC_OPTS += --email-obfuscation=none
PANDOC_OPTS += -f markdown+smart

TARGET_FILE = slides.html

all: reveal.js $(TARGET_FILE)

reveal.js:
	git clone https://github.com/hakimel/reveal.js/

clean:
	rm $(TARGET_FILE)

.PHONY: all clean

$(TARGET_FILE): slides.md
	$(PANDOC) $(PANDOC_OPTS) -o $@ $?
