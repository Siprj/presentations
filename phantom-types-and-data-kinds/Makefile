PANDOC = pandoc
PANDOC_OPTS += -t slidy -s
PANDOC_OPTS += --mathjax="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
PANDOC_OPTS += --css slides.css
PANDOC_OPTS += --email-obfuscation=none
PANDOC_OPTS += -f markdown+smart

TARGET_FILE = slides.html

all: $(TARGET_FILE)

clean:
	rm $(TARGET_FILE)

.PHONY: all clean

$(TARGET_FILE): slides.md
	$(PANDOC) $(PANDOC_OPTS) -o $@ $?
