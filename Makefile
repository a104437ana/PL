TEX_FILE = Relatorio.tex
BUILD_DIR = build
PDF_FILE = $(BUILD_DIR)/$(TEX_FILE:.tex=.pdf)
LATEX = pdflatex

all: $(PDF_FILE)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(PDF_FILE): $(TEX_FILE) | $(BUILD_DIR)
	$(LATEX) -output-directory=$(BUILD_DIR) $(TEX_FILE)
	$(LATEX) -output-directory=$(BUILD_DIR) $(TEX_FILE)
	$(LATEX) -output-directory=$(BUILD_DIR) $(TEX_FILE)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: all clean