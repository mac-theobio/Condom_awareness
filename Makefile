# Condom_awareness
### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: plos_CondomMS.pdf 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md
include stuff.mk

##################################################################

## Data

data = $(gitroot)/DHS_downloads/legacy/condom

# Dataset names
ir = keir52 lsir60 nmir51 snir4h szir51 ugir52 zwir51
mr = kemr52 lsmr60 nmmr51 snmr4h szmr51 ugmr52 zwmr51
allsets = $(ir) $(mr)

######################################################################

## Selection

## Make a selection file with variable names for men and women
Sources += rawSelect.csv select.pl
uselect.csv: rawSelect.csv select.pl
	$(PUSH)

## Select variables and make smaller data frames; print some sort of summary (selections.Routput)
select = $(allsets:%=%.select.Rout)
selections = $(allsets:%=%.select.Routput)

Sources += uselect.R
$(select): %.select.Rout: uselect.csv $(data)/%.rename.RData uselect.R
	$(run-R)

selections.Routput: $(selections)
	cat $^ > $@

######################################################################

## Cleaning

# As Audrey points out the cleaning script is pretty old-school

Sources += clean.R
clean = $(allsets:%=%.clean.Rout)
$(clean): %.clean.Rout: %.select.RData clean_%.R clean.R
	$(run-R)

# Country-specific rules if necessary
Sources += clean_snir4h.R
clean_%.R:
	touch $@

clean: $(clean)
	echo $^ > $@

cleaned = $(allsets:%=%.clean.Routput)
cleaned.Routput: $(cleaned)
	cat $^  > $@

######################################################################

## Vertical merge
%.fake: %
	touch $@

fake = $(allsets:%=%.clean.Rout.fake)

Sources += mergedData.R
mergedData.Rout: $(fake) mergedData.R
	$(run-R)

######################################################################

## Run the big model
Sources += model.R
model.big.Rout: mergedData.Rout model.R
	$(run-R)

### Crib

# $(Sources): 
	# /bin/cp WorkingWiki-export/Condom_awareness/$@ .

##################################################################

# The Manuscript

Sources += condom_ms.bib plos_CondomMS.tex
Sources += PLOS-submission.eps plos2015.bst

plos_CondomMS.pdf: PLOS-submission.pdf
PLOS-submission.pdf: PLOS-submission.eps
	convert $< $@

plos_CondomMS.pdf: plos_CondomMS.bbl plos_CondomMS.tex


######################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
# Makefile: start.makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/perl.def

-include $(ms)/wrapR.mk
-include $(ms)/oldlatex.mk
