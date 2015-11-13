Trying to reconstruct http://lalashan.mcmaster.ca/theobio/circumcision/index.php/Condom_awareness/model.

To work, this directory should be in parallel with:

* makestuff (you should be able to just type make start.makestuff if it's not)

* the _private_ repo `DHS_downloads`

If you type `make` just now, it will probably run for a long time (trying to make the big model). Try `make mergedData.Rout` to run the upstream pipeline first.

Downstream pipeline not installed yet. Still trying to think about a good workflow involving a directory where we could share target files (Dropbox?) and qsub.
