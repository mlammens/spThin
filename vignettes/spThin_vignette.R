## ----load_package--------------------------------------------------------

## Install package from source, then load package into workspace
install.packages( type = "source", pkgs = "spThin_0.1.0.tar.gz", repos = NULL )
library( spThin )


## ------------------------------------------------------------------------
data( Heteromys_anomalus_South_America )
head( Heteromys_anomalus_South_America )

## ------------------------------------------------------------------------
table( Heteromys_anomalus_South_America$REGION )

