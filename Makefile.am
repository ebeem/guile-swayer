# Top-level files to distribute
EXTRA_DIST = README.org LICENSE

# Define the base directory for Scheme modules
schemedir = $(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)

# Scheme files to install (including subdirectories)
nobase_scheme_DATA = \
    guile-swayer.scm \
    guile-swayer/libs.scm \
    guile-swayer/modules.scm \
    guile-swayer/sjson.scm \
    guile-swayer/swayipc.scm \
    guile-swayer/libs/sway-tree-helper.scm \
    guile-swayer/modules/*.scm \
    guile-swayer/sjson/*.scm \
    guile-swayer/swayipc/*.scm*
