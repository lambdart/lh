# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch

# Remove command
RM = rm

# Additional emacs load-path and autoload
LOAD_PATH := -L ${PWD}
LOAD_AUTOLOAD := -l autoload

# Define Compile Command
# Call batch-byte-compile function: -f
COMPILE  = -f batch-byte-compile

# Autoload related variables
AUTOLOAD_DIR  := "${PWD}"
AUTOLOAD_FILE := "${PWD}/lh-autoloads.el"
AUTOLOAD_EVAL := --eval '(make-directory-autoloads ${AUTOLOAD_DIR} ${AUTOLOAD_FILE})'

# Expand the source code files
EL != ls *.el

# Compiled files
ELC = ${EL:.el=.elc}

# Entry
all: compile update_autoloads

# Compile needed files
compile: $(ELC)

# Translate lisp text (.el) files in byte compiled (.elc) files
$(ELC): $(EL)
	${EMACS} ${LOAD_PATH} ${COMPILE} ${.ALLSRC}

# Update load definitions
update_autoloads:
	${EMACS} ${LOAD_AUTOLOAD} ${AUTOLOAD_EVAL}

# Delete elisp byte compiled files (.elc)
clean:
	${RM} ${ELC}
