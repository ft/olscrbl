# Example ‘olscrbl’ initialisation file.   -*- shell-script -*-
# ...with annotations.

# This file is sources by a shell script that later dispatches to the actual
# scheme code. Doing this enables the user to selectively alter the execution
# environment of the program.

# For example, you might want to adjust some of the environment variables, that
# change the way that guile itself behaves.

# Like turning off automatic byte compilation:
GUILE_AUTO_COMPILE=0
export GUILE_AUTO_COMPILE

# Or maybe amend its load-path?
GUILE_LOAD_PATH="${HOME}/src/olscrbl/scheme"
export GUILE_LOAD_PATH

# And for byte-compiled files?
GUILE_LOAD_COMPILED_PATH="${HOME}/src/olscrbl/scheme"
export GUILE_LOAD_COMPILED_PATH

# More detailed warnings for deprecated features used by olscrbl?
GUILE_WARN_DEPRECATED=detailed
export GUILE_WARN_DEPRECATED


# There are also some variables, that are used by the ‘olscrbl’ wrapper script
# itself. You don't need to export these yourself. Should it be required to do
# that, the script will take care of it.

# For example, you can disable message about which config file is read, by
# setting the following variable to "t". "f" is the default value.
OLSCRBL_LOAD_QUIET=f

# Finally, you might have a specific version of guile installed somewhere, that
# you want to point the ‘olscrbl’ script to. The following parameter let's you
# do just that:
OLSCRBL_GUILE_BINARY="${HOME}/local/guile/bin/guile"
