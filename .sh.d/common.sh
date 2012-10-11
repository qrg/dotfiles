# -----------------------------------------------
# path config
# -----------------------------------------------
COMMONDIR=~/.sh.d
ALIASESFILE=${COMMONDIR}/aliases.sh
ENVFILE=${COMMONDIR}/env.sh

# -----------------------------------------------
# shell environment variable
# -----------------------------------------------
[ -f ${ENVFILE} ] && . ${ENVFILE}

# -----------------------------------------------
# aliases
# -----------------------------------------------
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
[ -f ${ALIASESFILE} ] && . ${ALIASESFILE}


