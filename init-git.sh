IGNOREFILE=~/.gitignore_global

cd $(dirname $0)

echo -ne "\nSetup ${IGNOREFILE} as Git global core.excludesfile, Right? [y/n] : "
read SETGLOBALIGNORE

if [ ${SETGLOBALIGNORE} = 'y' -o ${SETGLOBALIGNORE} = 'yes' ]; then
  if [ ! -e ${IGNOREFILE} ]; then
    echo "${IGNOREFILE} does not exist."
  else
    echo "git config --global core.excludesfile ${IGNOREFILE}"
    git config --global core.excludesfile ${IGNOREFILE}
    echo -e '\nComplete!'
    echo -e '\n--------------------'
    echo '~/.gitconfig'
    echo ''
    cat -n ~/.gitconfig
  fi
elif [ ${SETGLOBALIGNORE} = 'n' -o ${SETGLOBALIGNORE} = 'no' ]; then
  echo 'Cancelled. Nothing to do.'
else
  echo 'Nothing to do.'
fi

echo -e '\nUpdating Git submodules...'
echo 'git submodule init'
git submodule init
echo 'git submodule update'
git submodule update
echo -e '\nComplete!'
