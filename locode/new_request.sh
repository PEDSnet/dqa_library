#!/bin/bash

# Set up new directory tree for a request using the standard framework
#
# Usage: /path/to/new_request.sh dir_name

target=${1:-new_request}
if [[ ! -z $(ls -d "$target"/* 2>/dev/null)  || \
      ! -z $(ls -d "$target"/.[a-zA-Z0-9]* 2>/dev/null | grep -v /.git\$ ) ]]
then
    echo >&2 "Target directory $target already has content."
    exit 1
fi
if [[ ! -d "$target" ]]; then mkdir -p "$target"; fi
target=$(cd "$target"; pwd)

src=$(dirname $0)
src=${src:-$PWD}
if [[ ! -d "$src" || ! -e "$src/new_request.sh" ]]
then
    echo >&2 "Unable to find standard framework"
    exit 1
fi

cd "$src/.."
cp -R code  local locode reporting request results site specs "$target"
cat .gitignore >>"$target/.gitignore"
vers=$(git log -1 --date=short --format='%h %ad')

cd "$target"
sed -e "s/<framework_version>/$vers/" <code/req_info.R >code/$$.tmp && \
    mv -f code/$$.tmp code/req_info.R
if [[ $(git status >/dev/null 2>&1; echo $?) -ne 0 ]]; then git init .; fi
git add .
git commit -m 'Standard framework'

tbase=$(basename $target)
echo "
Setup complete.

Please remember to:
  * Create tracking ticket(s) in Jira for this request
  * If this is not part of a larger existing project, connect this repo
    to a Bitbucket remote (creating it if needed) using something like
    git remote add origin https://reslnpedsnops01.research.chop.edu/bitbucket/scm/prp/$tbase.git
    git push -u origin master

Done."

