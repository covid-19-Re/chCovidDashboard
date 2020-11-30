# for development on local macOS machine (on Linux just install the owncloud cli tools)
# symlink your polybox folder to ../data/BAG
# i.e. ln -s 'path/to/polybox/shared/BAG COVID19 Data' 'path/to/app/data/BAG'
# owncloudcmd -n -s ~/BAGdata https://polybox.ethz.ch/remote.php/webdav/BAG%20COVID19%20Data
# owncloudcmd -n -s ~/ICUdata https://polybox.ethz.ch/remote.php/webdav/Shared/outputTaskforce
# polybox is updated every minute from covid-19-re, no need to do it here

parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)
cd "$parent_path"

latestDir=$(ls -td ../data/BAG/*/ | head -1)
if [ -f lastBAGdir.txt ]; then
  lastDir=$(<lastBAGdir.txt)
else
  lastDir="no latest dir found"
fi

if [ "$latestDir" != "$lastDir" ]; then
  # scripts to be run when new BAG data is available 
  bash ../R/trendsModule-Files/updateTrends.sh
  echo "$latestDir">lastBAGdir.txt
fi

