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

git pull

latestDir=$(ls -td ../data/BAG/*/ | head -1)
if [ -f lastBAGdir.txt ]; then
  lastDir=$(<lastBAGdir.txt)
else
  lastDir="no latest dir found"
fi

latestFile=$(ls -t ../data/BAG/*/*FOPH_COVID19_data_extract.csv | head -1)
if [ -f lastBAGFile.txt ]; then
  lastFile=$(<lastBAGFile.txt)
else
  lastFile="no latest dir found"
fi

LBupdate=$(stat -c ‘%y’ ../R/trendsModule-Files/Lagebeurteilung.Rmd)
if [ -f lastLBupdate.txt ]; then
  lastLBupdate=$(<lastLBupdate.txt)
else
  lastLBupdate="no latest LB update found"
fi

RePrivateUpdate=$(stat -c ‘%y’ ../data/Re/CHE-Estimates.rds)
RePublicUpdate=$(stat -c ‘%y’ ../data/RePublic/CHE-Estimates.rds)
if ["$RePrivateUpdate" -gt "$RePublicUpdate"]; then
  ReUpdate=RePrivateUpdate
else
  ReUpdate=RePublicUpdate
fi

if [ -f lastReUpdate.txt ]; then
  lastReUpdate=$(<lastReUpdate.txt)
else
  lastReUpdate="no latest Re update found"
fi

# scripts to be run when new BAG data is available 
if [ "$latestFile" != "$lastFile" ]; then
  bash ../R/trendsModule-Files/updateTrends.sh
  echo "$latestFile">lastBAGFile.txt
fi

# scripts to be run when new lagebericht is available 
if [ "$LBupdate" != "$lastLBupdate" ]; then
  bash ../R/trendsModule-Files/updateTrends.sh
  echo "$LBupdate">lastLBupdate.txt
fi

# scripts to be run when new Re data is available 
if [ "$ReUpdate" != "$lastReUpdate" ]; then
  bash ../R/trendsModule-Files/updateTrends.sh
  echo "$ReUpdate">lastReUpdate.txt
fi

