if [ ! -d serialbox ]; then
  git clone -b master https://github.com/VulcanClimateModeling/serialbox2.git serialbox
  cd serialbox
  git pull
  git checkout savepoint_as_string
  cd -
fi

docker build -t phys_standalone .
