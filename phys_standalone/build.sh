if [ ! -d serialbox ]; then
  git clone -b master https://github.com/GridTools/serialbox.git serialbox
  cd serialbox
  git checkout f6faa58
  cd -
fi

docker build -t phys_standalone .
