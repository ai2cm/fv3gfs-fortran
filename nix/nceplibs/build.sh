source $stdenv/setup

PATH=$perl/bin:$PATH
pwd
mkdir -p $out
cp -r $src src
chmod -R +w src
cd src

echo "y" | bash $src/make_ncep_libs.sh -s ${arch} -c gnu -d $out -o 1
