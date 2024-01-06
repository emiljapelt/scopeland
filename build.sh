echo "Building Scopeland..."

echo "Building interpreter..."
cd ./interp
dune build
cd ..
if [ -e scopeland ] 
then rm -f ./scopeland
fi
mv -f ./interp/_build/default/src/scopeland.exe ./scopeland

echo "Done"
