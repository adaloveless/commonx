del ..\commonx_private\*.dcu
copy ..\commonx_private\%1.*
cd ..
cd commonx_private
git rm %1.*
git commit -m "rm %1"
git push
cd ..
cd commonx
git add %1.*
git commit -m "add %1"
git push



