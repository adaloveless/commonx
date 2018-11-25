set /P msg=Enter Message: 
git add *
git commit -m "%msg%"
git push
cd ..
git add commonx
git commit -m "%msg%"
git push
cd commonx
pause

