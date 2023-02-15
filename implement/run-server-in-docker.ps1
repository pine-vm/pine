# Since it only uses Docker commands, we can run this script on both Windows and Linux.

docker  build  --tag elm-time-test  .

docker  stop  elm-test-container

docker  run  --rm  -p 80:80  -p 4000:4000  --name elm-test-container  --env "APPSETTING_adminPassword=notempty"  elm-time-test
