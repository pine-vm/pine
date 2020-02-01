# Since it only uses Docker commands, we can run this script on both Windows and Linux.

docker build --tag elm-fullstack-test .

docker stop fullstack-test-container

docker run --rm -p 80:80 -p 443:443 --name fullstack-test-container --env "APPSETTING_adminRootPassword=notempty" elm-fullstack-test
