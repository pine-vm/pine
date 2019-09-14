# Since it only uses Docker commands, we can run this script on both Windows and Linux.

docker build --tag kalmit-test .

docker stop kalmit-test-container

docker run --rm -p 80:80 -p 443:443 --name kalmit-test-container --env "APPSETTING_adminRootPassword=notempty" kalmit-test
