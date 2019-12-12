docker build --tag elm-fullstack-test ./../../

docker stop fullstack-test-container

docker build --tag demo-backend-state .

docker stop demo-backend-state-container

docker run --rm -p 80:80 -p 443:443 --name demo-backend-state-container --env "APPSETTING_adminRootPassword=notempty" demo-backend-state
