# Build dotnet build image
FROM mcr.microsoft.com/dotnet/sdk:8.0.403 AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/pine
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:8.0.10 AS binaries

COPY --from=build-env /app/pine/out /pine/dotnet/

# Build the process with a deployment for the default app.
FROM binaries AS build-default-config

RUN apt update
RUN apt install -y curl
# Support partial clone of git repositories: Install git as fallback implementation for cloning.
RUN apt install -y git

COPY ./example-apps/docker-image-default-app /docker-image-default-app/

RUN dotnet "/pine/dotnet/pine.dll"  deploy  /docker-image-default-app/  /pine-vm/process-store  --init-app-state

WORKDIR /pine

# Mounting a docker volume can shadow the previous state of the process store:
#
# docker  volume  create  docker-volume-name
# docker  run  --mount 'source=docker-volume-name,destination=/pine-vm/process-store'  ....

ENTRYPOINT ["dotnet", "/pine/dotnet/pine.dll", "run-server", "--process-store=/pine-vm/process-store", "--process-store-readonly=/elm-time/process-store"]

# ENV APPSETTING_adminPassword="password-for-admin-interface"
