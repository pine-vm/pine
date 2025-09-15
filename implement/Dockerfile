# Build dotnet build image
FROM mcr.microsoft.com/dotnet/sdk:9.0.305 AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./

# Build the files to bundle with the executable file
RUN dotnet  run  --project  ./prebuild/prebuild.csproj

WORKDIR /app/pine
RUN dotnet  publish  -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:9.0.9 AS binaries

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
