# Build dotnet build image
FROM mcr.microsoft.com/dotnet/sdk:8.0.100-rc.2 AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/elm-time
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:8.0.0-rc.2 AS binaries

COPY --from=build-env /app/elm-time/out /elm-time/dotnet/

# Build the process with a deployment for the default app.
FROM binaries AS build-default-config

RUN apt update
RUN apt install -y curl
# Support partial clone of git repositories: Install git as fallback implementation for cloning.
RUN apt install -y git

COPY ./example-apps/docker-image-default-app /docker-image-default-app/

RUN dotnet "/elm-time/dotnet/elm-time.dll"  deploy  /docker-image-default-app/  /elm-time/process-store  --init-app-state

WORKDIR /elm-time

ENTRYPOINT ["dotnet", "/elm-time/dotnet/elm-time.dll", "run-server", "--process-store=/elm-time/process-store"]

# ENV APPSETTING_adminPassword="password-for-admin-interface"
