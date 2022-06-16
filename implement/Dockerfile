# Build dotnet build image
FROM mcr.microsoft.com/dotnet/sdk:6.0.301 AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/elm-fullstack
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:6.0.6 AS binaries

COPY --from=build-env /app/elm-fullstack/out /elm-fullstack/dotnet/

# Build the process with a deployment for the default app.
FROM binaries AS build-default-config

RUN apt update
RUN apt install -y curl
# Support partial clone of git repositories: Install git as fallback implementation for cloning.
RUN apt install -y git

COPY ./example-apps/docker-image-default-app /docker-image-default-app/

RUN dotnet "/elm-fullstack/dotnet/elm-fs.dll"  deploy  /docker-image-default-app/  /elm-fullstack/process-store  --init-app-state

WORKDIR /elm-fullstack

ENTRYPOINT ["dotnet", "/elm-fullstack/dotnet/elm-fs.dll", "run-server", "--process-store=/elm-fullstack/process-store"]

# ENV APPSETTING_adminPassword="password-for-admin-interface"
