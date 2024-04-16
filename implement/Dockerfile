# Build dotnet build image
FROM mcr.microsoft.com/dotnet/sdk:8.0.204 AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/pine
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:8.0.4 AS binaries

COPY --from=build-env /app/pine/out /pine/dotnet/

# Build the process with a deployment for the default app.
FROM binaries AS build-default-config

RUN apt update
RUN apt install -y curl
# Support partial clone of git repositories: Install git as fallback implementation for cloning.
RUN apt install -y git

COPY ./example-apps/docker-image-default-app /docker-image-default-app/

RUN dotnet "/pine/dotnet/pine.dll"  deploy  /docker-image-default-app/  /pine/process-store  --init-app-state

WORKDIR /pine

ENTRYPOINT ["dotnet", "/pine/dotnet/pine.dll", "run-server", "--process-store=/pine/process-store"]

# ENV APPSETTING_adminPassword="password-for-admin-interface"
