# Build dotnet build image
FROM mcr.microsoft.com/dotnet/core/sdk:3.1 AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/elm-fullstack
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/core/aspnet:3.1 AS binaries

COPY --from=build-env /app/elm-fullstack/out /elm-fullstack/dotnet/

# Build the process with a deployment for the default app.
FROM binaries AS build-default-config

COPY ./example-apps/docker-image-default-app /docker-image-default-app/

RUN dotnet "/elm-fullstack/dotnet/elm-fs.dll"  deploy-app --from=/docker-image-default-app/  --site=/elm-fullstack/process-store  --init-app-state

WORKDIR /elm-fullstack

ENTRYPOINT ["dotnet", "/elm-fullstack/dotnet/elm-fs.dll", "run-server", "--process-store-directory-path=/elm-fullstack/process-store"]

# ENV APPSETTING_adminPassword="password-for-admin-interface"
