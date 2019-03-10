# Based on guide from https://docs.docker.com/engine/examples/dotnetcore/#create-a-dockerfile-for-an-aspnet-core-application

# Build dotnet build image
FROM microsoft/dotnet:2.2-sdk AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/PersistentProcess.WebHost
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM microsoft/dotnet:2.2-aspnetcore-runtime AS deploy-env

# Paths relative to the app entry which is placed in a subdirectory.
ENV APPSETTING_webAppConfigurationFilePath="./../web-app-config.zip"
ENV APPSETTING_processStoreDirectoryPath="./../process-store"

COPY --from=build-env /app/PersistentProcess.WebHost/out ./app/
ENTRYPOINT ["dotnet", "./app/PersistentProcess.WebHost.dll"]

# Configure a default app. Replace this file with a zip archive which contains your app.
COPY ./default-config/web-app/web-app-config.zip .

# ENV APPSETTING_adminRootPassword="password-for-kalmit-root"
