# Build dotnet build image
FROM microsoft/dotnet:2.2-sdk AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/PersistentProcess.WebHost
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM microsoft/dotnet:2.2-aspnetcore-runtime AS deploy-env

WORKDIR /kalmit

ENV APPSETTING_webAppConfigurationFilePath="/kalmit/web-app-config.zip"
ENV APPSETTING_processStoreDirectoryPath="/kalmit/process-store"

COPY --from=build-env /app/PersistentProcess.WebHost/out /kalmit/dotnet/
ENTRYPOINT ["dotnet", "/kalmit/dotnet/PersistentProcess.WebHost.dll"]

# Configure a default app. Replace this file with a zip archive which contains your app.
COPY ./default-config/web-app/web-app-config.zip /kalmit

# ENV APPSETTING_adminRootPassword="password-for-kalmit-root"
