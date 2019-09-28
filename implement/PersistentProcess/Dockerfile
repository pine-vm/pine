# Build dotnet build image
FROM microsoft/dotnet:2.2-sdk AS build-env
WORKDIR /app

# Copy everything and build
COPY . ./
WORKDIR /app/PersistentProcess.WebHost
RUN dotnet publish -c Debug -o out

# Build runtime image
FROM microsoft/dotnet:2.2-aspnetcore-runtime AS binaries

COPY --from=build-env /app/PersistentProcess.WebHost/out /kalmit/dotnet/

# Build the `web-app-config.zip` for the default app.
FROM binaries AS build-default-config

COPY ./example-elm-apps/default-full-stack-app /default-app/

WORKDIR /default-app
RUN dotnet "/kalmit/dotnet/PersistentProcess.WebHost.dll" build-config --frontend-web-elm-source="elm-app/src/FrontendWeb/Main.elm" --output="/build-output/web-app-config.zip"

# Reduce startup cost: Produce an image which has popular blobs already in cache: Continue with state after having built a configuration.
FROM build-default-config AS with-default-app

COPY --from=build-default-config /build-output/web-app-config.zip /kalmit/

WORKDIR /kalmit

ENV APPSETTING_webAppConfigurationFilePath="/kalmit/web-app-config.zip"
ENV APPSETTING_processStoreDirectoryPath="/kalmit/process-store"

ENTRYPOINT ["dotnet", "/kalmit/dotnet/PersistentProcess.WebHost.dll"]

# ENV APPSETTING_adminRootPassword="password-for-kalmit-root"
