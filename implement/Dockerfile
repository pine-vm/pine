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

# Build the `web-app-config.zip` for the default app.
FROM binaries AS build-default-config

COPY ./PersistentProcess/example-elm-apps/default-full-stack-app /default-app/

WORKDIR /default-app
RUN dotnet "/elm-fullstack/dotnet/elm-fullstack.dll" build-config --frontend-web-elm-make-appendix="--debug" --output="/build-output/web-app-config.zip"

# Reduce startup cost: Produce an image which has popular blobs already in cache: Continue with state after having built a configuration.
FROM build-default-config AS with-default-app

COPY --from=build-default-config /build-output/web-app-config.zip /elm-fullstack/

WORKDIR /elm-fullstack

ENTRYPOINT ["dotnet", "/elm-fullstack/dotnet/elm-fullstack.dll", "run-server", "--process-store-directory-path=/elm-fullstack/process-store", "--web-app-configuration-file-path=/elm-fullstack/web-app-config.zip"]

# ENV APPSETTING_adminRootPassword="password-for-elm-fullstack-root"
