FROM elm-fullstack-test

COPY . /demo-app/

WORKDIR /demo-app
RUN dotnet "/elm-fullstack/dotnet/elm-fullstack.dll" build-config --output="/elm-fullstack/web-app-config.zip"

