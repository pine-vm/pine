FROM elm-fullstack-test

COPY . /demo-app/

WORKDIR /demo-app
RUN dotnet "/elm-fullstack/dotnet/elm-fullstack.dll" build-config --frontend-web-elm-make-appendix="--debug" --output="/elm-fullstack/web-app-config.zip"

