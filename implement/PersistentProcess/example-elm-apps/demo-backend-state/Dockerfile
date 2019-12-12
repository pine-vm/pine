FROM elm-fullstack-test

COPY . /demo-app/

WORKDIR /demo-app
RUN dotnet "/elm-fullstack/dotnet/PersistentProcess.WebHost.dll" build-config --frontend-web-elm-source="elm-app/src/FrontendWeb/Main.elm" --frontend-web-elm-make-appendix="--debug" --output="/elm-fullstack/web-app-config.zip"

