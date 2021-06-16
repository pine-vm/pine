# 2021-06-16 Install elm-fs on Ubuntu

## Motivation

From https://github.com/elm-fullstack/elm-fullstack/issues/5

> Hi dear,
> 
> I'm trying to install elm-fs for running a project example,
> but without success..
> 
> my machine is: Ubuntu 20.10, 64-bit
> 
> thanks in advance

## How Can We Test?

Let's see how we can reproduce the reported problem.

```PS
docker run  --name=ubuntu  -t -i ubuntu:20.10  bash

apt-get update
apt-get install -y wget
apt-get install -y unzip

wget  https://github.com/elm-fullstack/elm-fullstack/releases/download/v2021-06-05/elm-fullstack-bin-6d96fca86dc807208e923caffb94a449d6f4b22d-linux-x64.zip

unzip  elm-fullstack-bin-6d96fca86dc807208e923caffb94a449d6f4b22d-linux-x64.zip

chmod a+x  elm-fs
```

Then running elm-fs results in an error:

```PS
root@5d520e80c116:/# ./elm-fs
Process terminated. Couldn't find a valid ICU package installed on the system. Set the configuration flag System.Globalization.Invariant to true if you want to run with no globalization support.
   at System.Environment.FailFast(System.String)
   at System.Globalization.GlobalizationMode.GetGlobalizationInvariantMode()
   at System.Globalization.GlobalizationMode..cctor()
   at System.Globalization.CultureData.CreateCultureWithInvariantData()
   at System.Globalization.CultureData.get_Invariant()
   at System.Globalization.CultureInfo..cctor()
   at System.StringComparer..cctor()
   at System.StringComparer.get_OrdinalIgnoreCase()
   at McMaster.Extensions.CommandLineUtils.CommandLineApplication..ctor(McMaster.Extensions.CommandLineUtils.CommandLineApplication, McMaster.Extensions.CommandLineUtils.HelpText.IHelpTextGenerator, McMaster.Extensions.CommandLineUtils.Abstractions.CommandLineContext)
   at McMaster.Extensions.CommandLineUtils.CommandLineApplication..ctor()
   at elm_fullstack.Program.Main(System.String[])
Aborted
```

Searching on this problem reveals:

+ https://github.com/dotnet/core/issues/5019
+ https://docs.microsoft.com/en-gb/dotnet/core/install/linux
+ https://docs.microsoft.com/en-gb/dotnet/core/install/linux-ubuntu

Use the commands from https://docs.microsoft.com/en-gb/dotnet/core/install/linux-ubuntu to work around the error shown above:

```PS
wget https://packages.microsoft.com/config/ubuntu/20.10/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb

apt-get update; \
  apt-get install -y apt-transport-https && \
  apt-get update && \
  apt-get install -y aspnetcore-runtime-3.1
```

After running these commands, `./elm-fs` ran successfully: It displayed the default help text.

However, it seemed that running the `install` command did not achieve the desired effect:

```PS
root@5d520e80c116:/# ./elm-fs  install
I added the path '/' to the 'PATH' environment variable for the current user account. You will be able to use the 'elm-fs' command in newer instances of the Command Prompt.
root@5d520e80c116:/# echo $PATH
/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
```

At the moment, I don't see a use case for depending on that libicu. Maybe removing that dependency is better for our application.

## Update After Working on The Implementation

Release 2021-06-16 fixes both issues. The installation on Linux now uses the approach implemented with https://github.com/elm-fullstack/elm-fullstack/commit/0774c4fc344b0cf3f2ea5b5e9ab8aee5f8d86d46
Copy the executable file into the directory `/bin/`

Following are the commands for installing on Ubuntu 20.10:

```PS
docker run  -p 5000:5000 -p 4000:4000  --name=ubuntu  -t -i ubuntu:20.10  bash

apt-get update && apt-get install -y wget && apt-get install -y unzip

wget  https://github.com/elm-fullstack/elm-fullstack/releases/download/v2021-06-16/elm-fullstack-bin-0774c4fc344b0cf3f2ea5b5e9ab8aee5f8d86d46-linux-x64.zip

unzip  elm-fullstack-bin-0774c4fc344b0cf3f2ea5b5e9ab8aee5f8d86d46-linux-x64.zip

chmod a+x  elm-fs

./elm-fs  install
```

The docker run command above also exposes ports for further testing and administration from outside. Skip the docker to work directly on the host system.

Now the system is ready to run productive commands like this one:

```text
elm-fs  run-server  --admin-password=test  --public-urls="http://*:5000"  --deploy-app-from=https://github.com/elm-fullstack/elm-fullstack/tree/6d96fca86dc807208e923caffb94a449d6f4b22d/implement/example-apps/docker-image-default-app
```
