## Release

```
dotnet publish -c Release -r win-x64   /p:PublishReadyToRun=false /p:TieredCompilation=false --self-contained
dotnet publish -c Release -r linux-x64 /p:PublishReadyToRun=false /p:TieredCompilation=false --self-contained
mkdir dist
```
