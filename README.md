## Release

```
dotnet publish -c Release -r win-x64   /p:PublishReadyToRun=false /p:TieredCompilation=false --self-contained
dotnet publish -c Release -r linux-x64 /p:PublishReadyToRun=false /p:TieredCompilation=false --self-contained
mkdir dist
cp -Recurse .\bin\Release\netcoreapp3.1\win-x64\publish .\dist\JungleRecords-windows-1.0
rcedit .\dist\JungleRecords-windows-1.0\JungleRecords.exe --set-icon Icon.ico
```
