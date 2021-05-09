# Waiting for Jungle Records to come in the Mail

This is a game I made in 72 hours for [Ludum Dare 48](https://ldjam.com/events/ludum-dare/48).

https://ldjam.com/events/ludum-dare/48/waiting-for-jungle-records-to-come-in-the-mail

It's a simple first person dungeon crawler with a rudimentary combat system and hand-drawn graphics.

It's written in F# and MonoGame.

## Release

```
dotnet publish -c Release -r win-x64   /p:PublishReadyToRun=false /p:TieredCompilation=false --self-contained
dotnet publish -c Release -r linux-x64 /p:PublishReadyToRun=false /p:TieredCompilation=false --self-contained
mkdir dist
cp -Recurse .\bin\Release\netcoreapp3.1\win-x64\publish .\dist\JungleRecords-windows-1.0
rcedit .\dist\JungleRecords-windows-1.0\JungleRecords.exe --set-icon Icon.ico
```
