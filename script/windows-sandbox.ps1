param([Parameter(Mandatory=$true)][ValidateSet('BuildInstall','Install','Resolve')][string]$Action,
      [Parameter(Mandatory=$true)][string]$Root,
      [string]$Source,
      [string]$Workspace,
      [string]$Builder)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
[Console]::OutputEncoding = [Text.UTF8Encoding]::new($false)
. (Join-Path (Split-Path -Parent $PSScriptRoot) 'bin\autolith-sandbox.ps1')
if ($Action -eq 'BuildInstall') {
  $temporary = [IO.Path]::GetTempPath()
  Assert-AutolithSandboxPath $temporary
  $build = Join-Path $temporary ('autolith-sandbox-build-' + [Guid]::NewGuid().ToString('N'))
  New-Item -ItemType Directory -Path $build | Out-Null
  try {
    Set-AutolithSandboxPrivateAcl $build
    $powershell = if ($env:POWERSHELL) { $env:POWERSHELL } else { 'powershell.exe' }
    & $powershell -NoProfile -ExecutionPolicy Bypass -File $Builder -OutputDirectory $build
    if ($LASTEXITCODE -ne 0) { throw 'Windows sandbox bundle build failed.' }
    Install-AutolithSandboxBundle $build $Root $Workspace
  } finally { Remove-Item -LiteralPath $build -Recurse -Force }
} elseif ($Action -eq 'Install') { Install-AutolithSandboxBundle $Source $Root $Workspace }
else { Get-AutolithSandboxSelection $Root }
