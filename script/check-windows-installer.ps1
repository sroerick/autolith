# Exercise the installer with a local release archive in either PowerShell host.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$installer = Join-Path $PSScriptRoot 'install.ps1'
$root = Join-Path ([IO.Path]::GetTempPath()) ('autolith installer ' + [guid]::NewGuid())
$previousHome = $env:LOCALAPPDATA
$previousPath = [Environment]::GetEnvironmentVariable('Path', 'User')
$version = 'v0.0.0'
$name = "autolith-$version-x86_64-windows"

try {
  $source = Join-Path $root $name
  $sourceBin = Join-Path $source 'bin'
  New-Item -ItemType Directory -Force -Path $sourceBin | Out-Null
  Set-Content -Encoding ascii -LiteralPath (Join-Path $source 'RELEASE') -Value 'platform=x86_64-windows'
  Set-Content -Encoding ascii -LiteralPath (Join-Path $sourceBin 'autolith.cmd') -Value '@echo fixture launched'
  Set-Content -Encoding ascii -LiteralPath (Join-Path $sourceBin 'autolith.ps1') -Value @("'fixture launched'", 'exit 0')
  $archive = Join-Path $root "$name.zip"
  Compress-Archive -LiteralPath $source -DestinationPath $archive
  $checksum = "$archive.sha256"
  $hash = (Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash
  Set-Content -Encoding ascii -LiteralPath $checksum -Value "$hash *$name.zip"
  $env:LOCALAPPDATA = Join-Path $root 'local app data'
  $installation = Join-Path $env:LOCALAPPDATA 'autolith\installation'
  $bin = Join-Path $env:LOCALAPPDATA 'autolith\bin'
  $target = Join-Path $installation "releases\$version-x86_64-windows"
  $pointer = Join-Path $installation 'current'

  foreach ($attempt in 1..2) {
    & $installer -Version $version -ArchiveUrl $archive -ChecksumUrl $checksum
    if ((Get-Content -Raw -LiteralPath $pointer) -cne $target) {
      throw 'The installed release pointer does not name the extracted release.'
    }
    foreach ($launcher in 'autolith.ps1', 'autolith.cmd') {
      if (-not (Test-Path -LiteralPath (Join-Path $bin $launcher) -PathType Leaf)) {
        throw "The installer did not create $launcher."
      }
    }
    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    if (@($userPath -split ';' | Where-Object { $_ -eq $bin }).Count -ne 1) {
      throw 'The launcher directory must occur once in the user PATH.'
    }
  }
  $output = & (Join-Path $bin 'autolith.ps1')
  if ($LASTEXITCODE -ne 0 -or $output -ne 'fixture launched') {
    throw 'The installed PowerShell launcher did not run the selected release.'
  }

  Set-Content -Encoding ascii -LiteralPath $checksum -Value (('0' * 64) + " *$name.zip")
  $rejected = $false
  try {
    & $installer -Version $version -ArchiveUrl $archive -ChecksumUrl $checksum
  } catch {
    if ($_.Exception.Message -notlike '*archive checksum is wrong*') { throw }
    $rejected = $true
  }
  if (-not $rejected) { throw 'The installer accepted an archive with the wrong checksum.' }
  if ((Get-Content -Raw -LiteralPath $pointer) -cne $target) {
    throw 'A rejected archive replaced the installed release pointer.'
  }
  Write-Host "Installer checks passed in PowerShell $($PSVersionTable.PSVersion)."
} finally {
  $env:LOCALAPPDATA = $previousHome
  [Environment]::SetEnvironmentVariable('Path', $previousPath, 'User')
  if (Test-Path -LiteralPath $root) { Remove-Item -Recurse -Force -LiteralPath $root }
}
