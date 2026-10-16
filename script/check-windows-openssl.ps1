# Filesystem-only fixtures, usable in Windows PowerShell 5.1 and PowerShell 7.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. (Join-Path (Split-Path -Parent $PSScriptRoot) 'bin/autolith-openssl.ps1')

function Assert-OpenSsl([bool]$Condition, [string]$Message) {
  if (-not $Condition) { throw $Message }
  $script:checks++
}

function New-OpenSslPair([string]$Directory, [int]$Major) {
  New-Item -ItemType Directory -Force -Path $Directory | Out-Null
  foreach ($library in 'crypto', 'ssl') {
    Set-Content -LiteralPath (Join-Path $Directory "lib$library-$Major-x64.dll") -Value "$library $Major"
  }
}

$checks = 0
$temporary = Join-Path ([IO.Path]::GetTempPath()) ( 'autolith-openssl-' + [guid]::NewGuid().ToString('N'))
$oldOpenSslDirectory = $env:AUTOLITH_OPENSSL_DIRECTORY
try {
  $source = Join-Path $temporary 'source'
  $local = Join-Path $source 'native/openssl'
  $override = Join-Path $temporary 'override with spaces'
  $git = Join-Path $temporary 'Git/cmd/git.exe'
  $gitBin = Join-Path $temporary 'Git/mingw64/bin'
  New-Item -ItemType Directory -Force -Path $source | Out-Null
  # Supply an absent Git path and empty override to isolate the host installation.
  $arguments = @{ SourceRoot = $source; OpenSslDirectory = ''; GitCommand = $git }
  Assert-OpenSsl ($null -eq (Find-AutolithOpenSsl @arguments)) 'Absence must return null.'

  foreach ($major in 3, 4) {
    New-OpenSslPair $gitBin $major
    $pair = Find-AutolithOpenSsl @arguments
    Assert-OpenSsl ($pair.Major -eq $major) "OpenSSL $major was not selected."
    Assert-OpenSsl ((Split-Path -Leaf $pair.Crypto) -eq "libcrypto-$major-x64.dll") 'Wrong crypto ABI.'
    Assert-OpenSsl ((Split-Path -Leaf $pair.Ssl) -eq "libssl-$major-x64.dll") 'Wrong SSL ABI.'
    $bundle = Join-Path $temporary "bundle-$major"
    New-Item -ItemType Directory -Path $bundle | Out-Null
    foreach ($dll in $pair.Crypto, $pair.Ssl) { Copy-Item -LiteralPath $dll -Destination $bundle }
    Assert-OpenSsl (@(Get-ChildItem -LiteralPath $bundle).Count -eq 2) 'Bundle must contain exactly one pair.'
    Remove-Item -LiteralPath $gitBin -Recurse -Force
  }

  New-OpenSslPair $gitBin 4
  New-OpenSslPair $local 3
  $pair = Find-AutolithOpenSsl @arguments
  Assert-OpenSsl ($pair.Directory -eq [IO.Path]::GetFullPath($local)) 'Local OpenSSL must precede Git.'
  New-OpenSslPair $override 3
  $arguments.OpenSslDirectory = $override
  $env:AUTOLITH_OPENSSL_DIRECTORY = $override
  $pair = Find-AutolithOpenSsl -SourceRoot $source -GitCommand $git
  Assert-OpenSsl ($pair.Directory -eq [IO.Path]::GetFullPath($override)) 'Environment override must precede local OpenSSL.'
  $pair = Find-AutolithOpenSsl @arguments
  Assert-OpenSsl ($pair.Directory -eq [IO.Path]::GetFullPath($override)) 'Override must precede local OpenSSL.'
  New-OpenSslPair $override 4
  Assert-OpenSsl ((Find-AutolithOpenSsl @arguments).Major -eq 4) 'Prefer 4 when both complete ABIs exist.'

  # Partial pairs fail instead of combining libraries across ABIs or directories.
  foreach ($names in @(
    @('libcrypto-3-x64.dll'),
    @('libssl-4-x64.dll'),
    @('libcrypto-4-x64.dll', 'libssl-3-x64.dll'),
    @('libcrypto-3-x64.dll', 'libssl-3-x64.dll', 'libcrypto-4-x64.dll')
  )) {
    Remove-Item -LiteralPath $override -Recurse -Force
    New-Item -ItemType Directory -Path $override | Out-Null
    foreach ($name in $names) { Set-Content -LiteralPath (Join-Path $override $name) -Value 'fixture' }
    $rejected = $false
    try { $null = Find-AutolithOpenSsl @arguments } catch {
      if ($_.Exception.Message -notlike 'Incomplete OpenSSL * pair in *') { throw }
      $rejected = $true
    }
    Assert-OpenSsl $rejected 'Partial or mismatched ABI must fail, even with a valid lower-priority directory.'
  }
  Remove-Item -LiteralPath $override -Recurse -Force
  Assert-OpenSsl ((Find-AutolithOpenSsl @arguments).Directory -eq [IO.Path]::GetFullPath($local)) 'Missing override must fall through.'
  Write-Host "OpenSSL discovery: $checks checks passed in PowerShell $($PSVersionTable.PSVersion)."
} finally {
  $env:AUTOLITH_OPENSSL_DIRECTORY = $oldOpenSslDirectory
  if (Test-Path -LiteralPath $temporary) { Remove-Item -LiteralPath $temporary -Recurse -Force }
}
