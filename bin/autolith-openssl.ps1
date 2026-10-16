# Shared Windows OpenSSL discovery before the Lisp runtime loads.
function Find-AutolithOpenSsl {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory=$true)][string]$SourceRoot,
    [string]$OpenSslDirectory = $env:AUTOLITH_OPENSSL_DIRECTORY,
    [string]$GitCommand
  )

  # Explicit override, checkout-local libraries, then Git for Windows.
  $candidates = @()
  if ($OpenSslDirectory) { $candidates += $OpenSslDirectory }
  $candidates += (Join-Path $SourceRoot 'native/openssl')
  if (-not $GitCommand) {
    $git = Get-Command git -CommandType Application -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($git) { $GitCommand = $git.Source }
  }
  if ($GitCommand) {
    $gitRoot = Split-Path -Parent (Split-Path -Parent $GitCommand)
    $candidates += (Join-Path $gitRoot 'mingw64/bin')
  }

  foreach ($directory in $candidates) {
    $pairs = @()
    foreach ($major in 4, 3) {
      $crypto = Join-Path $directory "libcrypto-$major-x64.dll"
      $ssl = Join-Path $directory "libssl-$major-x64.dll"
      $hasCrypto = Test-Path -LiteralPath $crypto -PathType Leaf
      $hasSsl = Test-Path -LiteralPath $ssl -PathType Leaf
      # Do not allow the native loader to combine a partial ABI with another
      # directory on PATH, even when this directory also has a complete pair.
      if ($hasCrypto -ne $hasSsl) {
        throw "Incomplete OpenSSL $major pair in '$directory': both libcrypto-$major-x64.dll and libssl-$major-x64.dll are required."
      }
      if ($hasCrypto) {
        $pairs += [pscustomobject]@{
          Directory = [IO.Path]::GetFullPath($directory)
          Major = $major
          Crypto = $crypto
          Ssl = $ssl
        }
      }
    }
    # Prefer OpenSSL 4 within the first usable directory.
    if ($pairs.Count) { return $pairs[0] }
  }
  return $null
}
