param([Parameter(Mandatory=$true)][string]$Archive)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Remove-SandboxUserProfile {
  param([Parameter(Mandatory=$true)][string]$Sid,
        [ValidateRange(0,60)][int]$TimeoutSeconds = 60)
  # Reap background compiler processes owned by this disposable account.
  $deadline = [DateTime]::UtcNow.AddSeconds($TimeoutSeconds)
  while ($true) {
    foreach ($candidate in Get-CimInstance Win32_Process) {
      $owner = Invoke-CimMethod -InputObject $candidate -MethodName GetOwnerSid -ErrorAction SilentlyContinue
      if ($owner -and $owner.ReturnValue -eq 0 -and $owner.Sid -eq $Sid) {
        Stop-Process -Id $candidate.ProcessId -Force -ErrorAction SilentlyContinue
      }
    }
    $profile = Get-CimInstance Win32_UserProfile -Filter "SID='$Sid'"
    if (-not $profile) { return }
    try {
      $profile | Remove-CimInstance -ErrorAction Stop
      return
    } catch {
      if ([DateTime]::UtcNow -ge $deadline) { throw }
    }
    Start-Sleep -Milliseconds 500
  }
}

$name = "AutolithSandbox$PID"
$root = Join-Path $env:PUBLIC $name
$password = ConvertTo-SecureString (([Guid]::NewGuid().ToString('N')) + '!aA1') -AsPlainText -Force
$user = $null
$process = $null
try {
  $user = New-LocalUser -Name $name -Password $password -AccountNeverExpires
  New-Item -ItemType Directory -Path $root | Out-Null
  & icacls.exe $root /inheritance:r /grant:r "*$($user.SID.Value):(OI)(CI)F" '*S-1-5-18:(OI)(CI)F' '*S-1-5-32-544:(OI)(CI)F'
  if ($LASTEXITCODE -ne 0) { throw 'Cannot prepare standard-user sandbox test root.' }
  Expand-Archive -LiteralPath $Archive -DestinationPath $root
  $release = Join-Path $root ([IO.Path]::GetFileNameWithoutExtension($Archive))
  $runner = Join-Path $release 'libexec\autolith\script\check-windows-sandbox-user.ps1'
  $credential = [Management.Automation.PSCredential]::new("$env:COMPUTERNAME\$name", $password)
  $process = Start-Process -FilePath (Get-Command pwsh.exe).Source -Credential $credential -LoadUserProfile -PassThru -WorkingDirectory $root -ArgumentList @('-NoProfile','-File',"`"$runner`"",'-Release',"`"$release`"",'-Root',"`"$root`"")
  if (-not $process.WaitForExit(1200000)) {
    & taskkill.exe /PID $process.Id /T /F
    throw 'Packaged Windows sandbox tests timed out.'
  }
  $process.Refresh()
  $log = Join-Path $root 'sandbox.log'
  if (Test-Path -LiteralPath $log) { Get-Content -LiteralPath $log }
  if ($process.ExitCode -ne 0) { throw "Packaged standard-user sandbox tests failed: $($process.ExitCode)" }
} finally {
  if ($process) { $process.Dispose() }
  try {
    if ($user) { Remove-SandboxUserProfile -Sid $user.SID.Value }
  } finally {
    try {
      if ($user) { Remove-LocalUser -Name $name }
    } finally {
      if (Test-Path -LiteralPath $root) {
        # Replace protected core ACLs with the disposable root's inherited grants.
        & takeown.exe /F $root /A /R /D Y | Out-Null
        if ($LASTEXITCODE -ne 0) { throw 'Cannot reclaim sandbox test files.' }
        & icacls.exe (Join-Path $root '*') /reset /T /C /L /Q
        if ($LASTEXITCODE -ne 0) { throw 'Cannot reset sandbox test file permissions.' }
        & attrib.exe -R (Join-Path $root '*') /S /D /L
        if ($LASTEXITCODE -ne 0) { throw 'Cannot clear sandbox test file attributes.' }
        Remove-Item -Recurse -Force -LiteralPath $root
      }
    }
  }
}
