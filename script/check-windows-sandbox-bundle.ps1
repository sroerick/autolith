param([Parameter(Mandatory=$true)][string]$Root)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. (Join-Path (Split-Path -Parent $PSScriptRoot) 'bin\autolith-sandbox.ps1')
function Assert([bool]$Value, [string]$Message) { if (-not $Value) { throw $Message } }
function Assert-Failure([scriptblock]$Action) {
  $failed = $false
  try { & $Action | Out-Null } catch { $failed = $true }
  Assert $failed 'Expected bundle validation failure.'
}
$source = Join-Path $Root 'workspace\build'
$private = Join-Path $Root 'private\sandbox'
New-Item -ItemType Directory -Force -Path $source,$private | Out-Null
foreach ($name in $AutolithSandboxFiles) { [IO.File]::WriteAllText((Join-Path $source $name), "fixture-$name") }
$identity = Get-AutolithSandboxIdentity $source
# Preserve the identity format used by Get-FileHash for readable bundle members.
$referenceRecords = foreach ($name in $AutolithSandboxFiles) {
  $hash = (Get-FileHash -LiteralPath (Join-Path $source $name) -Algorithm SHA256 -ErrorAction Stop).Hash.ToLowerInvariant()
  "$name=$hash`n"
}
$sha = [Security.Cryptography.SHA256]::Create()
try { $referenceIdentity = ([BitConverter]::ToString($sha.ComputeHash([Text.Encoding]::UTF8.GetBytes(($referenceRecords -join ''))))).Replace('-', '').ToLowerInvariant() }
finally { $sha.Dispose() }
Assert ($identity -ceq $referenceIdentity) 'Stream hashing changed the bundle identity.'
# Every hash reader must be closed before another caller requests exclusive access.
foreach ($name in $AutolithSandboxFiles) {
  $stream = [IO.File]::Open((Join-Path $source $name), 'Open', 'Read', 'None')
  $stream.Dispose()
}
$generation = Join-Path $private $identity
New-Item -ItemType Directory -Path $generation | Out-Null
foreach ($name in $AutolithSandboxFiles) { Copy-Item -LiteralPath (Join-Path $source $name) -Destination $generation }
$selector = Join-Path $private 'selected'
[IO.File]::WriteAllText($selector, $identity)
$helper = Get-AutolithSandboxSelection $private
Assert ($helper -eq (Join-Path $generation 'cl-exec-sandbox-windows.exe')) 'Selection must identify one generation.'
# Leftovers from an interrupted copy or selector publication are never selected.
New-Item -ItemType Directory -Path (Join-Path $private '.stage-interrupted') | Out-Null
[IO.File]::WriteAllText((Join-Path $private '.select-interrupted'), 'incomplete')
Assert ((Get-AutolithSandboxSelection $private) -eq $helper) 'Interrupted staging changed selection.'
[IO.File]::WriteAllText($selector, '..\build')
Assert-Failure { Get-AutolithSandboxSelection $private }
[IO.File]::WriteAllText($selector, $identity)
Remove-Item -LiteralPath (Join-Path $generation 'cl-exec-sandbox-compat32.dll')
Assert-Failure { Get-AutolithSandboxSelection $private }
Copy-Item -LiteralPath (Join-Path $source 'cl-exec-sandbox-compat32.dll') -Destination $generation
[IO.File]::AppendAllText((Join-Path $source 'LICENSE-Detours.txt'), 'changed notice')
Assert ((Get-AutolithSandboxIdentity $source) -ne $identity) 'Notices must participate in bundle identity.'
$saved = $env:CL_EXEC_SANDBOX_WINDOWS_HELPER
try {
  $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = 'explicit-user-helper.exe'
  Set-AutolithSandboxDefault $private
  Assert ($env:CL_EXEC_SANDBOX_WINDOWS_HELPER -eq 'explicit-user-helper.exe') 'Explicit override was replaced.'
  $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = ''
  Set-AutolithSandboxDefault $private
  Assert ($env:CL_EXEC_SANDBOX_WINDOWS_HELPER -eq $helper) 'Empty override must resolve the selected bundle.'
} finally { $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = $saved }
# Faults retain their native codes through PowerShell's exception wrappers.
foreach ($code in @(5, 32, 33)) {
  $state = @{ Attempts = 0 }
  $result = Invoke-AutolithSandboxSharingRetry {
    $state.Attempts++
    if ($state.Attempts -lt 3) { throw [ComponentModel.Win32Exception]::new($code) }
    'released'
  }
  Assert ($result -eq 'released' -and $state.Attempts -eq 3) 'Transient publication failure was not retried.'
}
$wrappedSharing = [Management.Automation.MethodInvocationException]::new(
  'Directory move failed.', [IO.IOException]::new('Injected sharing violation.', -2147024864))
Assert ((Get-AutolithSandboxErrorCode $wrappedSharing) -eq 32) 'Wrapped HRESULT lost its native sharing code.'
$state = @{ Attempts = 0 }
Assert-Failure {
  Invoke-AutolithSandboxSharingRetry {
    $state.Attempts++
    throw [ComponentModel.Win32Exception]::new(2)
  }
}
Assert ($state.Attempts -eq 1) 'A permanent publication failure was retried.'
$clock = [Diagnostics.Stopwatch]::StartNew()
Assert-Failure {
  Invoke-AutolithSandboxSharingRetry { throw [ComponentModel.Win32Exception]::new(32) } -TimeoutMilliseconds 100
}
Assert ($clock.ElapsedMilliseconds -ge 100 -and $clock.ElapsedMilliseconds -lt 2000) 'Sharing retry did not respect its bounded budget.'
$firstStage = Join-Path $private '.stage-first'
$firstGeneration = Join-Path $Root 'first-generation'
New-Item -ItemType Directory -Path $firstStage | Out-Null
foreach ($name in $AutolithSandboxFiles) { Copy-Item -LiteralPath (Join-Path $generation $name) -Destination $firstStage }
Publish-AutolithSandboxGeneration $firstStage $firstGeneration $identity
Assert (-not (Test-Path -LiteralPath $firstStage)) 'First publication did not rename its stage.'
Assert ((Get-AutolithSandboxIdentity $firstGeneration) -eq $identity) 'First publication changed bundle bytes.'
# A concurrent winner must have exactly the expected complete content.
$loser = Join-Path $private '.stage-loser'
New-Item -ItemType Directory -Path $loser | Out-Null
Publish-AutolithSandboxGeneration $loser $generation $identity
Assert (Test-Path -LiteralPath $loser) 'Winner reuse consumed an unpublished stage.'
Assert-Failure { Publish-AutolithSandboxGeneration $loser $generation ('0' * 64) }
Assert ((Get-AutolithSandboxIdentity $generation) -eq $identity) 'Mismatch handling modified the winning generation.'
Remove-Item -LiteralPath $loser -Force
if ([Environment]::OSVersion.Platform -eq [PlatformID]::Win32NT) {
  $workspace = Join-Path $Root 'workspace'
  $installed = Install-AutolithSandboxBundle $source $private $workspace
  Assert ($installed -ne $helper) 'New bytes need a new generation.'
  Assert ((Get-AutolithSandboxSelection $private) -eq $installed) 'New bundle was not selected.'
  Assert ((Get-AutolithSandboxIdentity $generation) -eq $identity) 'The old generation was modified.'
  Assert ((Install-AutolithSandboxBundle $source $private $workspace) -eq $installed) 'Identical installation was not idempotent.'
  Remove-Item -LiteralPath (Join-Path $source 'cl-exec-sandbox-compat64.dll')
  Assert-Failure { Install-AutolithSandboxBundle $source $private $workspace }
  Assert ((Get-AutolithSandboxSelection $private) -eq $installed) 'Failed installation changed selection.'
  Assert-Failure { Install-AutolithSandboxBundle $generation (Join-Path $workspace 'native') $workspace }
  $junction = Join-Path $Root 'redirected'
  New-Item -ItemType Junction -Path $junction -Target $private | Out-Null
  try { Assert-Failure { Install-AutolithSandboxBundle $generation (Join-Path $junction 'sandbox') $workspace } }
  finally { [IO.Directory]::Delete($junction) }
  Assert-Failure { Install-AutolithSandboxBundle $generation 'C:relative' $workspace }
  # Hold real native handles without FILE_SHARE_DELETE to block directory rename.
  Add-Type -TypeDefinition @'
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft.Win32.SafeHandles;
public static class AutolithSandboxSharingFixture {
  [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
  static extern SafeFileHandle CreateFile(string path, uint access, uint share,
      IntPtr security, uint disposition, uint flags, IntPtr template);
  public static SafeFileHandle HoldDirectory(string path) {
    // GENERIC_READ participates in sharing checks; a metadata-only handle does not.
    var handle = CreateFile(path, 0x80000000, 3, IntPtr.Zero, 3, 0x02000000, IntPtr.Zero);
    if (handle.IsInvalid) throw new Win32Exception(Marshal.GetLastWin32Error());
    return handle;
  }
  public static void ReleaseAfter(IDisposable handle, int milliseconds) {
    var thread = new Thread(() => { Thread.Sleep(milliseconds); handle.Dispose(); });
    thread.IsBackground = true;
    thread.Start();
  }
}
'@
  $sharingStage = Join-Path $private '.stage-sharing'
  $sharingGeneration = Join-Path $Root 'sharing-generation'
  New-Item -ItemType Directory -Path $sharingStage | Out-Null
  foreach ($name in $AutolithSandboxFiles) { Copy-Item -LiteralPath (Join-Path $generation $name) -Destination $sharingStage }
  $handle = [AutolithSandboxSharingFixture]::HoldDirectory($sharingStage)
  try {
    $blocked = $false
    try { [IO.Directory]::Move($sharingStage, $sharingGeneration) }
    catch {
      Assert ((Get-AutolithSandboxErrorCode $_.Exception) -in @(5, 32, 33)) 'Native rename failed for an unexpected reason.'
      $blocked = $true
    }
    Assert $blocked 'The native directory handle did not block rename.'
    [AutolithSandboxSharingFixture]::ReleaseAfter($handle, 250)
    Publish-AutolithSandboxGeneration $sharingStage $sharingGeneration $identity
    Assert ((Get-AutolithSandboxIdentity $sharingGeneration) -eq $identity) 'Retried rename published different bytes.'
  } finally { $handle.Dispose() }
  # A sharing violation while hashing an already-published winner is retried too.
  $handle = [IO.File]::Open((Join-Path $sharingGeneration $AutolithSandboxFiles[0]), 'Open', 'Read', 'None')
  try {
    $blocked = $false
    try { Get-AutolithSandboxIdentity $sharingGeneration | Out-Null }
    catch {
      Assert ((Get-AutolithSandboxErrorCode $_.Exception) -eq 32) 'Native hash failure lost its sharing code.'
      $blocked = $true
    }
    Assert $blocked 'An exclusive file handle did not block hashing.'
    [AutolithSandboxSharingFixture]::ReleaseAfter($handle, 250)
    Publish-AutolithSandboxGeneration $sharingStage $sharingGeneration $identity
  } finally { $handle.Dispose() }
  Assert (-not (Test-Path -LiteralPath $sharingStage)) 'Retry recreated staging after a successful rename.'
  # Drive the actual build/install entry point with a tiny independent builder.
  $builder = Join-Path $Root 'fixture-builder.ps1'
  $buildLog = Join-Path $Root 'build-directory.txt'
  $savedBuildLog = $env:AUTOLITH_BUNDLE_TEST_BUILD_LOG
  $env:AUTOLITH_BUNDLE_TEST_BUILD_LOG = $buildLog
  try {
    @'
param([Parameter(Mandatory=$true)][string]$OutputDirectory)
if (Test-Path -LiteralPath $OutputDirectory) { throw 'Builder output must be new.' }
New-Item -ItemType Directory -Path $OutputDirectory | Out-Null
[IO.File]::WriteAllText($env:AUTOLITH_BUNDLE_TEST_BUILD_LOG, $OutputDirectory)
foreach ($name in @('cl-exec-sandbox-windows.exe','cl-exec-sandbox-compat64.dll','cl-exec-sandbox-compat32.dll','LICENSE-Detours.txt')) {
  [IO.File]::WriteAllText((Join-Path $OutputDirectory $name), "unique-build-$name")
}
'@ | Set-Content -LiteralPath $builder -Encoding UTF8
    $powershell = if ($env:POWERSHELL) { $env:POWERSHELL } else { 'powershell.exe' }
    $installer = Join-Path $PSScriptRoot 'windows-sandbox.ps1'
    & $powershell -NoProfile -ExecutionPolicy Bypass -File $installer -Action BuildInstall -Builder $builder -Root $private -Workspace $workspace
    Assert ($LASTEXITCODE -eq 0) 'Unique-output build/install failed.'
    $outputDirectory = [IO.File]::ReadAllText($buildLog)
    Assert ([IO.Path]::IsPathRooted($outputDirectory)) 'Builder received a relative output directory.'
    Assert (-not (Test-Path -LiteralPath $outputDirectory)) 'Temporary build output was not removed.'
    $built = Get-AutolithSandboxSelection $private
    Assert ($built -ne $installed) 'The private build did not publish a new generation.'
    Assert (Test-Path -LiteralPath $installed) 'The previous installed generation was removed.'
    Add-Content -LiteralPath $builder -Value 'exit 9'
    & $powershell -NoProfile -ExecutionPolicy Bypass -File $installer -Action BuildInstall -Builder $builder -Root $private -Workspace $workspace
    Assert ($LASTEXITCODE -ne 0) 'Failed builder was accepted.'
    $failedOutput = [IO.File]::ReadAllText($buildLog)
    Assert ($failedOutput -ne $outputDirectory) 'Build output was reused.'
    Assert (-not (Test-Path -LiteralPath $failedOutput)) 'Failed build output was not removed.'
    Assert ((Get-AutolithSandboxSelection $private) -eq $built) 'Failed builder changed selection.'
  } finally { $env:AUTOLITH_BUNDLE_TEST_BUILD_LOG = $savedBuildLog }
    # Several installers must be able to publish distinct complete generations concurrently.
    $concurrentRoot = Join-Path $Root 'concurrent'
    $concurrentWorkspace = Join-Path $Root 'concurrent-workspace'
    New-Item -ItemType Directory -Force -Path $concurrentWorkspace | Out-Null
    $concurrentSources = foreach ($number in 1..3) {
      $directory = Join-Path $Root ('concurrent-source-' + $number)
      New-Item -ItemType Directory -Path $directory | Out-Null
      foreach ($name in $AutolithSandboxFiles) {
        [IO.File]::WriteAllText((Join-Path $directory $name), "concurrent-$number-$name")
      }
      [PSCustomObject]@{ Directory = $directory; Identity = Get-AutolithSandboxIdentity $directory }
    }
    $processes = @()
    try {
      foreach ($fixture in $concurrentSources) {
        $arguments = '-NoProfile -ExecutionPolicy Bypass -File "{0}" -Action Install -Source "{1}" -Root "{2}" -Workspace "{3}"' -f $installer, $fixture.Directory, $concurrentRoot, $concurrentWorkspace
        $process = [Diagnostics.Process]::new()
        $process.StartInfo = [Diagnostics.ProcessStartInfo]::new()
        $process.StartInfo.FileName = $powershell
        $process.StartInfo.Arguments = $arguments
        $process.StartInfo.UseShellExecute = $false
        $process.StartInfo.RedirectStandardError = $true
        $process.StartInfo.CreateNoWindow = $true
        Assert ($process.Start()) 'Concurrent installer did not start.'
        $process | Add-Member -NotePropertyName ErrorRead -NotePropertyValue ($process.StandardError.ReadToEndAsync())
        $processes += $process
      }
      $deadline = [DateTime]::UtcNow.AddSeconds(30)
      while ($processes | Where-Object { -not $_.HasExited }) {
        Assert ([DateTime]::UtcNow -lt $deadline) 'Concurrent publication timed out.'
        $selectorPath = Join-Path $concurrentRoot 'selected'
        if (Test-Path -LiteralPath $selectorPath) {
          try {
            $selected = Get-AutolithSandboxSelection $concurrentRoot
            Assert (Test-Path -LiteralPath $selected) 'Concurrent reader saw a missing selected helper.'
          } catch {
            throw "Concurrent reader observed an incomplete publication: $($_.Exception.Message)"
          }
        }
        Start-Sleep -Milliseconds 25
      }
      foreach ($process in $processes) {
        $process.WaitForExit()
        $diagnostics = $process.ErrorRead.GetAwaiter().GetResult()
        Assert ($process.ExitCode -eq 0) "Concurrent installer failed with exit code $($process.ExitCode): $diagnostics"
      }
      $selected = Get-AutolithSandboxSelection $concurrentRoot
      Assert (Test-Path -LiteralPath $selected) 'Concurrent publication selected no helper.'
      foreach ($fixture in $concurrentSources) {
        Assert (Test-Path -LiteralPath (Join-Path $concurrentRoot $fixture.Identity)) 'A published generation was lost.'
      }
      Assert (Test-Path -LiteralPath $installed) 'The previous generation was not retained after concurrent publication.'
    } finally {
      foreach ($process in $processes) {
        if (-not $process.HasExited) { $process.Kill() }
        $process.Dispose()
      }
    }
} else {
  'Native private ACL and MoveFileEx publication cases require Windows.'
}
'Sandbox bundle behavior checks passed.'
exit 0
