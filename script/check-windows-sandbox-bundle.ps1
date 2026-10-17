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
  # Drive the actual build/install entry point with a tiny independent builder.
  $builder = Join-Path $Root 'fixture-builder.ps1'
  $buildLog = Join-Path $Root 'build-directory.txt'
  $savedBuildLog = $env:AUTOLITH_BUNDLE_TEST_BUILD_LOG
  $env:AUTOLITH_BUNDLE_TEST_BUILD_LOG = $buildLog
  try {
    @'
param([Parameter(Mandatory=$true)][string]$OutputDirectory)
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
} else {
  'Native private ACL and MoveFileEx publication cases require Windows.'
}
'Sandbox bundle behavior checks passed.'
