# Shared by source bootstrap, source launch, and packaged releases.
$AutolithSandboxFiles = @('cl-exec-sandbox-windows.exe', 'cl-exec-sandbox-compat64.dll', 'cl-exec-sandbox-compat32.dll', 'LICENSE-Detours.txt')

function Assert-AutolithSandboxPath([string]$Path) {
  # Reparse points in a private install would allow a later copy to escape it.
  $item = Get-Item -LiteralPath $Path -Force -ErrorAction Stop
  while ($null -ne $item) {
    if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) { throw "Sandbox bundle path is a reparse point: $($item.FullName)" }
    $parent = [IO.Directory]::GetParent($item.FullName)
    $item = if ($null -ne $parent) { Get-Item -LiteralPath $parent.FullName -Force -ErrorAction Stop } else { $null }
  }
}

function Get-AutolithSandboxIdentity([string]$Directory) {
  Assert-AutolithSandboxPath $Directory
  $records = foreach ($name in $AutolithSandboxFiles) {
    $path = Join-Path $Directory $name
    $item = Get-Item -LiteralPath $path -Force -ErrorAction Stop
    if ($item.PSIsContainer -or $item.Length -eq 0 -or ($item.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
      throw "Invalid sandbox bundle member: $path"
    }
    "$name=$((Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant())`n"
  }
  $sha = [Security.Cryptography.SHA256]::Create()
  try { return ([BitConverter]::ToString($sha.ComputeHash([Text.Encoding]::UTF8.GetBytes(($records -join ''))))).Replace('-', '').ToLowerInvariant() }
  finally { $sha.Dispose() }
}

function Get-AutolithSandboxSelection([string]$Root) {
  Assert-AutolithSandboxPath $Root
  $selector = Join-Path $Root 'selected'
  Assert-AutolithSandboxPath $selector
  $identity = [IO.File]::ReadAllText($selector).Trim()
  if ($identity -cnotmatch '^[0-9a-f]{64}$') { throw 'Invalid sandbox bundle selector.' }
  $directory = Join-Path $Root $identity
  if ((Get-AutolithSandboxIdentity $directory) -cne $identity) { throw 'Selected sandbox bundle is incomplete or corrupt.' }
  return (Join-Path $directory 'cl-exec-sandbox-windows.exe')

}
function Get-AutolithSandboxExplicitSelection([string]$Path) {
  $item = Get-Item -LiteralPath $Path -Force -ErrorAction Stop
  Assert-AutolithSandboxPath $item.FullName
  if ($item.PSIsContainer -or $item.Name -cne 'cl-exec-sandbox-windows.exe' -or
      $item.Length -eq 0 -or ($item.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    throw "Invalid explicit sandbox helper: $Path"
  }
  $identity = Get-AutolithSandboxIdentity $item.DirectoryName
  if (-not $identity) { throw "Explicit sandbox helper bundle is incomplete: $Path" }
  return $item.FullName
}

function Set-AutolithSandboxPrivateAcl([string]$Directory) {
  # Only the installing account, SYSTEM, and administrators can modify this tree.
  Assert-AutolithSandboxPath $Directory
  $acl = [Security.AccessControl.DirectorySecurity]::new()
  $acl.SetAccessRuleProtection($true, $false)
  foreach ($sid in @([Security.Principal.WindowsIdentity]::GetCurrent().User,
                     [Security.Principal.SecurityIdentifier]::new('S-1-5-18'),
                     [Security.Principal.SecurityIdentifier]::new('S-1-5-32-544'))) {
    $acl.AddAccessRule([Security.AccessControl.FileSystemAccessRule]::new($sid, 'FullControl', 'ContainerInherit,ObjectInherit', 'None', 'Allow'))
  }
  Set-Acl -LiteralPath $Directory -AclObject $acl
}

function Install-AutolithSandboxBundle([string]$Source, [string]$Root, [string]$Workspace) {
  if ($Root -notmatch '^(?:[A-Za-z]:[\\/]|\\\\[^\\/]+[\\/][^\\/]+)') {
    throw 'Sandbox install root must be absolute.'
  }
  $rootPath = [IO.Path]::GetFullPath($Root).TrimEnd('\')
  $workspacePath = [IO.Path]::GetFullPath($Workspace).TrimEnd('\')
  if ($rootPath.Equals($workspacePath, [StringComparison]::OrdinalIgnoreCase) -or
      $rootPath.StartsWith($workspacePath + '\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'Sandbox bundle must be installed outside the source workspace.'
  }
  # Validate existing ancestors before creating any directories.
  $ancestor = $rootPath
  while (-not (Test-Path -LiteralPath $ancestor)) { $ancestor = Split-Path -Parent $ancestor }
  Assert-AutolithSandboxPath $ancestor
  $identity = Get-AutolithSandboxIdentity $Source
  New-Item -ItemType Directory -Force -Path $rootPath | Out-Null
  Assert-AutolithSandboxPath $rootPath
  Set-AutolithSandboxPrivateAcl $rootPath
  $stage = Join-Path $rootPath ('.stage-' + [Guid]::NewGuid().ToString('N'))
  $selector = Join-Path $rootPath ('.select-' + [Guid]::NewGuid().ToString('N'))
  $generation = Join-Path $rootPath $identity
  try {
    New-Item -ItemType Directory -Path $stage | Out-Null
    foreach ($name in $AutolithSandboxFiles) { Copy-Item -LiteralPath (Join-Path $Source $name) -Destination (Join-Path $stage $name) }
    if ((Get-AutolithSandboxIdentity $stage) -cne $identity) { throw 'Sandbox bundle changed while copying.' }
    try { [IO.Directory]::Move($stage, $generation) }
    catch { if (-not (Test-Path -LiteralPath $generation)) { throw } }
    # A concurrent installer may have published the identical content first.
    if ((Get-AutolithSandboxIdentity $generation) -cne $identity) { throw 'Existing sandbox generation is corrupt.' }
    [IO.File]::WriteAllText($selector, $identity + "`n", [Text.Encoding]::ASCII)
    if (-not ('AutolithSandboxMove' -as [type])) {
      Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class AutolithSandboxMove {
  [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
  public static extern bool MoveFileEx(string source, string target, uint flags);
}
'@
    }
    # Same-directory MoveFileEx(REPLACE_EXISTING), as in platform-win32.lisp.
    if (-not [AutolithSandboxMove]::MoveFileEx($selector, (Join-Path $rootPath 'selected'), 1)) {
      throw [ComponentModel.Win32Exception]::new([Runtime.InteropServices.Marshal]::GetLastWin32Error())
    }
    return (Join-Path $generation 'cl-exec-sandbox-windows.exe')
  } finally {
    if (Test-Path -LiteralPath $stage) { Remove-Item -LiteralPath $stage -Recurse -Force }
    if (Test-Path -LiteralPath $selector) { Remove-Item -LiteralPath $selector -Force }
  }
}

function Get-AutolithSandboxRoot {
  $absolute = '^(?:[A-Za-z]:[\\/]|\\\\[^\\/]+[\\/][^\\/]+)'
  if ($env:XDG_DATA_HOME -and $env:XDG_DATA_HOME -match $absolute) {
    return (Join-Path $env:XDG_DATA_HOME 'autolith\native\sandbox')
  }
  if (-not $env:LOCALAPPDATA -or $env:LOCALAPPDATA -notmatch $absolute) {
    throw 'LOCALAPPDATA must name an absolute directory.'
  }
  return (Join-Path $env:LOCALAPPDATA 'autolith\data\native\sandbox')
}

function Set-AutolithSandboxDefault([string]$Root) {
  if (-not $env:CL_EXEC_SANDBOX_WINDOWS_HELPER) {
    $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = Get-AutolithSandboxSelection $Root
  }
}
