param(
  [string]$RepoPath = "C:\bofe\R_project_git",
  [switch]$SkipRtools,
  [switch]$SkipPackageInstall,
  [switch]$RunPipeline,
  [int]$BootstrapWorkers = 2
)

$ErrorActionPreference = "Stop"
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass -Force
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

$RInstallDir = "C:\R\R-current"
$RtoolsInstallDir = "C:\rtools"
$DownloadDir = Join-Path $env:TEMP "bofe-bootstrap"
New-Item -ItemType Directory -Force -Path $DownloadDir | Out-Null

function Write-Log {
  param([string]$Message)
  Write-Host ("[{0}] {1}" -f (Get-Date -Format "yyyy-MM-dd HH:mm:ss"), $Message)
}

function Resolve-CranInstallerUrl {
  param(
    [string]$PageUrl,
    [string]$RegexPattern,
    [string]$PrefixUrl
  )

  $page = Invoke-WebRequest -Uri $PageUrl -UseBasicParsing
  $href = $page.Links.href | Where-Object { $_ -match $RegexPattern } | Select-Object -First 1
  if (-not $href) {
    $href = ([regex]::Match($page.Content, $RegexPattern)).Value
  }
  if (-not $href) {
    throw "Could not find a download link on $PageUrl."
  }
  if ($href -notmatch '^https?://') {
    $href = ($PrefixUrl.TrimEnd('/') + '/' + $href.TrimStart('/'))
  }
  $href
}

function Install-MsiOrInno {
  param(
    [string]$InstallerPath,
    [string[]]$Arguments
  )

  Write-Log ("Installing from {0}" -f $InstallerPath)
  $proc = Start-Process -FilePath $InstallerPath -ArgumentList $Arguments -Wait -PassThru
  if ($proc.ExitCode -ne 0) {
    throw "Installer exited with code $($proc.ExitCode)."
  }
}

function Add-MachinePathSegment {
  param([string]$Segment)

  if (-not (Test-Path $Segment)) { return }

  $machinePath = [Environment]::GetEnvironmentVariable("Path", "Machine")
  $segments = $machinePath -split ';' | Where-Object { $_ -and $_.Trim() }
  if ($segments -notcontains $Segment) {
    $newPath = (($segments + $Segment) | Select-Object -Unique) -join ';'
    [Environment]::SetEnvironmentVariable("Path", $newPath, "Machine")
  }
  if ($env:Path -notlike "*$Segment*") {
    $env:Path = "$env:Path;$Segment"
  }
}

function Find-Rscript {
  if (Get-Command Rscript -ErrorAction SilentlyContinue) {
    return (Get-Command Rscript).Source
  }

  $candidate = Join-Path $RInstallDir "bin\x64\Rscript.exe"
  if (Test-Path $candidate) { return $candidate }

  $fallback = Get-ChildItem -Path "C:\R" -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue |
    Sort-Object FullName -Descending |
    Select-Object -First 1
  if ($fallback) { return $fallback.FullName }

  throw "Rscript.exe was not found after installation."
}

Write-Log "Checking for Rscript."
if (-not (Get-Command Rscript -ErrorAction SilentlyContinue)) {
  Write-Log "Rscript is not on PATH; installing R from CRAN."
  New-Item -ItemType Directory -Force -Path (Split-Path -Parent $RInstallDir) | Out-Null
  $rUrl = "https://cran.r-project.org/bin/windows/base/R-4.6.0-win.exe"
  $rInstaller = Join-Path $DownloadDir "R-win.exe"
  Invoke-WebRequest -Uri $rUrl -OutFile $rInstaller
  Install-MsiOrInno -InstallerPath $rInstaller -Arguments @(
    "/VERYSILENT",
    "/SUPPRESSMSGBOXES",
    "/NORESTART",
    "/DIR=$RInstallDir"
  )
} else {
  Write-Log "Rscript is already on PATH."
}

if (-not $SkipRtools) {
  Write-Log "Installing Rtools from CRAN."
  $rtoolsUrl = "https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6768-6492.exe"
  $rtoolsInstaller = Join-Path $DownloadDir "Rtools.exe"
  Invoke-WebRequest -Uri $rtoolsUrl -OutFile $rtoolsInstaller
  New-Item -ItemType Directory -Force -Path $RtoolsInstallDir | Out-Null
  Install-MsiOrInno -InstallerPath $rtoolsInstaller -Arguments @(
    "/VERYSILENT",
    "/SUPPRESSMSGBOXES",
    "/NORESTART",
    "/DIR=$RtoolsInstallDir"
  )
}

$RscriptExe = Find-Rscript
Add-MachinePathSegment (Split-Path -Parent $RscriptExe)
Add-MachinePathSegment (Join-Path $RtoolsInstallDir "bin")
Add-MachinePathSegment (Join-Path $RtoolsInstallDir "mingw64\bin")
Add-MachinePathSegment (Join-Path $RtoolsInstallDir "usr\bin")

if (-not $SkipPackageInstall) {
  Write-Log "Installing CRAN packages used by the BOFE pipeline."
  $packages = @(
    "tidyverse",
    "haven",
    "labelled",
    "mice",
    "geepack",
    "lme4",
    "broom.mixed"
  )
  $pkgList = ($packages | ForEach-Object { '"' + $_ + '"' }) -join ', '
  $installExpr = @"
options(repos = c(CRAN = 'https://cloud.r-project.org'))
pkgs <- c($pkgList)
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, dependencies = TRUE, Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L))
}
cat('Installed packages:', paste(pkgs, collapse = ', '), '\n')
"@
  & $RscriptExe -e $installExpr
}

