param()

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $repoRoot

if (-not (Get-Command Rscript -ErrorAction SilentlyContinue)) {
  throw "Rscript was not found on PATH."
}

function Get-Timestamp {
  Get-Date -Format "yyyy-MM-dd HH:mm:ss"
}

$steps = @(
  "R/01_cleaning.R",
  "R/02_imputation.R",
  "R/03_descriptives.R",
  "R/04b_gee.R",
  "R/05_cost_effectiveness.R",
  "R/06_outputs.R",
  "R/07_manuscript_report.R"
)

$totalSteps = $steps.Count
Write-Host ("[{0}] [START] BOFE pipeline ({1} phases)" -f (Get-Timestamp), $totalSteps)

for ($i = 0; $i -lt $steps.Count; $i++) {
  $step = $steps[$i]
  $phaseName = [System.IO.Path]::GetFileNameWithoutExtension($step)
  $phaseNum = $i + 1

  Write-Host ("[{0}] [{1}/{2}] START {3}" -f (Get-Timestamp), $phaseNum, $totalSteps, $phaseName)
  & Rscript $step
  if ($LASTEXITCODE -ne 0) {
    throw "Phase $phaseName failed with exit code $LASTEXITCODE."
  }
  Write-Host ("[{0}] [{1}/{2}] DONE  {3}" -f (Get-Timestamp), $phaseNum, $totalSteps, $phaseName)
}

Write-Host ("[{0}] [DONE] BOFE pipeline complete." -f (Get-Timestamp))
