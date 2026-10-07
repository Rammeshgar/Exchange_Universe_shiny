param([int]$Port = 4888)
$ErrorActionPreference = 'Stop'
$rCommand = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if ($rCommand) {
    $rExecutable = $rCommand.Source
} else {
    $rCandidates = Get-ChildItem -LiteralPath 'C:\Program Files\R' -Directory -ErrorAction SilentlyContinue |
        Sort-Object Name -Descending |
        ForEach-Object { Join-Path $_.FullName 'bin\Rscript.exe' } |
        Where-Object { Test-Path -LiteralPath $_ }
    $rExecutable = $rCandidates | Select-Object -First 1
}
if (-not $rExecutable) { throw 'R was not found. Install R or add Rscript.exe to PATH.' }
Push-Location -LiteralPath $PSScriptRoot
try {
    $rMinorVersion = Split-Path (Split-Path $rExecutable -Parent) -Parent | Split-Path -Leaf
    if ($rMinorVersion -match 'R-(\d+\.\d+)') {
        $rUserLibraryCandidates = @(
            (Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) "R\win-library\$($Matches[1])"),
            (Join-Path $env:USERPROFILE "AppData\Local\R\win-library\$($Matches[1])")
        )
        $rUserLibrary = $rUserLibraryCandidates | Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
        if ($rUserLibrary) { $env:R_LIBS_USER = $rUserLibrary }
    }
    Write-Host "Exchange Universe: http://127.0.0.1:$Port"
    & $rExecutable (Join-Path $PSScriptRoot 'scripts\run-local.R') $Port
    if ($LASTEXITCODE -ne 0) { throw "R exited with code $LASTEXITCODE." }
} finally {
    Pop-Location
}
