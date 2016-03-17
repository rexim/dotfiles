Import-Module .\DotFiles.psm1

$HomeExists = Test-Path Env:HOME
if ($HomeExists -ne $True) {
    echo "Creating HOME environment variable and targeting it to $env:USERPROFILE"
    [Environment]::SetEnvironmentVariable("HOME", $env:USERPROFILE, "User")
} else {
    Write-Warning "HOME environment variable already exists. Not modifying the existing value."
}

Deploy-Manifest MANIFEST

pause
