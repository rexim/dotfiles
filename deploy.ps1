function Symlink-File {
    $DestPath = $args[0]
    if (Test-Path $DestPath) {
        Write-Warning "$DestPath is already symlinked"
    } else {
        if ((Get-Item $SourcePath) -is [System.IO.DirectoryInfo]) {
            cmd /c mklink /D "$DestPath" "$SourcePath"
        } else {
            cmd /c mklink "$DestPath" "$SourcePath"
        }
        echo "$DestPath has been symlinked"
    }
}

function Deploy-Manifest {
    $ManifestFile = $args[0]
    
    echo "Deploying $ManifestFile..."

    $Manifest = Import-Csv -Header ("file", "operation") -Delimiter ("|") -Path ".\$ManifestFile"
    $EmacsHome = $env:HOME
    foreach ($ManifestRow in $Manifest) {
        $DeployFile = $ManifestRow.file
        $DeployOp = $ManifestRow.operation
        $SourcePath = "$PSScriptRoot\$DeployFile"
        $DestPath = "$EmacsHome\$DeployFile"
        switch($DeployOp) {
            "symlink" {
                Symlink-File "$DestPath"
            }
    
            "copy" {
                Write-Warning "The 'copy' operation is not implemented yet. Skipping..."
            }
    
            default {
                Write-Warning "Unknown operation $operation. Skipping..."
            }
        }
    }
}

$HomeExists = Test-Path Env:HOME
if ($HomeExists -ne $True) {
    echo "Creating HOME environment variable and targeting it to $env:USERPROFILE"
    [Environment]::SetEnvironmentVariable("HOME", $env:USERPROFILE, "User")
} else {
    Write-Warning "HOME environment variable already exists. Not modifying the existing value."
}

Deploy-Manifest MANIFEST

pause
