function Symlink-File {
    $DestPath = $args[0]
    $SourcePath = $args[1]
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

function Unsymlink-File {
    $DestPath = $args[0]
    $SourcePath = $args[1]
    if (Test-Path $DestPath) {
        if ((Get-Item $SourcePath) -is [System.IO.DirectoryInfo]) {
            cmd /c rmdir "$DestPath"
        } else {
            cmd /c del "$DestPath"
        }
        echo "$DestPath has been unsymlinked"
    } else {
        Write-Warning "$DestPath doesn't exist"
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
                Symlink-File $DestPath $SourcePath
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

function Undeploy-Manifest {
    $ManifestFile = $args[0]
    
    echo "Undeploying $ManifestFile..."

    $Manifest = Import-Csv -Header ("file", "operation") -Delimiter ("|") -Path ".\$ManifestFile"
    $EmacsHome = $env:HOME
    foreach ($ManifestRow in $Manifest) {
        $DeployFile = $ManifestRow.file
        $DeployOp = $ManifestRow.operation
        $SourcePath = "$PSScriptRoot\$DeployFile"
        $DestPath = "$EmacsHome\$DeployFile"
        switch($DeployOp) {
            "symlink" {
                Unsymlink-File $DestPath $SourcePath
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
