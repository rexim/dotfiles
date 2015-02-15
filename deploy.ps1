$Manifest = Import-Csv -Header ("file", "operation") -Delimiter ("|") -Path .\MANIFEST
$EmacsHome = Get-ChildItem Env:\APPDATA | %{ $_.VALUE }
foreach ($ManifestRow in $Manifest) {
    $DeployFile = $ManifestRow.file
    $DeployOp = $ManifestRow.operation
    $SourcePath = "$PSScriptRoot\$DeployFile"
    $DestPath = "$EmacsHome\$DeployFile"
    switch($DeployOp) {
        "symlink" {
            if ((Get-Item $SourcePath) -is [System.IO.DirectoryInfo]) {
                cmd /c mklink /D "$DestPath" "$SourcePath"
            } else {
                cmd /c mklink "$DestPath" "$SourcePath"
            }
            echo "$DeployFile has been symlinked"
        }

        "copy" {
            Write-Warning "The 'copy' operation is not implemented yet. Skipping..."
        }

        default {
            Write-Warning "Unknown operation $operation. Skipping..."
        }
    }
}