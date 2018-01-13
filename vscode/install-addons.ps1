Write-Host "Installing addons for Vs\S Code"
Write-Host ""
foreach($line in Get-Content .\addons.list) {
    if($line -match $regex){
        code-insiders --install-extension $line
    }
}

Write-Host ""
Write-Host "Done"
