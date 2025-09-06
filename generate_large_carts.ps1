# generate_large_carts.ps1
param(
    [int]$Size = 100,
    [string]$OutputFile = "cart_large.json"
)

# Productos disponibles (ajusta según tu products.json)
$products = @("prod1", "prod2", "prod3", "prod4", "prod5", "prod6", "prod7", "prod8")

$cart = @()
for ($i = 1; $i -le $Size; $i++) {
    $randomProduct = $products | Get-Random
    $randomQty = Get-Random -Minimum 1 -Maximum 10
    
    $cart += @{
        "product_id" = $randomProduct
        "qty" = $randomQty
    }
}

$cart | ConvertTo-Json | Out-File -FilePath $OutputFile -Encoding UTF8
Write-Host "Generado carrito con $Size items en $OutputFile"