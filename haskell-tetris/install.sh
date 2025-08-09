#!/bin/bash

# Tetris Haskell - Script de Instalación y Ejecución

set -e  # Exit on any error

echo "=== Tetris Haskell - Instalación ==="
echo ""

# Check if cabal is installed
if ! command -v cabal &> /dev/null; then
    echo "❌ Error: Cabal no está instalado."
    echo "Por favor instala Haskell y Cabal desde: https://www.haskell.org/get-started/"
    exit 1
fi

# Check if ghc is installed
if ! command -v ghc &> /dev/null; then
    echo "❌ Error: GHC no está instalado."
    echo "Por favor instala Haskell y GHC desde: https://www.haskell.org/get-started/"
    exit 1
fi

echo "✅ Cabal encontrado: $(cabal --version | head -n1)"
echo "✅ GHC encontrado: $(ghc --version)"
echo ""

# Update package index
echo "📦 Actualizando índice de paquetes..."
cabal update

# Build the project
echo "🔨 Compilando proyecto..."
cabal build

# Run tests
echo "🧪 Ejecutando tests..."
cabal test

echo ""
echo "✅ ¡Instalación completada con éxito!"
echo ""
echo "Para jugar, ejecuta:"
echo "  cabal run tetris"
echo ""
echo "Para ejecutar tests:"
echo "  cabal test"
echo ""
echo "Controles del juego:"
echo "  4 = Izquierda"
echo "  6 = Derecha"
echo "  7 = Rotar ↺"
echo "  9 = Rotar ↻"
echo "  q = Salir"