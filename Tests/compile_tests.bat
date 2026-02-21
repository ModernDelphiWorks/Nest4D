@echo off
echo ========================================
echo    Nest4D - Compilador de Testes
echo ========================================
echo.

REM Define o diretório base do projeto
set PROJECT_ROOT=%~dp0..
set DELPHI_PATH="C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\dcc32.exe"

REM Verifica se o compilador Delphi existe
if not exist %DELPHI_PATH% (
    echo ERRO: Compilador Delphi não encontrado em %DELPHI_PATH%
    echo Por favor, ajuste o caminho do Delphi no script.
    pause
    exit /b 1
)

echo Configurando paths do Nest4D...
echo.

REM Define todos os paths do Nest4D
set NEST4D_PATHS=-I"%PROJECT_ROOT%\Source" ^
-I"%PROJECT_ROOT%\Source\Core" ^
-I"%PROJECT_ROOT%\Source\Horse" ^
-I"%PROJECT_ROOT%\Source\Binds" ^
-I"%PROJECT_ROOT%\Source\Interfaces" ^
-I"%PROJECT_ROOT%\Source\Modules" ^
-I"%PROJECT_ROOT%\Source\Routes" ^
-I"%PROJECT_ROOT%\Source\Pipes\Core" ^
-I"%PROJECT_ROOT%\Source\Pipes\Converts" ^
-I"%PROJECT_ROOT%\Source\Pipes\Decorators" ^
-I"%PROJECT_ROOT%\Source\Pipes\Transforms" ^
-I"%PROJECT_ROOT%\Source\Pipes\Transforms External" ^
-I"%PROJECT_ROOT%\Source\Pipes\Validators" ^
-I"%PROJECT_ROOT%\Source\Addons\MessagesBus" ^
-I"%PROJECT_ROOT%\Source\Microservices\RPC"

REM Define paths de bibliotecas externas (ajuste conforme necessário)
set EXTERNAL_PATHS=-I"C:\Horse\src" ^
-I"C:\DUnitX\src"

REM Compila todos os arquivos .dpr na pasta Tests
echo Compilando testes...
echo.

for %%f in (*.dpr) do (
    echo Compilando: %%f
    %DELPHI_PATH% %%f %NEST4D_PATHS% %EXTERNAL_PATHS% -B -Q
    if errorlevel 1 (
        echo ERRO: Falha ao compilar %%f
        pause
        exit /b 1
    ) else (
        echo OK: %%f compilado com sucesso
    )
    echo.
)

echo ========================================
echo   Todos os testes compilados com sucesso!
echo ========================================
pause