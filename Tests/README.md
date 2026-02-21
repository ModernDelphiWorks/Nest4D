# Nest4D - Testes Unitários

Este diretório contém os testes unitários para o framework Nest4D.

## 📋 Estrutura dos Testes

### 🧪 **Projetos de Teste**
- `nest4d_core_tests.dpr` - Testes principais dos componentes Core

### 📦 **Módulos de Teste**
- `Test.Nest4D.Logging.pas` - Testes do sistema de logging estruturado
- `Test.Nest4D.Metrics.pas` - Testes do sistema de métricas
- `Test.Nest4D.Health.pas` - Testes dos health checks
- `Test.Nest4D.Cache.pas` - Testes do sistema de cache
- `Test.Nest4D.Pool.pas` - Testes do pool de handlers

## 🚀 Como Executar os Testes

### ⚡ **Compilação Automática**

1. **Execute o script de compilação:**
   ```batch
   compile_tests.bat
   ```

2. **O script irá:**
   - Configurar todos os paths do Nest4D automaticamente
   - Compilar todos os projetos `.dpr` na pasta Tests
   - Exibir o status de cada compilação

### 🔧 **Configuração Manual**

Se preferir compilar manualmente, certifique-se de incluir os seguintes paths:

```
// Paths principais do Nest4D
..\Source
..\Source\Core
..\Source\Horse
..\Source\Binds
..\Source\Interfaces
..\Source\Modules
..\Source\Routes

// Paths dos Pipes
..\Source\Pipes\Core
..\Source\Pipes\Converts
..\Source\Pipes\Decorators
..\Source\Pipes\Transforms
..\Source\Pipes\Transforms External
..\Source\Pipes\Validators

// Paths dos Addons
..\Source\Addons\MessagesBus
..\Source\Microservices\RPC
```

### 📊 **Executando os Testes**

1. **Compile o projeto:**
   ```batch
   compile_tests.bat
   ```

2. **Execute o arquivo gerado:**
   ```batch
   nest4d_core_tests.exe
   ```

3. **Visualize os resultados:**
   - Console: Resultados em tempo real
   - XML: Arquivo `dunitx-results.xml` (compatível com CI/CD)

## 🧪 Cobertura dos Testes

### ✅ **Componentes Testados**

| Componente | Arquivo de Teste | Status |
|------------|------------------|--------|
| 📝 Logging | `Test.Nest4D.Logging.pas` | ✅ Implementado |
| 📊 Métricas | `Test.Nest4D.Metrics.pas` | ✅ Implementado |
| 🏥 Health Checks | `Test.Nest4D.Health.pas` | ✅ Implementado |
| 💾 Cache | `Test.Nest4D.Cache.pas` | ✅ Implementado |
| 🏊 Pool de Handlers | `Test.Nest4D.Pool.pas` | ✅ Implementado |

### 🔄 **Testes Planejados**

- [ ] Interceptadores (`nest4d.interceptor.pas`)
- [ ] Injeção de Dependência (`nest4d.injector.pas`)
- [ ] Sistema de Exceções (`nest4d.exception.pas`)
- [ ] Rastreamento (`nest4d.tracker.pas`)
- [ ] Integração Horse (`nest4d.horse.pas`)

## 📋 Requisitos

### 🛠️ **Dependências**

1. **Delphi/RAD Studio** (versão 10.3 ou superior)
2. **DUnitX** - Framework de testes unitários
3. **Horse** - Framework web (para testes de integração)

### 📁 **Estrutura de Pastas**

```
Nest4D/
├── Source/           # Código fonte do framework
│   ├── Core/         # Componentes principais
│   ├── Horse/        # Integração com Horse
│   └── ...
├── Tests/            # Esta pasta
│   ├── compile_tests.bat
│   ├── nest4d_core_tests.dpr
│   └── Test.*.pas
└── Examples/         # Exemplos de uso
```

## 🔧 Configuração do Script

### ⚙️ **Personalizando o compile_tests.bat**

Se necessário, ajuste as seguintes variáveis no script:

```batch
# Caminho do compilador Delphi
set DELPHI_PATH="C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\dcc32.exe"

# Paths de bibliotecas externas
set EXTERNAL_PATHS=-I"C:\Horse\src" ^
-I"C:\DUnitX\src"
```

### 🎯 **Adicionando Novos Testes**

1. **Crie um novo arquivo de teste:**
   ```pascal
   unit Test.Nest4D.NovoComponente;
   
   interface
   uses DUnitX.TestFramework;
   
   [TestFixture]
   TTestNovoComponente = class
     [Test]
     procedure TestAlgumaFuncionalidade;
   end;
   ```

2. **Adicione ao projeto principal:**
   ```pascal
   // Em nest4d_core_tests.dpr
   uses
     Test.Nest4D.NovoComponente in 'Test.Nest4D.NovoComponente.pas';
   ```

3. **Execute o script de compilação:**
   ```batch
   compile_tests.bat
   ```

## 📈 Integração Contínua

Os testes geram saída compatível com sistemas de CI/CD:

- **Console Output**: Para visualização em tempo real
- **XML Output**: Arquivo `dunitx-results.xml` no formato NUnit

### 🔄 **Exemplo para GitHub Actions**

```yaml
- name: Run Tests
  run: |
    cd Tests
    compile_tests.bat
    nest4d_core_tests.exe --xml
```

## 🐛 Troubleshooting

### ❌ **Problemas Comuns**

1. **"Compilador não encontrado"**
   - Ajuste o `DELPHI_PATH` no script
   - Verifique se o Delphi está instalado

2. **"Unit não encontrada"**
   - Verifique os paths no script
   - Certifique-se que todas as dependências estão instaladas

3. **"Falha na compilação"**
   - Execute o script em modo verbose
   - Verifique os logs de erro

### 📞 **Suporte**

Para problemas ou sugestões:
- 📧 Email: isaquesp@gmail.com
- 🌐 Site: https://www.isaquepinheiro.com.br
- 📚 Documentação: https://nest4d-en.docs-br.com

---

**Nest4D Framework** - Desenvolvendo o futuro em Delphi! 🚀