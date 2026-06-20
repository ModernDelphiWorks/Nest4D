# Nidus — DI · RPC microservices · validation pipes · object pooling for Delphi

[![Delphi XE+](https://img.shields.io/badge/Delphi-XE%20or%20superior-blue.svg)]()
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![CRA-ready](https://img.shields.io/badge/CRA--ready-SBOM%20%2B%20Security%20policy-success)](https://www.pubpascal.dev/packages/nidus)

> 🔒 **Supply-chain transparency (CRA-ready):** a machine-readable **SBOM** (CycloneDX) is published on the package portal — [pubpascal.dev/packages/nidus](https://www.pubpascal.dev/packages/nidus) · security disclosure policy in **[SECURITY.md](SECURITY.md)**.

📚 **[Documentation](https://moderndelphiworks.github.io/Nidus/)** · ⬇️ **[Download](../../releases)** · 🐛 **[Issues](../../issues)**

*   [🇬🇧 English](#-english)
*   [🇧🇷 Português](#-português)

---

## 🇬🇧 English

**Nidus** is a state-of-the-art modular, highly scalable application and microservice framework for Delphi, deeply inspired by the modern architectural patterns of **NestJS**. It provides Delphi developers with an enterprise-ready architecture that brings advanced Dependency Injection (DI), modular structural organization, request validation pipes (utilizing class RTTI decorators), security guards, high-concurrency object pooling, caching adapters, and native socket-based RPC microservices. Nidus makes it simple to build modern, testable, and highly maintainable enterprise backend applications in Object Pascal.

### 🚀 Key Features

*   **Modular Organization:** Decouple codebase domains into logical boundaries (Modules, Providers, Services) matching NestJS design paradigms.
*   **Dependency Injection (DI) Engine:** Highly performant, automated binding engine that registers, resolves, and manages class lifecycles transparently.
*   **Validation Pipes (Decorators):** Validate request parameters and object fields out of the box using descriptive RTTI attributes (`[IsEmail]`, `[IsUUID]`, `[IsStrongPassword]`, `[IsNotEmpty]`).
*   **Security Guards:** Implement clean, reusable authorization and access control logic with Guards (`UseGuard`).
*   **Allocation & Component Pooling:** Integrated object and `TComponent` pooling (`UsePools`) to maximize throughput under extreme concurrent workloads.
*   **Module Caching:** Native caching provider support (`UseCache`) with customizable eviction policies to boost API performance.
*   **RPC Microservices:** Publish and invoke backend microservices natively over RPC using Indy or Synapse socket adapters.

### 🏛 Compatibility Matrix

| Environment / IDE | Platform / Compiler | Dependency Injection | RPC Microservices |
| :--- | :--- | :---: | :---: |
| **Delphi XE or superior** | VCL, FMX, Console (Win/Linux/macOS/iOS/Android) | ✅ Yes | ✅ Yes |

### 🐧 Cross-Platform Build — Win32 / Win64 / Linux64 (verified)

> **✅ Verified 2026-06-20** in a real production backend: Nidus compiles as a dependency on **Win32, Win64 and Linux64** (`dcclinux64`), and the Linux server boots and registers routes. macOS/iOS/Android follow from the Delphi RTL but are **not build-verified** here yet.

The only Windows-only touch was the `DEBUG` trace in `Nidus.Exception` (`OutputDebugString`), now `{$IFDEF MSWINDOWS}`-guarded with a `stderr` fallback on POSIX; the rest of the framework is already platform-neutral. Windows behaviour is unchanged.

**Building a consumer app for Linux64:** install the Linux 64-bit platform (RAD Studio GetIt / `GetItCmd -if=delphi_linux -ae`), provide a Linux SDK (RAD Studio SDK Manager + PAServer, **or** a sysroot assembled from a WSL/Linux toolchain passed to `dcclinux64` via `--syslibroot` / `--libpath`), then compile with `dcclinux64`. Note: a FireDAC-backed server also needs the driver registered for console/Linux (e.g. `FireDAC.Stan.Def` + `FireDAC.Phys.FB` + the headless `FireDAC.ConsoleUI.Wait`).

### ⚙️ Installation

To install using the package manager [**Boss**](https://github.com/HashLoad/boss):

```sh
boss install "https://github.com/ModernDelphiWorks/Nidus"
```

> [!NOTE]
> Since this package does not have a static registry on Boss, it must be installed using its direct Git repository HTTPS URL.

Alternatively, install via [**pubpascal**](https://www.pubpascal.dev/packages/nidus):

```sh
boss install Nidus
```

---

### ⚡️ Quick Start

#### 1. Define a Modular Architecture (NestJS Style)

```delphi
type
  [Module]
  TUserModule = class(TModule)
  public
    procedure Configure(const ABuilder: IModuleBuilder); override;
  end;
```

#### 2. Declare Validation Decorators (Pipes)

```delphi
type
  TCreateUserDto = class
  private
    FName: string;
    FEmail: string;
    FPassword: string;
  public
    [IsNotEmpty('Name is required!')]
    property Name: string read FName write FName;

    [IsEmail('Invalid email address!')]
    property Email: string read FEmail write FEmail;

    [IsStrongPassword('Password is too weak!')]
    property Password: string read FPassword write FPassword;
  end;
```

#### 3. Initialize and Bootstrap Nidus

```delphi
var
  LApp: TNidus;
begin
  LApp := GetNidus;
  try
    LApp
      .UseListener(TMyLogger.Log)
      .UsePipes(TValidationPipe.Create)  // Global RTTI validation
      .UseCache(TMemoryCache.Create)      // Enable memory caching
      .UsePools<TMyHeavyResource>(128)   // Pool heavy resources
      .Start(TAppModule.Create);         // Bootstrap main application module

    // Resolve services dynamically
    var LUserService := LApp.Get<TUserService>;
    LUserService.Register('Isaque Pinheiro', 'isaquesp@gmail.com');
  finally
    LApp.Finalize;
  end;
end;
```

---

## 🇧🇷 Português

**Nidus** é um framework moderno de arquitetura modular, altamente escalável e focado em microsserviços para Delphi, profundamente inspirado nos padrões consagrados do **NestJS**. Ele fornece aos desenvolvedores Delphi uma arquitetura robusta e corporativa que integra Injeção de Dependências (DI), organização estrutural modular, pipes de validação de requisição (baseados em decorators RTTI), guards de segurança, pool de alocação de objetos/componentes, adaptadores de cache de dados e microsserviços RPC nativos baseados em sockets. O Nidus foi feito sob medida para criar backends testáveis, performáticos e extremamente legíveis em Object Pascal.

### 🚀 Recursos Principais

*   **Organização Modular:** Divida os domínios do seu código em fronteiras lógicas desacopladas e organizadas (Módulos, Provedores, Serviços) herdadas do NestJS.
*   **Motor de Injeção de Dependências (DI):** Registro automático e resolução ágil de dependências que gerencia de forma transparente o ciclo de vida de classes e instâncias.
*   **Pipes de Validação (Decorators):** Validação rica de dados de entrada usando atributos RTTI intuitivos diretamente nas propriedades (`[IsEmail]`, `[IsUUID]`, `[IsStrongPassword]`, `[IsNotEmpty]`).
*   **Guards de Segurança:** Implemente controles de acesso, políticas e regras de autorização centralizadas e reutilizáveis (`UseGuard`).
*   **Pool de Alocação de Objetos:** Pools nativos de alocação de objetos e componentes (`UsePools`) para maximizar a vazão do servidor sob cenários de concorrência extrema.
*   **Cache de Módulos:** Suporte a cache integrado (`UseCache`) com políticas configuráveis de descarte para otimizar endpoints e chamadas pesadas.
*   **Microsserviços RPC:** Publique, disponibilize e consuma microsserviços distribuídos via RPC usando adaptadores de socket Indy ou Synapse.

### 🏛 Matriz de Compatibilidade

| Ambiente / IDE | Plataforma / Compilador | Injeção de Dependências | Microsserviços RPC |
| :--- | :--- | :---: | :---: |
| **Delphi XE ou superior** | VCL, FMX, Console (Win/Linux/macOS/iOS/Android) | ✅ Sim | ✅ Sim |

### 🐧 Build Multiplataforma — Win32 / Win64 / Linux64 (verificado)

> **✅ Verificado em 2026-06-20** num backend real em produção: o Nidus compila como dependência em **Win32, Win64 e Linux64** (`dcclinux64`), e o servidor Linux sobe e registra as rotas. macOS/iOS/Android seguem da RTL Delphi, mas **ainda não foram verificados** em build aqui.

O único ponto Windows-only era o trace de `DEBUG` em `Nidus.Exception` (`OutputDebugString`), agora sob `{$IFDEF MSWINDOWS}` com fallback para `stderr` no POSIX; o resto do framework já é neutro de plataforma. O comportamento no Windows não muda.

**Para buildar um app consumidor no Linux64:** instale a plataforma Linux 64-bit (RAD Studio GetIt / `GetItCmd -if=delphi_linux -ae`), forneça um SDK Linux (SDK Manager do RAD Studio + PAServer, **ou** um sysroot montado de um toolchain WSL/Linux passado ao `dcclinux64` via `--syslibroot` / `--libpath`), e compile com `dcclinux64`. Obs.: um servidor com FireDAC também precisa do driver registrado para console/Linux (ex.: `FireDAC.Stan.Def` + `FireDAC.Phys.FB` + o `FireDAC.ConsoleUI.Wait` headless).

### ⚙️ Instalação

Para instalar usando o gerenciador de pacotes [**Boss**](https://github.com/HashLoad/boss):

```sh
boss install "https://github.com/ModernDelphiWorks/Nidus"
```

> [!NOTE]
> Como esta biblioteca não está pré-registrada com apelido no indexador global do Boss, ela é instalada informando-se o link HTTPS direto de seu repositório Git.

Alternativamente, instale via [**pubpascal**](https://www.pubpascal.dev/packages/nidus):

```sh
boss install Nidus
```

---

### ⚡️ Início Rápido

#### 1. Defina um Módulo (Estilo NestJS)

```delphi
type
  [Module]
  TUserModule = class(TModule)
  public
    procedure Configure(const ABuilder: IModuleBuilder); override;
  end;
```

#### 2. Declare Atributos de Validação (Decorators/Pipes)

```delphi
type
  TCreateUserDto = class
  private
    FNome: string;
    FEmail: string;
    FSenha: string;
  public
    [IsNotEmpty('O nome é obrigatório!')]
    property Nome: string read FNome write FNome;

    [IsEmail('Endereço de e-mail inválido!')]
    property Email: string read FEmail write FEmail;

    [IsStrongPassword('A senha informada é muito fraca!')]
    property Senha: string read FSenha write FSenha;
  end;
```

#### 3. Inicialização e Bootstrap do Nidus

```delphi
var
  LApp: TNidus;
begin
  LApp := GetNidus;
  try
    LApp
      .UseListener(TMyLogger.Log)
      .UsePipes(TValidationPipe.Create)  // Validação RTTI global
      .UseCache(TMemoryCache.Create)      // Ativa cache em memória
      .UsePools<TMyHeavyResource>(128)   // Pool de recursos pesados
      .Start(TAppModule.Create);         // Inicializa o módulo principal

    // Resolve serviços dinamicamente
    var LUserService := LApp.Get<TUserService>;
    LUserService.Register('Isaque Pinheiro', 'isaquesp@gmail.com');
  finally
    LApp.Finalize;
  end;
end;
```

---

## ⛏️ Contributing / Contribuição

Contributions are welcome — bug reports, feature requests, and pull requests all help Nidus grow.
Contribuições são bem-vindas — relatórios de bugs, sugestões de features e pull requests ajudam o Nidus a crescer.

[![Issues](https://img.shields.io/badge/Issues-channel-orange)](../../issues)

**Steps / Passos:**

1. Fork the repository / Faça um fork do repositório.
2. Create a feature branch / Crie uma branch de feature: `git checkout -b feat/my-feature`.
3. Commit your changes / Faça commits das suas alterações: `git commit -m "feat: describe change"`.
4. Push to your fork / Envie para o seu fork: `git push origin feat/my-feature`.
5. Open a Pull Request targeting `main` / Abra um Pull Request apontando para `main`.

---

## 📬 Contact / Contato

[![Email](https://img.shields.io/badge/Email-isaquesp%40gmail.com-D14836?logo=gmail&logoColor=white)](mailto:isaquesp@gmail.com)

---

## 💲 Donation / Doação

If Nidus saves you time, consider supporting its development.
Se o Nidus economiza seu tempo, considere apoiar o seu desenvolvimento.

[![Doação](https://img.shields.io/badge/PagSeguro-contribua-green)](https://pag.ae/bglQrWD)

---

## 📄 License / Licença

Distributed under the **MIT License**. See [LICENSE](LICENSE) for full text.
Distribuído sob a **Licença MIT**. Consulte [LICENSE](LICENSE) para o texto completo.

*Copyright © 2025-2026 Isaque Pinheiro.*
