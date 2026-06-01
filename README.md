# Nidus Application Framework for Delphi

[![Delphi Supported Versions](https://img.shields.io/badge/Delphi%20Supported%20Versions-XE%2B-blue.svg)]()
[![License](https://img.shields.io/badge/License-Apache--2.0-blue.svg)](LICENSE)

*   [🇬🇧 English](#-english)
*   [🇧🇷 Português](#-português)

---

## 🇬🇧 English

**Nidus** is a state-of-the-art modular, highly scalable application and microservice framework for Delphi, deeply inspired by the modern architectural patterns of **NestJS**. 

It provides an enterprise-ready architecture that brings Dependency Injection, modular structural organization, request validation pipes, security guards, object pooling, caching, and cross-platform RPC microservices to Delphi developers.

<p align="center">
  <img src="https://img.shields.io/badge/Nidus-NestJS%20for%20Delphi-red.svg" alt="Nidus Badges">
</p>

### 🏛 Supported Platforms
*   **Delphi XE or superior** (VCL, FMX, Console, Multi-Threaded, Microservices)
*   **Lazarus / FreePascal** (Core Architecture)

### ⚙️ Installation
To install using [`boss`]:
```sh
boss install github.com/ModernDelphiWorks/Nidus
```

---

### 🚀 Key Features

*   **Modular Architecture:** Organize your application code into decoupled, maintainable logical boundaries (Modules, Providers, Services), matching NestJS paradigms.
*   **Dependency Injection (DI) & Binding:** Native, fast dependency binding engine that loads, resolves, and manages class lifecycles automatically.
*   **Validation Pipes (Decorators):** Powerful request parameter and body validation using RTTI attributes. Validate fields out-of-the-box (e.g., `[IsEmail]`, `[IsUUID]`, `[IsStrongPassword]`, `[IsIP]`, `[IsSemver]`).
*   **Security Guards:** Implement uniform authorization and access control logic with reusable Guards (e.g. `UseGuard`).
*   **Object & Component Pooling:** Integrated allocation pools (`UsePools`) for objects and Delphi `TComponent` instances to maximize high-load throughput.
*   **Intelligent Caching:** Dynamic caching support for modules (`UseCache`) with customizable eviction policies.
*   **RPC Microservices:** Publish and invoke microservices over RPC using Indy or Synapse socket providers natively.
*   **Routing & Integration:** Built-in adapter for HTTP drivers (such as the Horse micro-framework).

---

### ⚡️ Quick Start

#### 1. Define a Modular Architecture (NestJS-style)
```delphi
type
  [Module]
  TUserModule = class(TModule)
  public
    // Declare imported modules, exported providers, and services
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
      .UsePipes(TValidationPipe.Create) // Enable global RTTI validators
      .UseCache(TMemoryCache.Create)     // Enable memory caching
      .UsePools<TMyHeavyResource>(128)  // Pool up to 128 instances
      .Start(TAppModule.Create);        // Bootstrap main module
      
    // Resolve services dynamically
    var LUserService := LApp.Get<TUserService>;
    LUserService.Register('Isaque Pinheiro', 'isaquesp@gmail.com');
  finally
    LApp.Finalize;
  end;
end;
```

---

### ⛏️ Contributing
We love contributions! Feel free to open issues or submit pull requests.

### 📬 Contact & Support
*   **Telegram**: [HashLoad Channel](https://t.me/hashload)
*   **Website**: [isaquepinheiro.com.br](https://www.isaquepinheiro.com.br)

---

## 🇧🇷 Português

**Nidus** é um framework moderno de arquitetura modular, altamente escalável e focado em microsserviços para Delphi, profundamente inspirado nos padrões arquiteturais do **NestJS**.

Ele fornece uma arquitetura robusta e corporativa que traz Injeção de Dependência, organização estrutural modular, pipes de validação de requisição (decorators), guards de segurança, pool de objetos/componentes, caching de módulos e microsserviços RPC multiplataforma nativos para desenvolvedores Delphi.

---

### 🚀 Recursos Principais

*   **Arquitetura Modular:** Organize o código da sua aplicação em fronteiras lógicas desacopladas e fáceis de manter (Módulos, Provedores, Serviços), seguindo os padrões consagrados do NestJS.
*   **Injeção de Dependências (DI):** Motor nativo e ultra-rápido de vinculação de dependências que carrega, resolve e gerencia o ciclo de vida de classes automaticamente.
*   **Validation Pipes (Decorators):** Validação poderosa de parâmetros e corpos de requisição baseada em atributos RTTI. Valide campos imediatamente (ex: `[IsEmail]`, `[IsUUID]`, `[IsStrongPassword]`, `[IsIP]`, `[IsSemver]`).
*   **Guards de Segurança:** Implemente lógicas uniformes de autorização e controle de acesso com Guards reutilizáveis (ex: `UseGuard`).
*   **Pool de Objetos & Componentes:** Pools integrados de alocação de alta performance (`UsePools`) para objetos e instâncias de `TComponent` do Delphi para maximizar a vazão sob alta carga.
*   **Cache Inteligente:** Suporte de cache dinâmico para módulos (`UseCache`) com políticas configuráveis de descarte.
*   **Microsserviços RPC:** Publique e consuma microsserviços via RPC de forma nativa usando provedores de socket Indy ou Synapse.
*   **Roteamento & Integração:** Adaptador integrado para drivers HTTP (como o micro-framework Horse).

---

### ⚡️ Início Rápido

#### 1. Defina um Módulo (Estilo NestJS)
```delphi
type
  [Module]
  TUserModule = class(TModule)
  public
    // Declara módulos importados, provedores exportados e serviços
    procedure Configure(const ABuilder: IModuleBuilder); override;
  end;
```

#### 2. Declare Atributos de Validação (Pipes)
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
      .UsePipes(TValidationPipe.Create) // Ativa validadores RTTI globais
      .UseCache(TMemoryCache.Create)     // Ativa cache em memória
      .UsePools<TMyHeavyResource>(128)  // Pool de até 128 instâncias
      .Start(TAppModule.Create);        // Inicializa o módulo principal
      
    // Resolve serviços dinamicamente
    var LUserService := LApp.Get<TUserService>;
    LUserService.Register('Isaque Pinheiro', 'isaquesp@gmail.com');
  finally
    LApp.Finalize;
  end;
end;
```

---

### ⛏️ Contribuição
Adoramos contribuições! Sinta-se à vontade para abrir issues ou enviar pull requests.

### 📬 Contato & Suporte
*   **Telegram**: [Canal HashLoad](https://t.me/hashload)
*   **Website**: [isaquepinheiro.com.br](https://www.isaquepinheiro.com.br)

---
*Copyright © 2025-2026 Isaque Pinheiro. Licensed under Apache-2.0.*
