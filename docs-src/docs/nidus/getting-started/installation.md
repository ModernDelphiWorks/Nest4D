---
title: Installation
displayed_sidebar: nidusSidebar
sidebar_position: 1
---

# Installation

## Requirements

- Delphi XE or later (Win32, Win64, Linux64, macOS, iOS, Android)
- [Boss](https://github.com/HashLoad/boss) package manager **or** [PubPascal](https://www.pubpascal.dev)

## Via Boss (Git URL)

Nidus is not registered in the Boss global index, so install it using its direct HTTPS URL:

```sh
boss install "https://github.com/ModernDelphiWorks/Nidus"
```

:::note
The direct URL form (`boss install "<url>"`) bypasses the registry lookup and clones the repository directly into the `modules/` folder of your project.
:::

## Via PubPascal

```sh
pubpascal install nidus
```

Package page: [pubpascal.dev/packages/nidus](https://www.pubpascal.dev/packages/nidus)

## Manual (search path)

1. Clone or download the repository.
2. Add the `Source` sub-tree to your project's search path in the Delphi IDE (Project Options → Delphi Compiler → Search path):

```
<nidus-root>\Source
<nidus-root>\Source\Core
<nidus-root>\Source\Modules
<nidus-root>\Source\Binds
<nidus-root>\Source\Pipes\Core
<nidus-root>\Source\Pipes\Decorators
<nidus-root>\Source\Addons\Caching
<nidus-root>\Source\Addons\Pooling
<nidus-root>\Source\Addons\MessagesBus
<nidus-root>\Source\Horse
<nidus-root>\Source\Interfaces
<nidus-root>\Source\Microservices\RPC
<nidus-root>\Source\Microservices\RPC\Providers
<nidus-root>\Source\Microservices\RPC\Providers\Indy
<nidus-root>\Source\Microservices\RPC\Providers\Synapse
```

## Linux64 builds

Install the Linux 64-bit platform (RAD Studio GetIt / `GetItCmd -if=delphi_linux -ae`), provide a Linux SDK via RAD Studio SDK Manager + PAServer **or** a sysroot assembled from a WSL/Linux toolchain (`--syslibroot` / `--libpath`), then compile with `dcclinux64`.

If the consuming app uses FireDAC, also register the headless driver units:

```delphi
uses
  FireDAC.Stan.Def,
  FireDAC.Phys.FB,
  FireDAC.ConsoleUI.Wait;
```

## Supply-chain / SBOM

A machine-readable CycloneDX SBOM is published at [pubpascal.dev/packages/nidus](https://www.pubpascal.dev/packages/nidus). Security disclosure policy: [SECURITY.md](https://github.com/ModernDelphiWorks/Nidus/blob/main/SECURITY.md).
