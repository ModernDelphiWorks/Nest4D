---
title: Introduction
displayed_sidebar: nidusSidebar
sidebar_position: 1
---

# Introduction

## What is Nidus?

Nidus is a state-of-the-art modular framework for building enterprise backend applications and microservices in Delphi (Object Pascal). Its design is deeply inspired by [NestJS](https://nestjs.com/), bringing the same separation-of-concerns philosophy and structural conventions to the Delphi ecosystem.

## Design philosophy

### Modules as first-class citizens

Every feature lives inside a `TModule` subclass. Modules declare their own providers (services), import other modules, and export providers to consumers. This mirrors the NestJS module pattern and keeps large codebases maintainable.

### Inversion of control via Dependency Injection

Nidus includes a built-in DI engine (`TCoreInject`) that automatically resolves constructor dependencies, manages object lifecycles (Singleton, SingletonLazy, Factory), and wires the application graph at startup.

### Declarative validation via RTTI decorators

Request DTOs are annotated with RTTI attributes (`[IsEmail]`, `[IsNotEmpty]`, `[IsStrongPassword]`, …). The global `TValidationPipe` inspects incoming requests and rejects invalid payloads before any service code runs.

### Guards for access control

`UseGuard` accepts a callback that returns `Boolean`. If it returns `False`, Nidus raises `EUnauthorizedException` (HTTP 401) before the route handler is invoked.

### Addons: pooling, caching, RPC, message bus

Beyond the HTTP request pipeline, Nidus provides first-class addons for high-throughput scenarios:

- **Object pooling** — reuse expensive objects or `TComponent` instances across requests.
- **Module caching** — keep module instances alive across requests to avoid repeated construction.
- **RPC microservices** — publish named handlers over a TCP socket (Indy or Synapse adapters).
- **Message bus** — in-process pub/sub with wildcard topic matching.

## Architecture overview

```
Application bootstrap (GetNidus / TNidus)
  │
  ├── UseListener   → TAppListener  (structured log)
  ├── UsePipes      → IValidationPipe (global RTTI validation)
  ├── UseGuard      → TGuardCallback  (authorization)
  ├── UseCache      → IModuleCache    (module-level caching)
  ├── UsePools      → IPoolRegistry   (object / component pools)
  ├── UseRPC        → IRPCProviderServer (TCP microservice server)
  │
  └── Start(TAppModule)
        │
        └── TModule tree (imports / providers / services)
              │
              └── Route handlers → LoadRouteModule(path)
```

## Key source units

| Unit | Role |
|---|---|
| `Nidus.pas` | `TNidus` facade — the single entry point for application configuration |
| `Nidus.Inject` | `TCoreInject` — wires the internal DI container at startup |
| `Nidus.Module.Provider` | Walks the module tree, registers routes, manages lifetimes |
| `Nidus.Driver.Horse` | Horse middleware adapter (`Nidus_Horse`) |
| `Nidus.Module.Cache` | `TModuleCacheManager` — thread-safe per-module cache |
| `Nidus.ObjectPool` | Generic `TObjectPool<T>` with acquire/release semantics |
| `Nidus.RPC.Publish` | `TRPCPublish` — RPC name→class registry |
| `Nidus.Message.Bus` | `TMessageBus` — in-process event/pub-sub |
