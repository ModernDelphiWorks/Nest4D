---
title: Nidus
displayed_sidebar: nidusSidebar
sidebar_position: 0
slug: /nidus/
---

# Nidus

**Nidus** is a modular, highly scalable application and microservice framework for Delphi, inspired by the architectural patterns of [NestJS](https://nestjs.com/).

It provides enterprise-ready backend infrastructure for Object Pascal developers:

| Capability | Unit / Module |
|---|---|
| Dependency Injection | `Nidus.pas`, `Nidus.Inject` |
| Modular organisation | `Nidus.Module.*` |
| Validation Pipes (RTTI) | `Nidus.Pipes.*`, `Nidus.Decorator.*` |
| Security Guards | `TNidus.UseGuard` |
| Object Pooling | `Nidus.ObjectPool`, `Nidus.Pooling.*` |
| Module Caching | `Nidus.Module.Cache` |
| RPC Microservices | `Nidus.Microservices.RPC.*` |
| Message Bus | `Nidus.Message.Bus` |
| Horse HTTP Integration | `Nidus.Driver.Horse`, `Horse.ResponseCache` |

## Compatibility

| Environment | Platforms | DI | RPC |
|---|---|:---:|:---:|
| Delphi XE or later | Win32, Win64, Linux64, macOS, iOS, Android | ✅ | ✅ |

:::note Cross-platform build verified
Win32, Win64 and Linux64 (`dcclinux64`) are build-verified in production (2026-06-20). macOS/iOS/Android follow from the Delphi RTL but are not build-verified yet.
:::

## Quick links

- [Installation](./getting-started/installation)
- [Quickstart](./getting-started/quickstart)
- [API Reference](./reference/api)
- [Decorator Reference](./reference/decorators)
