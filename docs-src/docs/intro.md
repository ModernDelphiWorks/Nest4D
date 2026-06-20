---
displayed_sidebar: docsSidebar
title: Documentation portal
slug: /
sidebar_position: 0
---

Welcome to the **Nidus** technical documentation portal. Content is derived from source code, tests, and pipeline artifacts.

## Projects

<div className="row">
  <div className="col col--6 margin-bottom--lg">
    <div className="card">
      <div className="card__header">
        <h3>Nidus</h3>
      </div>
      <div className="card__body">
        <p>Modular, scalable application and microservice framework for Delphi, inspired by NestJS. Provides Dependency Injection, validation pipes (RTTI decorators), security guards, object pooling, module caching, and socket-based RPC microservices. Verified on Win32, Win64 and Linux64.</p>
      </div>
      <div className="card__footer">
        <a className="button button--primary" href="./nidus/">Open documentation →</a>
      </div>
    </div>
  </div>
</div>

## Documented release

This portal tracks the **main** branch of [ModernDelphiWorks/Nidus](https://github.com/ModernDelphiWorks/Nidus).

- Cross-platform build verified: **Win32, Win64, Linux64** (2026-06-20).
- `{$IFDEF MSWINDOWS}` guard added to `Nidus.Exception` for `OutputDebugString`; POSIX falls back to `stderr`.
- Supply-chain: SBOM (CycloneDX) published at [pubpascal.dev/packages/nidus](https://www.pubpascal.dev/packages/nidus).
