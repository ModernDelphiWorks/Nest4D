---
title: Modular Architecture
displayed_sidebar: nidusSidebar
sidebar_position: 1
---

# Modular Architecture

Nidus structures applications as a tree of **modules** (`TModule`), each encapsulating a cohesive slice of functionality. This mirrors the NestJS module pattern.

## The `[Module]` attribute

Decorate every module class with `[Module]` so that Nidus can inspect the type via RTTI at startup:

```delphi
type
  [Module]
  TUserModule = class(TModule)
  public
    procedure Configure(const ABuilder: IModuleBuilder); override;
  end;
```

## Declaring providers

Inside `Configure`, use `IModuleBuilder` to register providers and exports:

```delphi
procedure TUserModule.Configure(const ABuilder: IModuleBuilder);
begin
  ABuilder
    .AddProvider(TBind<TUserService>.Singleton)
    .AddProvider(TBind<TUserRepository>.Factory)
    .Export(TUserService);   // expose TUserService to importing modules
end;
```

### Bind strategies

| Method | Lifecycle | Description |
|---|---|---|
| `TBind<T>.Singleton` | One instance per app | Created once, reused forever |
| `TBind<T>.SingletonLazy` | Lazy singleton | Created on first `Get<T>` call |
| `TBind<T>.Factory` | Per-resolution | New instance on every `Get<T>` |
| `TBind<T>.SingletonInterface<I>` | Singleton via interface | Registered under interface `I` |
| `TBind<T>.AddInstance` | Manual | Registers a pre-built instance |

Source: `Nidus.Bind` (`TBind<T>`, `TSingletonBind<T>`, `TFactoryBind<T>`, …)

## Importing modules

A module can import another module to consume its exported providers:

```delphi
type
  [Module]
  TAppModule = class(TModule)
  public
    procedure Configure(const ABuilder: IModuleBuilder); override;
  end;

procedure TAppModule.Configure(const ABuilder: IModuleBuilder);
begin
  ABuilder
    .Import(TUserModule)
    .Import(TOrderModule);
end;
```

## Resolving services

After `TNidus.Start`, resolve any exported service by type:

```delphi
var LSvc := GetNidus.Get<TUserService>;
```

Or by interface:

```delphi
var IRepo := GetNidus.GetInterface<IUserRepository>;
```

## Route modules (lazy loading)

Modules can be loaded and disposed per HTTP request via the route mechanism:

```delphi
// inside a Horse middleware or route handler:
GetNidus.LoadRouteModule('/users/create', LRequest);

// after the request, dispose the module tree for that path:
GetNidus.DisposeRouteModule('/users/create');
```

`TModuleProvider.DisposeModule` walks the path segments and frees each module instance, unless the module is pinned by the cache policy.
