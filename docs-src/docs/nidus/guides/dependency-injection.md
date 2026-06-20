---
title: Dependency Injection
displayed_sidebar: nidusSidebar
sidebar_position: 2
---

# Dependency Injection

Nidus includes a purpose-built DI engine (`TCoreInject`, `TNidusInject`) that manages the entire application object graph.

## How it works

When the `Nidus.Inject` unit is first loaded (e.g. via its `initialization` section in the `.dpr`), it creates a global pointer `GNidusInject: PNidusInject` and bootstraps `TCoreInject`. `TCoreInject` registers all framework internals:

| Internal | Registration |
|---|---|
| `TTracker`, `TRouteManager` | `SingletonLazy` |
| `TModernObject` | `Singleton` |
| `TBindService`, `TRouteService`, `TModuleService` | `Factory` |
| `TBindProvider`, `TModuleProvider`, `TRouteProvider`, `TRouteParse` | `Factory` |
| `TNidus` | `SingletonLazy` + wired via `IncludeModuleService` / `IncludeBindService` / `IncludeRouteParser` |

## Registering your own providers

Use `TModule.Configure` to declare providers for a module scope:

```delphi
procedure TOrderModule.Configure(const ABuilder: IModuleBuilder);
begin
  ABuilder
    // Singleton: one TOrderService instance for the app lifetime
    .AddProvider(TBind<TOrderService>.Singleton)
    // Factory: new TOrderValidator on each injection
    .AddProvider(TBind<TOrderValidator>.Factory)
    // Interface-backed singleton
    .AddProvider(TBind<TMemoryOrderRepo>.SingletonInterface<IOrderRepository>)
    .Export(TOrderService);
end;
```

## Resolving dependencies

```delphi
// Resolve by class
var LOrderSvc := GetNidus.Get<TOrderService>;

// Resolve by interface
var IRepo := GetNidus.GetInterface<IOrderRepository>;

// Resolve with a tag (when multiple bindings exist for the same type)
var LSvc := GetNidus.Get<TOrderService>('v2');
```

Source: `TNidus.Get<T>` and `TNidus.GetInterface<I>` delegate to `TBindService.GetBind<T>` / `TBindService.GetBindInterface<I>`, returning a `TResultPair<T, Exception>`. Errors are re-raised automatically.

## Lifecycle management

Object lifetimes are controlled by the bind strategy:

- **Singleton** — created during `Start`, destroyed during `Finalize`.
- **SingletonLazy** — created on first resolution, destroyed during `Finalize`.
- **Factory** — created on every `Get<T>` call; caller owns the instance.

## Finalization

```delphi
GetNidus.Finalize;
```

`Finalize` frees `FAppModule` and calls `GNidusInject^.ExtractInject<TNidusInject>(C_NIDUS)` to clean up the internal container. The `finalization` section of `Nidus.Inject` also calls `Finalize` automatically when the unit is unloaded, so double-finalization is safe.
