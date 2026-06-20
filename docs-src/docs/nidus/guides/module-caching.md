---
title: Module Caching
displayed_sidebar: nidusSidebar
sidebar_position: 6
---

# Module Caching

By default, Nidus creates and destroys module instances on every request (`DisposeRouteModule`). Module caching pins selected modules so their instances survive across requests, avoiding repeated construction of expensive services.

## Enabling caching

### Memory cache for specific modules

```delphi
GetNidus
  .UseCache(
    TModuleCacheManager.Create,
    [TUserModule, TOrderModule])   // pinned modules
  .Start(TAppModule.Create);
```

Source: `TNidus.UseCache(ACache, AModules)` calls `ACache.SetPolicy(AModules)`.

### Cache all modules

```delphi
GetNidus
  .UseCache(TModuleCacheManager.Create)
  .Start(TAppModule.Create);

// Then enable globally:
GetNidus.Cache.EnableAll;
```

Or use the sentinel class in `SetPolicy`:

```delphi
GetNidus.Cache.SetPolicy([TNidusCacheAll]);
```

`TNidusCacheAll` is a marker class defined in `Nidus.Module.Cache.Interfaces`; passing it sets `FCacheAll := True`.

## Cache API (`IModuleCache`)

```delphi
var LCache := GetNidus.Cache;  // returns IModuleCache

// Pin additional modules at runtime
LCache.SetPolicy([TReportModule]);

// Enable caching for all modules
LCache.EnableAll;

// Disable caching and evict all cached instances
LCache.DisableAll;

// Evict a single module's cached instance
LCache.Invalidate(TUserModule);

// Check whether a module is cached
if LCache.IsEnabledFor(TUserModule) then ...;

// Evict all cached instances (without changing policy)
LCache.Clear;
```

## How `DisposeRouteModule` respects the cache

`TModuleProvider.DisposeModule` checks `IModuleCache.IsEnabledFor(LRoute.Module)` before calling `FreeAndNil(LRoute.ModuleInstance)`. If the module is cached, the instance is kept alive and reused on the next matching request.

## `TModuleCacheManager` internals

| Field | Type | Role |
|---|---|---|
| `FInstances` | `TObjectDictionary<string, TObject>` | Live cached instances, owns values |
| `FEnabled` | `TDictionary<string, Byte>` | Set of enabled module keys |
| `FCacheAll` | `Boolean` | Fast-path "cache everything" flag |
| `FLock` | `TCriticalSection` | Thread-safety for all operations |

`ResolveInstance(AModuleClass, AFactory)` is the core method: if the class is cached and an instance exists, it returns the existing one; otherwise it calls `AFactory()` and stores the result.

Source: `Nidus.Module.Cache` — `TModuleCacheManager`.
