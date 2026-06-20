---
title: API Reference
displayed_sidebar: nidusSidebar
sidebar_position: 1
---

# API Reference

## `TNidus` — application facade

`TNidus` is the single entry point for configuring and running a Nidus application. Obtain the singleton via `GetNidus`.

```delphi
function GetNidus: TNidus;
```

### Configuration methods (fluent, call before `Start`)

| Signature | Description |
|---|---|
| `class function UseListener(const AListener: TListener): TNidus` | Register a structured-log callback. Class method — call before `GetNidus`. |
| `function UseGuard(const AGuardCallback: TGuardCallback): TNidus` | Register a global authorization guard. |
| `function UsePipes(const AValidationPipe: IValidationPipe): TNidus` | Register the global validation pipe. |
| `function UseCache(const ACache: IModuleCache): TNidus` | Enable module caching globally. |
| `function UseCache(const ACache: IModuleCache; const AModules: array of TClass): TNidus` | Enable module caching for specific modules and apply policy. |
| `function UsePools(const ARegistry: IPoolRegistry): TNidus` | Set a custom pool registry. |
| `function UsePools<T: class, constructor>(const AMaxSize: Integer = 256): TNidus` | Register a default object pool for `T`. |
| `function UsePools<T: TComponent>(const AMaxSize: Integer = 32; const AOwner: TComponent = nil; const AReset: TProc<T> = nil): TNidus` | Register a default component pool for `T`. |
| `function UseComponentPool<T: TComponent>(const AMaxSize: Integer = 32; const AOwner: TComponent = nil; const AReset: TProc<T> = nil): TNidus` | Alias for `UsePools<T: TComponent>`. |
| `function UseRPC(const ARPCProviderServer: IRPCProviderServer): TNidus` | Set the RPC transport server and start it. |

### Bootstrap

| Signature | Description |
|---|---|
| `function Start(const AModule: TModule; const AInitialRoutePath: string = '/'): TNidus` | Boot the root module and start the route parser. Raises `EModuleStartedException` if called twice. |
| `procedure Finalize` | Free the module tree and tear down the DI container. |

### Runtime methods

| Signature | Description |
|---|---|
| `function Get<T: class, constructor>(ATag: string = ''): T` | Resolve a registered class by type (and optional tag). |
| `function GetInterface<I: IInterface>(ATag: string = ''): I` | Resolve a registered interface. |
| `function LoadRouteModule(const APath: string; const AReq: IRouteRequest = nil): TReturnPair` | Run guard, validation pipe, and route dispatch for the given path. |
| `procedure DisposeRouteModule(const APath: string)` | Walk path segments and dispose module instances for each segment. |
| `procedure RegisterRouteHandler(const ARouteHandler: TRouteHandlerClass)` | Register a route handler class with the validation registry. |
| `function PublishRPC(const ARPCName: string; const ARPCClass: TRPCResourceClass): TNidus` | Publish a named RPC handler. Raises `ERPCProviderNotSetException` if `UseRPC` was not called. |
| `function Cache: IModuleCache` | Return the active `IModuleCache` instance. |
| `function Pools: IPoolRegistry` | Return the active `IPoolRegistry` instance. |
| `function Request: IRouteRequest` | Return the `IRouteRequest` for the current in-flight request. |
| `procedure WithPool<T: class>(const AProc: TProc<T>)` | Acquire a pooled instance of `T`, run `AProc`, then release. |
| `procedure WithPool<T: class>(const AKey: string; const AProc: TProc<T>)` | Named-key variant of `WithPool`. |
| `procedure Listener(const AMessage: string)` | Emit a message through the registered listener. |

---

## `IModuleCache`

Defined in `Nidus.Module.Cache.Interfaces`.

| Method | Description |
|---|---|
| `function ResolveInstance(const AModuleClass: TClass; const AFactory: TFunc<TObject>): TObject` | Return cached instance or call factory and cache the result. |
| `procedure Invalidate(const AModuleClass: TClass)` | Evict cached instance for a specific module class. |
| `procedure Clear` | Evict all cached instances (policy unchanged). |
| `function SetPolicy(const AModuleClasses: array of TClass): IModuleCache` | Enable caching for the listed classes (use `TNidusCacheAll` to cache all). |
| `function EnableAll: IModuleCache` | Set `FCacheAll := True`. |
| `function DisableAll: IModuleCache` | Clear policy and evict all instances. |
| `function IsEnabledFor(const AModuleClass: TClass): Boolean` | Check whether a module is cached. |

---

## `IPoolRegistry`

Defined in `Nidus.Pooling.Interfaces`.

<!-- TODO: confirm full interface methods — unit not read in detail -->

---

## `TObjectPool<T>`

Defined in `Nidus.ObjectPool`.

| Method | Description |
|---|---|
| `function Acquire: T` | Dequeue existing instance or call `T.Create`. |
| `procedure Release(const AValue: T)` | Return instance to pool (frees if pool is full). |
| `function Count: Integer` | Current number of idle instances. |

Constructor: `Create(const AOptions: TObjectPoolOptions)`. `TObjectPoolOptions.MaxSize` (default 256) is the maximum idle capacity.

---

## `TMessageBus`

Defined in `Nidus.Message.Bus`.

| Method | Description |
|---|---|
| `procedure RegisterEvent<T>(const AName: string; const ACallback: TCallback<T>)` | Subscribe to a named topic. |
| `procedure UnregisterEvent(const AName: string)` | Remove a subscription. |
| `procedure Notify<T>(const AName: string; const AValue: T)` | Dispatch an event to matching subscribers (exact or wildcard prefix). |

---

## `TRPCPublish`

Defined in `Nidus.RPC.Publish`.

| Method | Description |
|---|---|
| `procedure PublishRPC(const ARPCName: string; const ARPCClass: TRPCResourceClass)` | Register a name → class mapping. |
| `procedure UnPublishRPC(const ARPCName: string)` | Remove a named RPC handler. |
| `function RPCs: TDictionary<string, TRPCResourceClass>` | Return the full registry (read-only use). |

---

## `TBind<T>` — provider binding

Defined in `Nidus.Bind`.

| Factory method | Lifecycle |
|---|---|
| `TBind<T>.Singleton(...)` | One instance, eager creation |
| `TBind<T>.SingletonLazy(...)` | One instance, lazy creation |
| `TBind<T>.Factory(...)` | New instance per resolution |
| `TBind<T>.SingletonInterface<I>(...)` | Singleton registered under interface `I` |
| `TBind<T>.AddInstance(AInstance)` | Use a pre-built instance |

All factory methods accept optional `AOnCreate: TProc<T>`, `AOnDestroy: TProc<T>`, and `AOnConstructorParams: TConstructorCallback` callbacks.

---

## Global functions

| Function | Unit | Description |
|---|---|---|
| `GetNidus: TNidus` | `Nidus` | Return the singleton `TNidus` instance from the DI container |
| `SetGlobalModuleCache(ACache)` | `Nidus.Module.Cache.Interfaces` | Set the global `IModuleCache` |
| `GetGlobalModuleCache: IModuleCache` | `Nidus.Module.Cache.Interfaces` | Get the global `IModuleCache` |
| `SetGlobalPoolRegistry(ARegistry)` | `Nidus.Pooling.Interfaces` | Set the global `IPoolRegistry` |
| `GetGlobalPoolRegistry: IPoolRegistry` | `Nidus.Pooling.Interfaces` | Get the global `IPoolRegistry` |
| `Nidus_Horse(AAppModule)` | `Nidus.Driver.Horse` | Bootstrap Nidus + return Horse middleware |
| `Nidus_Horse(ACharset)` | `Nidus.Driver.Horse` | Return Horse middleware without bootstrapping |
