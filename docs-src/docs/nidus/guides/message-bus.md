---
title: Message Bus
displayed_sidebar: nidusSidebar
sidebar_position: 8
---

# Message Bus

The `TMessageBus` class provides in-process, generic publish/subscribe messaging with wildcard topic matching.

## Overview

`TMessageBus` lives in `Nidus.Message.Bus`. It holds a `TDictionary<String, TValue>` of named callbacks, where `TValue` wraps a `TCallback<T>` anonymous method. Events are dispatched synchronously on the calling thread.

## Registering a listener

```delphi
var LBus := TMessageBus.Create;

// Exact topic
LBus.RegisterEvent<TOrderCreated>('order.created',
  procedure(const E: TOrderCreated)
  begin
    WriteLn('Order created: ', E.Id);
  end);

// Wildcard prefix — matches 'order.created', 'order.cancelled', etc.
LBus.RegisterEvent<TOrderEvent>('order.*',
  procedure(const E: TOrderEvent)
  begin
    WriteLn('Any order event');
  end);
```

`TCallback<T> = reference to procedure(const Value: T)` — defined in `Nidus.Message.Bus`.

## Publishing an event

```delphi
var LEvent := TOrderCreated.Create(42);
LBus.Notify<TOrderCreated>('order.created', LEvent);
```

### Wildcard dispatch rules

`Notify<T>(AName, AValue)`:

1. If `AName` exists verbatim in the registry → invoke that callback and exit.
2. If `AName` ends with `*` → strip the `*` to form `LPattern`; invoke all keys that start with `LPattern`.
3. Otherwise use `AName` as `LPattern` and match all keys that start with it.

Example: `Notify<TOrderEvent>('order.*', E)` invokes all callbacks whose key starts with `'order.'`.

## Unregistering a listener

```delphi
LBus.UnregisterEvent('order.created');
```

## Integration with Nidus modules

`TMessageBus` is a standalone class — it is not automatically wired into the DI container. Register it as a singleton in your module if you want DI resolution:

```delphi
procedure TEventModule.Configure(const ABuilder: IModuleBuilder);
begin
  ABuilder
    .AddProvider(TBind<TMessageBus>.Singleton)
    .Export(TMessageBus);
end;
```

Then resolve normally:

```delphi
var LBus := GetNidus.Get<TMessageBus>;
```

:::note
The `Nidus.Message.Bus.Module` unit (`Nidus.Message.Bus.Module.pas`) provides a pre-built module definition for the message bus. The class is `TMessageBusModule` (extends `TModule`) and exports `TMessageBus` as a singleton bind.
:::

## Lifecycle

`TMessageBus` owns no external resources. `Destroy` clears and frees the internal dictionary. No thread-safety is provided — if you publish from multiple threads, wrap calls in a `TCriticalSection`.
