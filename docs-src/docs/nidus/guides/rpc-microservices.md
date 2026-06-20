---
title: RPC Microservices
displayed_sidebar: nidusSidebar
sidebar_position: 7
---

# RPC Microservices

Nidus enables native socket-based RPC communication between services. A server publishes named handlers; clients invoke them by name over TCP.

## Server side

### 1. Choose a provider

Two transport providers are included:

| Provider | Unit | Notes |
|---|---|---|
| Indy | `Nidus.Server.Indy` | `TRPCServerIndy` |
| Synapse | `Nidus.Server.Synapse` | `TRPCServerSynapse` |

Both inherit from `TRPCProviderServer` (`Nidus.RPC.Server`).

### 2. Register and publish

```delphi
uses
  Nidus,
  Nidus.RPC.Server,
  Nidus.Server.Indy;  // or Nidus.Server.Synapse

var LServer: TRPCServerIndy;
begin
  LServer := TRPCServerIndy.Create('0.0.0.0', 9900);

  GetNidus
    .UseRPC(LServer)
    .PublishRPC('GetUser',  TGetUserRPCResource)
    .PublishRPC('CreateOrder', TCreateOrderRPCResource)
    .Start(TAppModule.Create);
end;
```

`UseRPC` calls `IRPCProviderServer.Start` to begin listening. `PublishRPC` registers a name → `TRPCResourceClass` mapping in the `TRPCRouteHandle` registry.

### 3. Implement an RPC resource

```delphi
uses
  Nidus.RPC.Resource;

type
  TGetUserRPCResource = class(TRPCResource)
  public
    function Execute(const ARequest: string): string; override;
  end;

function TGetUserRPCResource.Execute(const ARequest: string): string;
begin
  // ARequest is the raw JSON payload sent by the client
  Result := '{"id":1,"name":"Isaque"}';
end;
```

`TRPCResourceClass = class of TRPCResource` — defined in `Nidus.RPC.Resource`.

### 4. Dispatch (internal)

`TRPCRouteHandle.ExecuteRPC(ARequest)` parses the request (via `TRPCParse`), looks up the registered class by name, instantiates it, calls `Execute`, and returns the result string.

## Client side

```delphi
uses
  Nidus.RPC.Client,
  Nidus.Client.Indy;  // or Nidus.Client.Synapse

var LClient: TRPCClientIndy;
begin
  LClient := TRPCClientIndy.Create('127.0.0.1', 9900);
  try
    var LResponse := LClient.ExecuteRPC('{"rpc":"GetUser","payload":{"id":1}}');
    // process LResponse
  finally
    LClient.Free;
  end;
end;
```

`TRPCProviderClient.ExecuteRPC` is the abstract base; `TRPCClientIndy` / `TRPCClientSynapse` provide the actual TCP transport. <!-- TODO: confirm exact JSON envelope format from Nidus.RPC.Parse -->

## Unpublishing

```delphi
// via TRPCPublish directly (if you hold a reference)
LPublish.UnPublishRPC('GetUser');
```

Source: `Nidus.RPC.Publish` — `TRPCPublish.UnPublishRPC`.

## Architecture

```
TNidus.UseRPC(IRPCProviderServer)
  └── IRPCProviderServer.Start   (Indy / Synapse TCP listener)

TNidus.PublishRPC(name, class)
  └── IRPCProviderServer.PublishRPC
        └── TRPCRouteHandle.PublishRPC
              └── TRPCPublish registry (name → TRPCResourceClass)

Incoming TCP request
  └── TRPCProviderServer.ExecuteRPC(raw)
        └── TRPCRouteHandle.ExecuteRPC
              └── TRPCParse → name lookup → TRPCResource.Execute
```
