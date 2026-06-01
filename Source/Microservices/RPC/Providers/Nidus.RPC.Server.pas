{
  ------------------------------------------------------------------------------
  Nidus
  Modular and scalable application framework for Delphi, inspired by the architectural patterns of NestJS.

  SPDX-License-Identifier: MIT
  Copyright (c) 2025-2026 Isaque Pinheiro

  Licensed under the MIT License.
  See the LICENSE file in the project root for full license information.
  ------------------------------------------------------------------------------
}

unit Nidus.RPC.Server;

interface

uses
  Nidus.RPC.RouteHandle,
  Nidus.RPC.Interfaces,
  Nidus.RPC.Resource;

type
  TRPCProviderServer = class(TInterfacedObject, IRPCProviderServer)
  private
    FRPCRouteHandle: IRPCRouteHandle;
  protected
    FHost: String;
    FPort: integer;
  public
    constructor Create(const AHost: String; const APort: integer = 8080); virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    function ExecuteRPC(const ARequest: String): String;
  end;

implementation

{ TTCPRPCProvider }

constructor TRPCProviderServer.Create(const AHost: String; const APort: integer);
begin
  FHost := AHost;
  FPort := APort;
  FRPCRouteHandle := TRPCRouteHandle.Create;
end;

destructor TRPCProviderServer.Destroy;
begin
  inherited;
end;

function TRPCProviderServer.ExecuteRPC(const ARequest: String): String;
begin
  Result := FRPCRouteHandle.ExecuteRPC(ARequest);
end;

procedure TRPCProviderServer.PublishRPC(const ARPCName: String;
  const ARPCClass: TRPCResourceClass);
begin
  FRPCRouteHandle.PublishRPC(ARPCName, ARPCClass);
end;

procedure TRPCProviderServer.Start;
begin

end;

procedure TRPCProviderServer.Stop;
begin

end;

end.






