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

unit Nidus.RPC.RouteHandle;

interface

uses
  Rtti,
  SysUtils,
  Nidus.RPC.Parse,
  Nidus.RPC.Publish,
  Nidus.RPC.Resource,
  Nidus.RPC.Interfaces;

type
  TRPCRouteHandle = class(TInterfacedObject, IRPCRouteHandle)
  private
    const
      RPCNOTFOUND = '{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method %s not found"},"id":%s}';
      RPCRESPONSE = '{"jsonrpc":"2.0","result":%s,"id":%s}';
  private
    FParseRPC: TRPCParse;
    FPublishRPC: TRPCPublish;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    function ExecuteRPC(const ARequest: String): String;
  end;

implementation

uses
  Nidus.RPC.Exception;

{ TRouteHandleRPC }

constructor TRPCRouteHandle.Create;
begin
  FParseRPC := TRPCParse.Create;
  FPublishRPC := TRPCPublish.Create;
end;

destructor TRPCRouteHandle.Destroy;
begin
  FParseRPC.Free;
  FPublishRPC.Free;
  inherited;
end;

function TRPCRouteHandle.ExecuteRPC(const ARequest: String): String;
var
  LContext: TRttiContext;
  LMethod: TRttiMethod;
  LResource: TRPCResourceClass;
  LResult: TValue;
  LRPCID: String;
  LRPCName: String;
  LRPCParams: TArray<TValue>;
begin
  FParseRPC.RPCParseRequest(ARequest, LRPCID, LRPCName, LRPCParams);
  if not FPublishRPC.RPCs.ContainsKey(LRPCName) then
  begin
    Result := Format(RPCNOTFOUND, [LRPCName, LRPCID]);
    exit;
  end;
  LResource := FPublishRPC.RPCs[LRPCName];
  LContext := TRttiContext.Create;
  try
    LMethod := LContext.GetType(LResource).GetMethod(LRPCName);
    if not Assigned(LMethod) then
    begin
      Result := Format(RPCNOTFOUND, [LRPCName, LRPCID]);
      exit;
    end;
    LResult := LMethod.Invoke(LResource, LRPCParams);
    Result := Format(RPCRESPONSE, [LResult.ToString, LRPCID]);
  finally
    LContext.Free;
  end;
end;

procedure TRPCRouteHandle.PublishRPC(const ARPCName: String;
  const ARPCClass: TRPCResourceClass);
begin
  FPublishRPC.RPCs.AddOrSetValue(ARPCName, ARPCClass);
end;

end.





