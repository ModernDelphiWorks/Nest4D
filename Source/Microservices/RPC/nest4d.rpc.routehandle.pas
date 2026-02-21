{
             Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers?o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos ? permitido copiar e distribuir c?pias deste documento de
       licen?a, mas mud?-lo n?o ? permitido.

       Esta vers?o da GNU Lesser General Public License incorpora
       os termos e condi??es da vers?o 3 da GNU General Public License
       Licen?a, complementado pelas permiss?es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://Nest4D-en.docs-br.com)
}

unit Nest4D.Rpc.RouteHandle;

interface

uses
  System.Rtti,
  System.SysUtils,
  Nest4D.Rpc.Parse,
  Nest4D.Rpc.Publish,
  Nest4D.Rpc.Resource,
  Nest4D.Rpc.Interfaces;

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
  Nest4D.rpc.exception;

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






