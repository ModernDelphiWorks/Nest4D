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

unit Nidus.Server.Indy;

// Optional RPC provider — requires the Indy library (IdContext/IdCustomTCPServer).
// Define NIDUS_INDY (and have Indy installed) to compile it in.
{$IFDEF NIDUS_INDY}

interface

uses
  SysUtils,
  Classes,
  IdContext,
  IdTCPConnection,
  IdCustomTCPServer,
  Nidus.RPC.Server;

type
  TIdCustomTCPServerHacker = class(IdCustomTCPServer.TIdCustomTCPServer);

  TRPCProviderServerIndy = class(TRPCProviderServer)
  private
    FTCPServer: TIdCustomTCPServer;
    procedure _OnExecute(AContext: TIdContext);
  public
    constructor Create(const AHost: string; const APort: integer = 8080); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

{ TTCPRPCProviderIndy }

constructor TRPCProviderServerIndy.Create(const AHost: string; const APort: integer);
begin
  inherited Create(AHost, APort);
  FTCPServer := TIdCustomTCPServer.Create(nil);
  FTCPServer.Bindings.Add.IP := FHost;
  FTCPServer.Bindings.Add.Port := FPort;
  TIdCustomTCPServerHacker(FTCPServer).FOnExecute := _OnExecute;
end;

destructor TRPCProviderServerIndy.Destroy;
begin
  FTCPServer.Free;
  inherited;
end;

procedure TRPCProviderServerIndy._OnExecute(AContext: TIdContext);
var
  LResponseData: string;
begin
  LResponseData := ExecuteRPC(AContext.Connection.IOHandler.ReadLn);
  AContext.Connection.IOHandler.WriteLn(LResponseData);
  AContext.Connection.Disconnect;
end;

procedure TRPCProviderServerIndy.Start;
begin
  FTCPServer.Active := True;
end;

procedure TRPCProviderServerIndy.Stop;
begin
  FTCPServer.Active := False;
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.
