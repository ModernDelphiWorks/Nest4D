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

unit Nidus.Client.Synapse;

interface

uses
  SysUtils,
  Classes,
  blcksock,
  Nidus.RPC.Client;

type
  TRPCProviderClientSynapse = class(TRPCProviderClient)
  private
    FTCPClient: TTCPBlockSocket;
  public
    constructor Create(const AHost: String; const APort: integer = 8080); override;
    destructor Destroy; override;
    function ExecuteRPC(const ARequest: String): String; override;
  end;

implementation

{ TRPCProviderClientSynapse }

constructor TRPCProviderClientSynapse.Create(const AHost: String; const APort: integer);
begin
  inherited Create(AHost, APort);
  FTCPClient := TTCPBlockSocket.Create;
  FTCPClient.SocksIP := AHost;
  FTCPClient.SocksPort := IntToStr(APort);
end;

destructor TRPCProviderClientSynapse.Destroy;
begin
  FTCPClient.Free;
  inherited;
end;

function TRPCProviderClientSynapse.ExecuteRPC(const ARequest: String): String;
begin
  try
    FTCPClient.Connect(FTCPClient.SocksIP, FTCPClient.SocksPort);
    FTCPClient.SendString(AnsiString(ARequest + CRLF));
    Result := String(FTCPClient.RecvString(5000));
  finally
    FTCPClient.CloseSocket;
  end;
end;

end.







