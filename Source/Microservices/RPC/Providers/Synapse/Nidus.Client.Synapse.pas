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

// Optional RPC provider — requires the third-party Synapse library (blcksock/synapse).
// Define NIDUS_SYNAPSE (and have Synapse installed) to compile it in.
{$IFDEF NIDUS_SYNAPSE}

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
    constructor Create(const AHost: string; const APort: integer = 8080); override;
    destructor Destroy; override;
    function ExecuteRPC(const ARequest: string): string; override;
  end;

implementation

{ TRPCProviderClientSynapse }

constructor TRPCProviderClientSynapse.Create(const AHost: string; const APort: integer);
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

function TRPCProviderClientSynapse.ExecuteRPC(const ARequest: string): string;
begin
  try
    FTCPClient.Connect(FTCPClient.SocksIP, FTCPClient.SocksPort);
    FTCPClient.SendString(AnsiString(ARequest + CRLF));
    Result := string(FTCPClient.RecvString(5000));
  finally
    FTCPClient.CloseSocket;
  end;
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.
