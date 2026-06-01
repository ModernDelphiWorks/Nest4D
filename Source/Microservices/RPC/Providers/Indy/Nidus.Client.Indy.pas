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

unit Nidus.Client.Indy;

interface

uses
  SysUtils,
  Classes,
  IdTCPClient,
  Nidus.RPC.Client;

type
  TIdCustomTCPClientHacker = class(IdTCPClient.TIdTCPClientCustom);

  TRPCProviderClientIndy = class(TRPCProviderClient)
  private
    FTCPClient: TIdTCPClientCustom;
  public
    constructor Create(const AHost: String; const APort: integer = 8080); override;
    destructor Destroy; override;
    function ExecuteRPC(const ARequest: String): String; override;
  end;

implementation

{ TRPCProviderClientIndy }

constructor TRPCProviderClientIndy.Create(const AHost: String; const APort: integer);
begin
  inherited Create(AHost, APort);
  FTCPClient := TIdTCPClientCustom.Create(nil);
  TIdCustomTCPClientHacker(FTCPClient).Host := AHost;
  TIdCustomTCPClientHacker(FTCPClient).Port := APort;
end;

destructor TRPCProviderClientIndy.Destroy;
begin
  FTCPClient.Free;
  inherited;
end;

function TRPCProviderClientIndy.ExecuteRPC(const ARequest: String): String;
begin
  try
    FTCPClient.Connect;
    FTCPClient.IOHandler.WriteLn(ARequest);
    Result := FTCPClient.IOHandler.ReadLn;
  finally
    FTCPClient.Disconnect;
  end;
end;

end.






