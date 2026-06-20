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

unit Nidus.Server.Synapse;

// Optional RPC provider — requires the third-party Synapse library (blcksock/synsock/synautil).
// Define NIDUS_SYNAPSE (and have Synapse installed) to compile it in.
{$IFDEF NIDUS_SYNAPSE}

interface

uses
  SysUtils,
  Classes,
  blcksock,
  synsock,
  synautil,
  Nidus.RPC.Server;

type
  TRPCProviderServerSynapse = class(TRPCProviderServer)
  private
    FTCPServer: TTCPBlockSocket;
    FTerminated: Boolean;
    procedure _HandleClientConnection(AClientSocket: TTCPBlockSocket);
  public
    constructor Create(const AHost: string; const APort: integer = 8080); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TTCPServerThread = class(TThread)
  private
    FServer: TRPCProviderServerSynapse;
  public
    constructor Create(const AServer: TRPCProviderServerSynapse);
    procedure Execute; override;
  end;

implementation

{ TRPCProviderServerSynapse }

constructor TRPCProviderServerSynapse.Create(const AHost: string; const APort: integer);
begin
  inherited Create(AHost, APort);
  FTCPServer := TTCPBlockSocket.Create;
  FTerminated := False;
end;

destructor TRPCProviderServerSynapse.Destroy;
begin
  FTCPServer.Free;
  inherited;
end;

procedure TRPCProviderServerSynapse._HandleClientConnection(AClientSocket: TTCPBlockSocket);
var
  LRequestData: string;
  LResponseData: string;
begin
  LRequestData := string(AClientSocket.RecvTerminated(5000, #10));
  LResponseData := ExecuteRPC(LRequestData);
  AClientSocket.SendString(AnsiString(LResponseData + CRLF));
  AClientSocket.CloseSocket;
end;

procedure TRPCProviderServerSynapse.Start;
begin
  TTCPServerThread.Create(Self).Start;
end;

procedure TRPCProviderServerSynapse.Stop;
begin
  FTerminated := True;
end;

{ TServerThread }

constructor TTCPServerThread.Create(const AServer: TRPCProviderServerSynapse);
begin
  inherited Create(True);
  FServer := AServer;
end;

procedure TTCPServerThread.Execute;
var
  LClientSocket: TTCPBlockSocket;
begin
  FServer.FTCPServer.Bind(FServer.FHost, IntToStr(FServer.FPort));
  FServer.FTCPServer.Listen;
  while not FServer.FTerminated do
  begin
    if FServer.FTCPServer.CanRead(100) then
    begin
      LClientSocket := TTCPBlockSocket.Create;
      try
        LClientSocket.Socket := FServer.FTCPServer.Accept;
        FServer._HandleClientConnection(LClientSocket);
      except
        LClientSocket.Free;
      end;
    end;
  end;
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.
