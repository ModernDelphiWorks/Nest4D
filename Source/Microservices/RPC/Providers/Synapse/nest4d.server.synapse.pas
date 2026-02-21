{
             Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit nest4d.server.synapse;

interface

uses
  SysUtils,
  Classes,
  blcksock,
  synsock,
  synautil,
  nest4d.rpc.server;

type
  TRPCProviderServerSynapse = class(TRPCProviderServer)
  private
    FTCPServer: TTCPBlockSocket;
    FTerminated: Boolean;
    procedure _HandleClientConnection(AClientSocket: TTCPBlockSocket);
  public
    constructor Create(const AHost: String; const APort: integer = 8080); override;
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

constructor TRPCProviderServerSynapse.Create(const AHost: String; const APort: integer);
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
  LRequestData: String;
  LResponseData: String;
begin
  LRequestData := String(AClientSocket.RecvTerminated(5000, #10));
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

end.








