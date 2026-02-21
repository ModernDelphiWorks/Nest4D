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

unit Nest4D.Client.Indy;

interface

uses
  System.SysUtils,
  System.Classes,
  IdTCPClient,
  Nest4D.Rpc.Client;

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







