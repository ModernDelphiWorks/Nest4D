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

unit Nest4D.Rpc.Interfaces;

interface

uses
  Nest4D.Rpc.Resource;

type
  IRPCProviderServer = interface
    ['{B6ABE323-4FD8-49DF-9D1F-208FF424A872}']
    procedure Start;
    procedure Stop;
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    function ExecuteRPC(const AContext: String): String;
  end;

  IRPCProviderClient = interface
    ['{53B122DB-B9DB-434F-A0DA-4A8EE44EA842}']
    function ExecuteRPC(const AContext: String): String;
  end;

  IRPCRouteHandle = interface
    ['{7C0BFB60-92F3-430B-A119-479A07F58EC4}']
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    function ExecuteRPC(const AContext: String): String;
  end;

implementation

end.








