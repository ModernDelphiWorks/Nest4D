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
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit Nest.Route.Param;

interface

uses
  SysUtils,
  Math,
  Rtti,
  Nest.Request;

type
  TRouteParam = record
  private
    FPath: String;
    FSchema: String;
    FRequest: IRouteRequest;
  public
    constructor Create(const APath: String;
      const AReq: IRouteRequest = nil; const ASchema: String = '');
    procedure ResolveURL;
    property Path: String read FPath;
    property Schema: String read FSchema;
    property Request: IRouteRequest read Frequest;
  end;

implementation

constructor TRouteParam.Create(const APath: String;
  const AReq: IRouteRequest; const ASchema: String);
begin
  FPath := APath;
  FSchema := ASchema;
  FRequest := AReq;
end;

procedure TRouteParam.ResolveURL;
begin
  FPath := FPath + '/';
end;

end.







