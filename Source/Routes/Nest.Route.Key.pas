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
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit Nest.Route.Key;

interface

uses
  SysUtils;

type
  TRouteKey = record
  public
    Path: String;
    Schema: String;
    constructor Create(const APath: String; const ASchema: String);
    function CopyWith(const APath: String; const ASchema: String): TRouteKey;
    function GetHashCode: Int64;
    class operator Equal(const ALeft, ARight: TRouteKey): Boolean;
    class operator NotEqual(const ALeft, ARight: TRouteKey): Boolean;
  end;

implementation

{ TRouteKey }

constructor TRouteKey.Create(const APath: String; const ASchema: String);
begin
  Schema := ASchema;
  Path := APath;
end;

function TRouteKey.CopyWith(const APath: String; const ASchema: String): TRouteKey;
begin
  Result.Schema := ASchema;
  Result.Path := APath;
end;

class operator TRouteKey.Equal(const ALeft, ARight: TRouteKey): Boolean;
begin
  Result := (ALeft.Schema = ARight.Schema) and (ALeft.Path = ARight.Path);
end;

class operator TRouteKey.NotEqual(const ALeft, ARight: TRouteKey): Boolean;
begin
  Result := not (ALeft = ARight);
end;

function TRouteKey.GetHashCode: Int64;
begin
  Result := Schema.GetHashCode + Path.GetHashCode;
end;

end.







