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
  @documentation(https://nest4d-en.docs-br.com)
}

unit Nest4D.Validation.Arguments;

interface

uses
  System.Rtti,
  Nest4D.Validation.Interfaces;

type
  TValidationArguments = class(TInterfacedObject, IValidationArguments)
  private
    FValues: TArray<TValue>;
    FObjectType: TClass;
    FTagName: String;
    FFieldName: String;
    FMessage: String;
    FTypeName: String;
  public
    constructor Create(const AValues: TArray<TValue>;
      const ATagName: String; const AFieldName: String; const AMessage: String;
      const ATypeName: String; const AObjectType: TClass);
    function Values: TArray<TValue>;
    function TagName: String;
    function FieldName: String;
    function Message: String;
    function TypeName: String;
    function ObjectType: TClass;
  end;

implementation

{ TArgumentMetadata }

function TValidationArguments.ObjectType: TClass;
begin
  Result := FObjectType;
end;

constructor TValidationArguments.Create(const AValues: TArray<TValue>;
  const ATagName: String; const AFieldName: String;
  const AMessage: String; const ATypeName: String; const AObjectType: TClass);
begin
  FTagName := ATagName;
  FFieldName := AFieldName;
  FValues := AValues;
  FMessage := AMessage;
  FTypeName := ATypeName;
  FObjectType := AObjectType;
end;

function TValidationArguments.FieldName: String;
begin
  Result := FFieldName;
end;

function TValidationArguments.Message: String;
begin
  Result := FMessage;
end;

function TValidationArguments.TagName: String;
begin
  Result := FTagName;
end;

function TValidationArguments.TypeName: String;
begin
  Result := FTypeName;
end;

function TValidationArguments.Values: TArray<TValue>;
begin
  Result := FValues;
end;

end.





