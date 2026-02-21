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

unit nest4d.transform.arguments;

interface

uses
  Rtti,
  nest4d.transform.interfaces;

type
  TTransformArguments = class(TInterfacedObject, ITransformArguments)
  private
    FValues: TArray<TValue>;
    FTagName: String;
    FFieldName: String;
    FObjectType: TClass;
    FMessage: String;
  public
    constructor Create(const AValues: TArray<TValue>;
      const ATagName: String; const AFieldName: String;
      const AMessage: String; const AObjectType: TClass);
    function TagName: String;
    function FieldName: String;
    function Values: TArray<TValue>;
    function Message: String;
    function ObjectType: TClass;
  end;

implementation

{ TConverterArguments }

constructor TTransformArguments.Create(const AValues: TArray<TValue>;
  const ATagName: String; const AFieldName: String;
  const AMessage: String; const AObjectType: TClass);
begin
  FTagName := ATagName;
  FFieldName := AFieldName;
  FValues := AValues;
  FMessage := AMessage;
  FObjectType := AObjectType;
end;

function TTransformArguments.FieldName: String;
begin
  Result := FFieldName;
end;

function TTransformArguments.Message: String;
begin
  Result := FMessage;
end;

function TTransformArguments.ObjectType: TClass;
begin
  Result := FObjectType;
end;

function TTransformArguments.TagName: String;
begin
  Result := FTagName;
end;

function TTransformArguments.Values: TArray<TValue>;
begin
  Result := FValues;
end;

end.




