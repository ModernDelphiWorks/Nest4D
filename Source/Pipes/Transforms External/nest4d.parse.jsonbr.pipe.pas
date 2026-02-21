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

unit Nest4D.Parse.JsonBr.Pipe;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  Nest4D.Transform.pipe,
  Nest4D.Transform.interfaces;


type
  TParseJsonBrPipe = class(TTransformPipe)
  public
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

uses
  jsonbr,
  jsonbr.builders;

{ TParseJsonBrPipe }

function TParseJsonBrPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LObject: TObject;
  LObjects: TObjectList<TObject>;
  LIsArray: Boolean;
begin
  LIsArray := Value.AsString[1] = '[';
  try
    if LIsArray then
    begin
      LObjects := TObjectList<TObject>.Create;
      LObjects := TJsonBr.JsonToObjectList(Value.AsString, Metadata.ObjectType);
      Result.Success(LObjects);
    end
    else
    begin
      LObject := Metadata.ObjectType.Create;
      TJsonBr.JsonToObject(Value.AsString, LObject);
      Result.Success(LObject);
    end;
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

end.




