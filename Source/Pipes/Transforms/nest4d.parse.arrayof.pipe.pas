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

unit nest4d.parse.arrayof.pipe;

interface

uses
  Rtti,
  Types,
  SysUtils,
  StrUtils,
  Classes,
  Generics.Collections,
  nest4d.transform.pipe,
  nest4d.transform.interfaces;


type
  TParseArrayPipe = class(TTransformPipe)
  public
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

function TParseArrayPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LValue: TStringDynArray;
  LMessage: String;
begin
  LValue := SplitString(Value.AsString, Metadata.Values[0].AsString);
  if Length(LValue) > 1 then
    Result.Success(TValue.From<TStringDynArray>(LValue))
  else
  begin
    LMessage := ifThen(Metadata.Message = '',
                       Format('[%s] %s-> [%s] Validation failed (array String is expected)', [Metadata.TagName,
                                                                                              Self.ClassName,
                                                                                              Metadata.FieldName]), Metadata.Message);
    Result.Failure(LMessage);
  end;
end;

end.




