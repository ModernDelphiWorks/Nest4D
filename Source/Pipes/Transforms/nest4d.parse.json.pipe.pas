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

unit Nest4D.Parse.Json.Pipe;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  Nest4D.Validation.Parse.Json,
  Nest4D.Transform.Pipe,
  Nest4D.Transform.Interfaces;


type
  TParseJsonPipe = class(TTransformPipe)
  private
    FJsonMap: TJsonMapped;
  public
    constructor Create;
    destructor Destroy; override;
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

{ TParseJsonPipe }

constructor TParseJsonPipe.Create;
begin
  FJsonMap := TJsonMapped.Create([doOwnsValues]);
end;

destructor TParseJsonPipe.Destroy;
begin
  FJsonMap.Free;
  inherited;
end;

function TParseJsonPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LKey: String;
begin
  try
    TJsonMap.Map(Value.AsString, Metadata.ObjectType,
      procedure (const AClassType: TClass; const AFieldName: String; const AValue: TValue)
      begin
        LKey := AClassType.ClassName + '->' + AFieldName;
        if not FJsonMap.ContainsKey(LKey) then
          FJsonMap.Add(LKey, TList<TValue>.Create);
        FJsonMap[LKey].Add(AValue);
      end);
    Result.Success(FJsonMap);
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

end.








