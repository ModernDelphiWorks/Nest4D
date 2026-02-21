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

unit Nest4D.Parse.Datetime.Pipe;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  Nest4D.Transform.Pipe,
  Nest4D.Transform.Interfaces;


type
  TParseDateTimePipe = class(TTransformPipe)
  public
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

function TParseDateTimePipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LValue: TDateTime;
  LMessage: String;
begin
  if TryStrToDateTime(Value.ToString, LValue) then
    Result.Success(LValue)
  else
  begin
    LMessage := ifThen(Metadata.Message = '',
                       Format('[%s] %s-> [%s] Validation failed (datetime String is expected)', [Metadata.TagName,
                                                                                                 Self.ClassName,
                                                                                                 Metadata.FieldName]), Metadata.Message);
    Result.Failure(LMessage);
  end;
end;

end.







