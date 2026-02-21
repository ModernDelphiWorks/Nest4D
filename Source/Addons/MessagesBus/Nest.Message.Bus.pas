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

unit Nest.Message.Bus;

interface

uses
  Rtti,
  Classes,
  StrUtils,
  SysUtils,
  Generics.Collections;

type
  TCallback<T> = reference to procedure(const Value: T);

  TMessageBus = class
  private
    FCallbackList: TDictionary<String, TValue>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterEvent<T>(const AName: String;
      const ACallback: TCallback<T>);
    procedure UnregisterEvent(const AName: String);
    procedure Notify<T>(const AName: String; const AValue: T);
  end;

implementation

{ TEventEmitter }

constructor TMessageBus.Create;
begin
  FCallbackList := TDictionary<String, TValue>.Create;
end;

destructor TMessageBus.Destroy;
begin
  FCallbackList.Clear;
  FCallbackList.Free;
  inherited;
end;

procedure TMessageBus.RegisterEvent<T>(const AName: String;
 const ACallback: TCallback<T>);
begin
  FCallbackList.AddOrSetValue(AName, TValue.From<TCallback<T>>(ACallback));
end;

procedure TMessageBus.UnregisterEvent(const AName: String);
begin
  FCallbackList.Remove(AName);
end;

procedure TMessageBus.Notify<T>(const AName: String; const AValue: T);
var
  LKey: String;
  LCallback: TValue;
  LPattern: String;
begin
  if FCallbackList.ContainsKey(AName) then
  begin
    LCallback := FCallbackList[AName];
    LCallback.AsType<TCallback<T>>()(AValue);
    Exit;
  end;
  // AEventName = 'FirstName_*'.
  // Notify = FirstName_LastName, FirstName_Alias etc...
  if EndsText('*', AName) then
    LPattern := Copy(AName, 1, Length(AName) - 1)
  else
    LPattern := AName;

  for LKey in FCallbackList.Keys do
  begin
    if not StartsText(LPattern, LKey) then
      Continue;
    LCallback := FCallbackList[LKey];
    LCallback.AsType<TCallback<T>>()(AValue);
  end;
end;


end.




