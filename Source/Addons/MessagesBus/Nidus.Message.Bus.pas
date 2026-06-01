{
  ------------------------------------------------------------------------------
  Nidus
  Modular and scalable application framework for Delphi, inspired by the architectural patterns of NestJS.

  SPDX-License-Identifier: MIT
  Copyright (c) 2025-2026 Isaque Pinheiro

  Licensed under the MIT License.
  See the LICENSE file in the project root for full license information.
  ------------------------------------------------------------------------------
}

unit Nidus.Message.Bus;

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




