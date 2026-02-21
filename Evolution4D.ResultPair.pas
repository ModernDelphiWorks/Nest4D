unit Evolution4D.ResultPair;

interface

uses
  System.SysUtils;

type
  TResultPair<T, E: Exception> = record
  private
    FValue: T;
    FException: E;
    FIsSuccess: Boolean;
  public
    class function Success(const AValue: T): TResultPair<T, E>; static;
    class function Failure(const AException: E): TResultPair<T, E>; static;
    
    function IsSuccess: Boolean;
    function IsFailure: Boolean;
    
    function Value: T;
    function Exception: E;
    
    function SuccessOrDefault(const ADefault: T): T;
    function FailureOrException: E;
    
    function Map<U>(const AFunc: TFunc<T, U>): TResultPair<U, E>;
    function FlatMap<U>(const AFunc: TFunc<T, TResultPair<U, E>>): TResultPair<U, E>;
    
    function ValueSuccess: T;
    function ValueFailure: E;
    
    function SuccessOrElse(const AFunc: TFunc<E, T>): T;
    
    procedure When(const AOnSuccess: TProc<T>; const AOnFailure: TProc<E>);
  end;

implementation

{ TResultPair<T, E> }

class function TResultPair<T, E>.Success(const AValue: T): TResultPair<T, E>;
begin
  Result.FValue := AValue;
  Result.FException := nil;
  Result.FIsSuccess := True;
end;

class function TResultPair<T, E>.Failure(const AException: E): TResultPair<T, E>;
begin
  Result.FValue := Default(T);
  Result.FException := AException;
  Result.FIsSuccess := False;
end;

function TResultPair<T, E>.IsSuccess: Boolean;
begin
  Result := FIsSuccess;
end;

function TResultPair<T, E>.IsFailure: Boolean;
begin
  Result := not FIsSuccess;
end;

function TResultPair<T, E>.Value: T;
begin
  if not FIsSuccess then
    raise Exception.Create('Cannot get value from failed result');
  Result := FValue;
end;

function TResultPair<T, E>.Exception: E;
begin
  if FIsSuccess then
    raise System.SysUtils.Exception.Create('Cannot get exception from successful result');
  Result := FException;
end;

function TResultPair<T, E>.SuccessOrDefault(const ADefault: T): T;
begin
  if FIsSuccess then
    Result := FValue
  else
    Result := ADefault;
end;

function TResultPair<T, E>.FailureOrException: E;
begin
  if FIsSuccess then
    Result := nil
  else
    Result := FException;
end;

function TResultPair<T, E>.Map<U>(const AFunc: TFunc<T, U>): TResultPair<U, E>;
begin
  if FIsSuccess then
    Result := TResultPair<U, E>.Success(AFunc(FValue))
  else
    Result := TResultPair<U, E>.Failure(FException);
end;

function TResultPair<T, E>.FlatMap<U>(const AFunc: TFunc<T, TResultPair<U, E>>): TResultPair<U, E>;
begin
  if FIsSuccess then
    Result := AFunc(FValue)
  else
    Result := TResultPair<U, E>.Failure(FException);
end;

function TResultPair<T, E>.ValueSuccess: T;
begin
  Result := Value;
end;

function TResultPair<T, E>.ValueFailure: E;
begin
  Result := Exception;
end;

function TResultPair<T, E>.SuccessOrElse(const AFunc: TFunc<E, T>): T;
begin
  if FIsSuccess then
    Result := FValue
  else
    Result := AFunc(FException);
end;

procedure TResultPair<T, E>.When(const AOnSuccess: TProc<T>; const AOnFailure: TProc<E>);
begin
  if FIsSuccess then
    AOnSuccess(FValue)
  else
    AOnFailure(FException);
end;

end.