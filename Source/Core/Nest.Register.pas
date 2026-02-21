unit Nest.Register;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  Nest.Validation.interfaces,
  Nest.Request,
  Nest.Route.Handler;

type
  TRegister = class
  strict private
    FRegisters: TDictionary<String, TRouteHandlerClass>;
    FRoutes: TDictionary<String, String>;
    FValidationPipe: IValidationPipe;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AClass: TRouteHandlerClass); overload;
    procedure Add(const AKey: String; const ARouteHandleName: String); overload;
    procedure UsePipes(const AValidationPipe: IValidationPipe);
    function ResgisterContainsKey(const AKey: String): Boolean;
    function RouteContainsKey(const AKey: String): Boolean;
    function FindRecord(const AKey: String): TRouteHandlerClass;
    function Pipe: IValidationPipe;
    function IsValidationPipe: Boolean;
  end;

implementation

{ TRegister }

constructor TRegister.Create;
begin
  FRegisters := TDictionary<String, TRouteHandlerClass>.Create;
  FRoutes := TDictionary<String, String>.Create;
end;

destructor TRegister.Destroy;
begin
  FRoutes.Free;
  FRegisters.Free;
end;

function TRegister.FindRecord(const AKey: String): TRouteHandlerClass;
begin
  Result := nil;
  if not FRoutes.ContainsKey(AKey) then
    exit;
  if not FRegisters.ContainsKey(FRoutes[AKey]) then
    exit;
  Result := FRegisters[FRoutes[AKey]];
end;

function TRegister.IsValidationPipe: Boolean;
begin
  Result := FValidationPipe <> nil;
end;

procedure TRegister.Add(const AClass: TRouteHandlerClass);
begin
  if not FRegisters.ContainsKey(AClass.ClassName) then
    FRegisters.Add(AClass.ClassName, AClass);
end;

procedure TRegister.Add(const AKey: String; const ARouteHandleName: String);
begin
  if not FRoutes.ContainsKey(AKey) then
    FRoutes.Add(AKey, ARouteHandleName);
end;

function TRegister.ResgisterContainsKey(const AKey: String): Boolean;
begin
  Result := FRegisters.ContainsKey(AKey);
end;

function TRegister.RouteContainsKey(const AKey: String): Boolean;
begin
  Result := FRoutes.ContainsKey(AKey);
end;

procedure TRegister.UsePipes(const AValidationPipe: IValidationPipe);
begin
  FValidationPipe := AValidationPipe;
end;

function TRegister.Pipe: IValidationPipe;
begin
  Result := FValidationPipe;
end;

end.








