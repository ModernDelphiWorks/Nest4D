unit Nest4D.Interceptor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;

type
  // Tipos básicos para interceptadores
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
  
  // Interface básica para interceptadores síncronos (se necessário)
  INest4DInterceptor = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetName: String;
    function GetPriority: Integer;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;
  
  // Classe base para interceptadores síncronos (se necessário)
  TNest4DInterceptorBase = class(TInterfacedObject, INest4DInterceptor)
  private
    FName: String;
    FPriority: Integer;
    FEnabled: Boolean;
    
  public
    constructor Create(const AName: String; APriority: Integer = 100);
    
    function GetName: String;
    function GetPriority: Integer;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;

implementation

{ TNest4DInterceptorBase }

constructor TNest4DInterceptorBase.Create(const AName: String; APriority: Integer);
begin
  inherited Create;
  FName := AName;
  FPriority := APriority;
  FEnabled := True;
end;

function TNest4DInterceptorBase.GetName: String;
begin
  Result := FName;
end;

function TNest4DInterceptorBase.GetPriority: Integer;
begin
  Result := FPriority;
end;

function TNest4DInterceptorBase.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TNest4DInterceptorBase.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

end.