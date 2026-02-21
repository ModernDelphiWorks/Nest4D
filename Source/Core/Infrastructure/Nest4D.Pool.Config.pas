{
                 Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit Nest4D.Pool.Config;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.SysUtils,
  System.DateUtils,
  Nest4D.Module.Abstract;

type
  // Record para armazenar informações do módulo no pool
  TPooledModule = record
    Module: TModuleAbstract;
    LastUsed: TDateTime;
    InUse: Boolean;
  end;

  TPoolConfig = class
  private
    FMinSize: Integer;
    FMaxSize: Integer;
    FEnabled: Boolean;
    FIdleTimeout: Integer;
    FGrowthFactor: Double;
    
    // Estrutura interna para o pool
    FModulePool: TDictionary<String, TList<TPooledModule>>;
    FCriticalSection: TCriticalSection;
    
    procedure CleanupIdleModules;
    procedure InitializePool;
    procedure FinalizePool;
    function CreateModuleKey(const AClassName: String): String;
    function GetPoolForClass(const AClassName: String): TList<TPooledModule>;
  public
    constructor Create;
    destructor Destroy; override;

    function SetMinSize(const ASize: Integer): TPoolConfig;
    function SetMaxSize(const ASize: Integer): TPoolConfig;
    function SetEnabled(const AEnabled: Boolean): TPoolConfig;
    function SetIdleTimeout(const ATimeout: Integer): TPoolConfig;
    function SetGrowthFactor(const AFactor: Double): TPoolConfig;

    function Clone: TPoolConfig;
    
    // Métodos do pool de módulos
    function GetModule(const AClassName: String): TModuleAbstract;
    procedure ReturnModule(const AClassName: String; const AModule: TModuleAbstract);
    procedure AddModule(const AClassName: String; const AModule: TModuleAbstract);
    function GetPoolSize(const AClassName: String): Integer;
    function GetActiveModules(const AClassName: String): Integer;
    procedure ClearPool(const AClassName: String = '');

    property MinSize: Integer read FMinSize;
    property MaxSize: Integer read FMaxSize;
    property Enabled: Boolean read FEnabled;
    property IdleTimeout: Integer read FIdleTimeout;
    property GrowthFactor: Double read FGrowthFactor;
  end;

implementation

{ TPoolConfig }

constructor TPoolConfig.Create;
begin
  inherited;
  FMinSize := 1;
  FMaxSize := 5;
  FEnabled := True;
  FIdleTimeout := 300000; // 5 minutos
  FGrowthFactor := 1.5;
  
  InitializePool;
end;

destructor TPoolConfig.Destroy;
begin
  FinalizePool;
  inherited;
end;

procedure TPoolConfig.InitializePool;
begin
  FModulePool := TDictionary<String, TList<TPooledModule>>.Create;
  FCriticalSection := TCriticalSection.Create;
end;

procedure TPoolConfig.FinalizePool;
var
  LPair: TPair<String, TList<TPooledModule>>;
  LPooledModule: TPooledModule;
begin
  if Assigned(FModulePool) then
  begin
    FCriticalSection.Enter;
    try
      // Liberar todos os módulos do pool
      for LPair in FModulePool do
      begin
        for LPooledModule in LPair.Value do
        begin
          if Assigned(LPooledModule.Module) then
            LPooledModule.Module.Free;
        end;
        LPair.Value.Free;
      end;
      FModulePool.Free;
    finally
      FCriticalSection.Leave;
    end;
  end;
  
  if Assigned(FCriticalSection) then
    FCriticalSection.Free;
end;

function TPoolConfig.SetMinSize(const ASize: Integer): TPoolConfig;
begin
  if ASize > 0 then
    FMinSize := ASize;
  Result := Self;
end;

function TPoolConfig.SetMaxSize(const ASize: Integer): TPoolConfig;
begin
  if ASize >= FMinSize then
    FMaxSize := ASize;
  Result := Self;
end;

function TPoolConfig.SetEnabled(const AEnabled: Boolean): TPoolConfig;
begin
  FEnabled := AEnabled;
  Result := Self;
end;

function TPoolConfig.SetIdleTimeout(const ATimeout: Integer): TPoolConfig;
begin
  if ATimeout > 0 then
    FIdleTimeout := ATimeout;
  Result := Self;
end;

function TPoolConfig.SetGrowthFactor(const AFactor: Double): TPoolConfig;
begin
  if AFactor > 1.0 then
    FGrowthFactor := AFactor;
  Result := Self;
end;

function TPoolConfig.Clone: TPoolConfig;
begin
  Result := TPoolConfig.Create
    .SetMinSize(FMinSize)
    .SetMaxSize(FMaxSize)
    .SetEnabled(FEnabled)
    .SetIdleTimeout(FIdleTimeout)
    .SetGrowthFactor(FGrowthFactor);
end;

function TPoolConfig.CreateModuleKey(const AClassName: String): String;
begin
  Result := UpperCase(AClassName);
end;

function TPoolConfig.GetPoolForClass(const AClassName: String): TList<TPooledModule>;
var
  LKey: String;
begin
  LKey := CreateModuleKey(AClassName);
  
  if not FModulePool.TryGetValue(LKey, Result) then
  begin
    Result := TList<TPooledModule>.Create;
    FModulePool.Add(LKey, Result);
  end;
end;

function TPoolConfig.GetModule(const AClassName: String): TModuleAbstract;
var
  LPool: TList<TPooledModule>;
  LPooledModule: TPooledModule;
  LFor: Integer;
begin
  Result := nil;
  
  if not FEnabled then
    Exit;
    
  FCriticalSection.Enter;
  try
    LPool := GetPoolForClass(AClassName);
    
    // Procurar por um módulo disponível
    for LFor := 0 to LPool.Count - 1 do
    begin
      LPooledModule := LPool[LFor];
      if not LPooledModule.InUse then
      begin
        LPooledModule.InUse := True;
        LPooledModule.LastUsed := Now;
        LPool[LFor] := LPooledModule;
        Result := LPooledModule.Module;
        Break;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPoolConfig.ReturnModule(const AClassName: String; const AModule: TModuleAbstract);
var
  LPool: TList<TPooledModule>;
  LPooledModule: TPooledModule;
  LFor: Integer;
begin
  if not FEnabled or not Assigned(AModule) then
    Exit;
    
  FCriticalSection.Enter;
  try
    LPool := GetPoolForClass(AClassName);
    
    // Encontrar o módulo no pool e marcá-lo como disponível
    for LFor := 0 to LPool.Count - 1 do
    begin
      LPooledModule := LPool[LFor];
      if LPooledModule.Module = AModule then
      begin
        LPooledModule.InUse := False;
        LPooledModule.LastUsed := Now;
        LPool[LFor] := LPooledModule;
        Break;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPoolConfig.AddModule(const AClassName: String; const AModule: TModuleAbstract);
var
  LPool: TList<TPooledModule>;
  LPooledModule: TPooledModule;
begin
  if not FEnabled or not Assigned(AModule) then
    Exit;
    
  FCriticalSection.Enter;
  try
    LPool := GetPoolForClass(AClassName);
    
    // Verificar se não excede o tamanho máximo
    if LPool.Count >= FMaxSize then
    begin
      // Se exceder, liberar o módulo
      AModule.Free;
      Exit;
    end;
    
    // Adicionar módulo ao pool
    LPooledModule.Module := AModule;
    LPooledModule.LastUsed := Now;
    LPooledModule.InUse := False;
    LPool.Add(LPooledModule);
  finally
    FCriticalSection.Leave;
  end;
end;

function TPoolConfig.GetPoolSize(const AClassName: String): Integer;
var
  LPool: TList<TPooledModule>;
begin
  Result := 0;
  
  FCriticalSection.Enter;
  try
    if FModulePool.TryGetValue(CreateModuleKey(AClassName), LPool) then
      Result := LPool.Count;
  finally
    FCriticalSection.Leave;
  end;
end;

function TPoolConfig.GetActiveModules(const AClassName: String): Integer;
var
  LPool: TList<TPooledModule>;
  LPooledModule: TPooledModule;
begin
  Result := 0;
  
  FCriticalSection.Enter;
  try
    if FModulePool.TryGetValue(CreateModuleKey(AClassName), LPool) then
    begin
      for LPooledModule in LPool do
      begin
        if LPooledModule.InUse then
          Inc(Result);
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPoolConfig.ClearPool(const AClassName: String);
var
  LPair: TPair<String, TList<TPooledModule>>;
  LPool: TList<TPooledModule>;
  LPooledModule: TPooledModule;
begin
  FCriticalSection.Enter;
  try
    if AClassName = '' then
    begin
      // Limpar todos os pools
      for LPair in FModulePool do
      begin
        for LPooledModule in LPair.Value do
        begin
          if Assigned(LPooledModule.Module) and not LPooledModule.InUse then
            LPooledModule.Module.Free;
        end;
        LPair.Value.Clear;
      end;
    end
    else
    begin
      // Limpar pool específico
      if FModulePool.TryGetValue(CreateModuleKey(AClassName), LPool) then
      begin
        for LPooledModule in LPool do
        begin
          if Assigned(LPooledModule.Module) and not LPooledModule.InUse then
            LPooledModule.Module.Free;
        end;
        LPool.Clear;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPoolConfig.CleanupIdleModules;
var
  LPair: TPair<String, TList<TPooledModule>>;
  LPool: TList<TPooledModule>;
  LPooledModule: TPooledModule;
  LIdleTime: TDateTime;
  LFor: Integer;
begin
  if not FEnabled then
    Exit;
    
  LIdleTime := IncMilliSecond(Now, -FIdleTimeout);
  
  FCriticalSection.Enter;
  try
    for LPair in FModulePool do
    begin
      LPool := LPair.Value;
      
      // Remover módulos ociosos (mantendo pelo menos MinSize)
      for LFor := LPool.Count - 1 downto 0 do
      begin
        if LPool.Count <= FMinSize then
          Break;
          
        LPooledModule := LPool[LFor];
        if not LPooledModule.InUse and (LPooledModule.LastUsed < LIdleTime) then
        begin
          if Assigned(LPooledModule.Module) then
            LPooledModule.Module.Free;
          LPool.Delete(LFor);
        end;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

end.