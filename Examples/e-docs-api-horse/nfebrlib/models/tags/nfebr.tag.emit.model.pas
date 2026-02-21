unit nfebr.tag.emit.model;

interface

uses
  nfebr.tag.enderemit.model;

type
  TEmitModel = class
  private
    FCNAE: String;
    FCNPJCPF: String;
    FCRT: integer;
    FEnderEmit: TEnderEmitModel;
    FIE: String;
    FIEST: String;
    FIM: String;
    FXFant: String;
    FXNome: String;
  public
    constructor Create;
    destructor Destroy; override;
    property CNAE: String read FCNAE write FCNAE;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property CRT: integer read FCRT write FCRT;
    property EnderEmit: TEnderEmitModel read FEnderEmit;
    property IE: String read FIE write FIE;
    property IEST: String read FIEST write FIEST;
    property IM: String read FIM write FIM;
    property XFant: String read FXFant write FXFant;
    property XNome: String read FXNome write FXNome;
  end;

implementation

{ TEmitModel }

constructor TEmitModel.Create;
begin
  FEnderEmit := TEnderEmitModel.Create;
end;

destructor TEmitModel.Destroy;
begin
  FEnderEmit.Free;
  inherited;
end;

end.

