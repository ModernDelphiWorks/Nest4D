unit nfebr.tag.dest.model;

interface

uses
  nfebr.tag.enderdest.model;

type
  TDestModel = class
  private
    FCNPJCPF: String;
    FEmail: String;
    FEnderDest: TEnderDestModel;
    FIE: String;
    FIM: String;
    FISUF: String;
    FIdEstrangeiro: String;
    FIndIEDest: integer;
    FXNome: String;
  public
    constructor Create;
    destructor Destroy; override;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property Email: String read FEmail write FEmail;
    property EnderDest: TEnderDestModel read FEnderDest;
    property IE: String read FIE write FIE;
    property IM: String read FIM write FIM;
    property ISUF: String read FISUF write FISUF;
    property IdEstrangeiro: String read FIdEstrangeiro write FIdEstrangeiro;
    property IndIEDest: integer read FIndIEDest write FIndIEDest;
    property XNome: String read FXNome write FXNome;
  end;

implementation

{ TDestModel }

constructor TDestModel.Create;
begin
  FEnderDest := TEnderDestModel.Create;
end;

destructor TDestModel.Destroy;
begin
  FEnderDest.Free;
  inherited;
end;

end.

