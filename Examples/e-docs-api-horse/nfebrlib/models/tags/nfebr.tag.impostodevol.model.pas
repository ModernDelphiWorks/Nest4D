unit nfebr.tag.impostodevol.model;

interface

uses
  nfebr.tag.ipi.model;

type
  TImpostoDevolModel = class
  private
    FIPI: TIPIModel;
    FPDevol: Double;
  public
    constructor Create;
    destructor Destroy; override;
    property IPI: TIPIModel read FIPI;
    property PDevol: Double read FPDevol write FPDevol;
  end;

implementation

constructor TImpostoDevolModel.Create;
begin
  FIPI := TIPIModel.Create;
end;

destructor TImpostoDevolModel.Destroy;
begin
  FIPI.Free;
  inherited;
end;

end.
