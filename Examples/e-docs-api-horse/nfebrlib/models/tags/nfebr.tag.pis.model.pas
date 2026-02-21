unit nfebr.tag.pis.model;

interface

uses
  Generics.Collections,
  nfebr.tag.pisaliq.model,
  nfebr.tag.pisoutr.model,
  nfebr.tag.pisqtde.model;

type
  TPISModel = class
  private
    FCST: String;
    FPISAliq: TPISAliqModel;
    FPISOutr: TPISOutrModel;
    FPISQtde: TPISQtdeModel;
  public
    constructor Create;
    destructor Destroy; override;
    property CST: String read FCST write FCST;
    property PISAliq: TPISAliqModel read FPISAliq;
    property PISOutr: TPISOutrModel read FPISOutr;
    property PISQtde: TPISQtdeModel read FPISQtde;
  end;

implementation

{ TPISModel }

constructor TPISModel.Create;
begin
  FPISAliq := TPISAliqModel.Create;
  FPISOutr := TPISOutrModel.Create;
  FPISQtde := TPISQtdeModel.Create;
end;

destructor TPISModel.Destroy;
begin
  FPISAliq.Free;
  FPISOutr.Free;
  FPISQtde.Free;
  inherited;
end;

end.

