unit nfebr.tag.detexport.model;

interface

uses
  nfebr.tag.exportind.model;

type
  TDetExportModel = class
  private
    FExportInd: TExportIndModel;
    FNDraw: String;
  public
    constructor Create;
    destructor Destroy; override;
    property ExportInd: TExportIndModel read FExportInd;
    property NDraw: String read FNDraw write FNDraw;
  end;

implementation

{ TDetExportModel }

constructor TDetExportModel.Create;
begin
  FExportInd := TExportIndModel.Create;
end;

destructor TDetExportModel.Destroy;
begin
  FExportInd.Free;
  inherited;
end;

end.

