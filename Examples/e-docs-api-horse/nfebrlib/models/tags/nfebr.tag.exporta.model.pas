unit nfebr.tag.exporta.model;

interface

type
  TExportaModel = class
  private
    FUFSaidaPais: String;
    FXLocDespacho: String;
    FXLocExporta: String;
  public
    property UFSaidaPais: String read FUFSaidaPais write FUFSaidaPais;
    property XLocDespacho: String read FXLocDespacho write FXLocDespacho;
    property XLocExporta: String read FXLocExporta write FXLocExporta;
  end;

implementation

end.

