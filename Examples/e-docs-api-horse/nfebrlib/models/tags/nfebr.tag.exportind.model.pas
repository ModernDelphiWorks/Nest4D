unit nfebr.tag.exportind.model;

interface

type
  TExportIndModel = class
  private
    FChNFe: String;
    FNRE: String;
    FQExport: Double;
  public
    property ChNFe: String read FChNFe write FChNFe;
    property NRE: String read FNRE write FNRE;
    property QExport: Double read FQExport write FQExport;
  end;

implementation

end.

