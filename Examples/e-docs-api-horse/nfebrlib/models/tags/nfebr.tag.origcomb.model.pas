unit nfebr.tag.origcomb.model;

interface

type
  TOrigCombModel = class
  private
    FCUFOrig: integer;
    FIndImport: integer;
    FPOrig: integer;
  public
    property CUFOrig: integer read FCUFOrig write FCUFOrig;
    property IndImport: integer read FIndImport write FIndImport;
    property POrig: integer read FPOrig write FPOrig;
  end;

implementation

end.

