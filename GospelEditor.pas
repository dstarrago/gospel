unit GospelEditor;

interface

  uses mwCustomEdit;

  type
    TCodeEdit =
      class(TmwCustomEdit)
        private
          FFiled: boolean;
          FFileName: string;
        public
          property FileName: string read FFileName write FFileName;
          property Filed: boolean read FFiled write FFiled;
      end;

implementation


end.
