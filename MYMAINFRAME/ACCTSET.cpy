      *   Micro Focus COBOL 2017  6.0.00246
      *   Micro Focus BMS Screen Painter (ver BMSIDE_6-0-02)
      *   MapSet Name   ACCTSET
      *   Date Created  07/06/2021
      *   Time Created  02:32:56

      *  Input Data For Map ACCTMNU
         01 ACCTMNUI.
            03 FILLER                         PIC X(12).
            03 SNAMEML                        PIC S9(4) COMP.
            03 SNAMEMF                        PIC X.
            03 FILLER REDEFINES SNAMEMF.
               05 SNAMEMA                        PIC X.
            03 FILLER                         PIC X(4).
            03 SNAMEMI                        PIC X(12).
            03 FNAMEML                        PIC S9(4) COMP.
            03 FNAMEMF                        PIC X.
            03 FILLER REDEFINES FNAMEMF.
               05 FNAMEMA                        PIC X.
            03 FILLER                         PIC X(4).
            03 FNAMEMI                        PIC X(7).
            03 REQML                          PIC S9(4) COMP.
            03 REQMF                          PIC X.
            03 FILLER REDEFINES REQMF.
               05 REQMA                          PIC X.
            03 FILLER                         PIC X(4).
            03 REQMI                          PIC X(1).
            03 ACCTML                         PIC S9(4) COMP.
            03 ACCTMF                         PIC X.
            03 FILLER REDEFINES ACCTMF.
               05 ACCTMA                         PIC X.
            03 FILLER                         PIC X(4).
            03 ACCTMI                         PIC X(5).
            03 PRTRML                         PIC S9(4) COMP.
            03 PRTRMF                         PIC X.
            03 FILLER REDEFINES PRTRMF.
               05 PRTRMA                         PIC X.
            03 FILLER                         PIC X(4).
            03 PRTRMI                         PIC X(4).
            03 SUMTTLML                       PIC S9(4) COMP.
            03 SUMTTLMF                       PIC X.
            03 FILLER REDEFINES SUMTTLMF.
               05 SUMTTLMA                       PIC X.
            03 FILLER                         PIC X(4).
            03 SUMTTLMI                       PIC X(79).
            03 SUMLNMD                        OCCURS 6 TIMES.
               05 SUMLNML                        PIC S9(4) COMP.
               05 SUMLNMF                        PIC X.
               05 FILLER                         PIC X(4).
               05 SUMLNMI                        PIC X(79).
            03 MSGML                          PIC S9(4) COMP.
            03 MSGMF                          PIC X.
            03 FILLER REDEFINES MSGMF.
               05 MSGMA                          PIC X.
            03 FILLER                         PIC X(4).
            03 MSGMI                          PIC X(70).

      *  Output Data For Map ACCTMNU
         01 ACCTMNUO REDEFINES ACCTMNUI.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 SNAMEMC                        PIC X.
            03 SNAMEMP                        PIC X.
            03 SNAMEMH                        PIC X.
            03 SNAMEMV                        PIC X.
            03 SNAMEMO                        PIC X(12).
            03 FILLER                         PIC X(3).
            03 FNAMEMC                        PIC X.
            03 FNAMEMP                        PIC X.
            03 FNAMEMH                        PIC X.
            03 FNAMEMV                        PIC X.
            03 FNAMEMO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 REQMC                          PIC X.
            03 REQMP                          PIC X.
            03 REQMH                          PIC X.
            03 REQMV                          PIC X.
            03 REQMO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 ACCTMC                         PIC X.
            03 ACCTMP                         PIC X.
            03 ACCTMH                         PIC X.
            03 ACCTMV                         PIC X.
            03 ACCTMO                         PIC X(5).
            03 FILLER                         PIC X(3).
            03 PRTRMC                         PIC X.
            03 PRTRMP                         PIC X.
            03 PRTRMH                         PIC X.
            03 PRTRMV                         PIC X.
            03 PRTRMO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 SUMTTLMC                       PIC X.
            03 SUMTTLMP                       PIC X.
            03 SUMTTLMH                       PIC X.
            03 SUMTTLMV                       PIC X.
            03 SUMTTLMO                       PIC X(79).
            03 DFHMS1 OCCURS 6.
               05 FILLER                         PIC X(2).
               05 SUMLNMA                        PIC X.
               05 SUMLNMC                        PIC X.
               05 SUMLNMP                        PIC X.
               05 SUMLNMH                        PIC X.
               05 SUMLNMV                        PIC X.
               05 SUMLNMO                        PIC X(79).
            03 FILLER                         PIC X(3).
            03 MSGMC                          PIC X.
            03 MSGMP                          PIC X.
            03 MSGMH                          PIC X.
            03 MSGMV                          PIC X.
            03 MSGMO                          PIC X(70).

      *  Input Data For Map ACCTDTL
         01 ACCTDTLI.
            03 FILLER                         PIC X(12).
            03 TITLEDL                        PIC S9(4) COMP.
            03 TITLEDF                        PIC X.
            03 FILLER REDEFINES TITLEDF.
               05 TITLEDA                        PIC X.
            03 FILLER                         PIC X(4).
            03 TITLEDI                        PIC X(14).
            03 ACCTDL                         PIC S9(4) COMP.
            03 ACCTDF                         PIC X.
            03 FILLER REDEFINES ACCTDF.
               05 ACCTDA                         PIC X.
            03 FILLER                         PIC X(4).
            03 ACCTDI                         PIC X(5).
            03 SNAMEDL                        PIC S9(4) COMP.
            03 SNAMEDF                        PIC X.
            03 FILLER REDEFINES SNAMEDF.
               05 SNAMEDA                        PIC X.
            03 FILLER                         PIC X(4).
            03 SNAMEDI                        PIC X(18).
            03 FNAMEDL                        PIC S9(4) COMP.
            03 FNAMEDF                        PIC X.
            03 FILLER REDEFINES FNAMEDF.
               05 FNAMEDA                        PIC X.
            03 FILLER                         PIC X(4).
            03 FNAMEDI                        PIC X(12).
            03 MIDL                           PIC S9(4) COMP.
            03 MIDF                           PIC X.
            03 FILLER REDEFINES MIDF.
               05 MIDA                           PIC X.
            03 FILLER                         PIC X(4).
            03 MIDI                           PIC X(1).
            03 TTLDL                          PIC S9(4) COMP.
            03 TTLDF                          PIC X.
            03 FILLER REDEFINES TTLDF.
               05 TTLDA                          PIC X.
            03 FILLER                         PIC X(4).
            03 TTLDI                          PIC X(4).
            03 TELDL                          PIC S9(4) COMP.
            03 TELDF                          PIC X.
            03 FILLER REDEFINES TELDF.
               05 TELDA                          PIC X.
            03 FILLER                         PIC X(4).
            03 TELDI                          PIC X(10).
            03 ADDR1DL                        PIC S9(4) COMP.
            03 ADDR1DF                        PIC X.
            03 FILLER REDEFINES ADDR1DF.
               05 ADDR1DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 ADDR1DI                        PIC X(24).
            03 ADDR2DL                        PIC S9(4) COMP.
            03 ADDR2DF                        PIC X.
            03 FILLER REDEFINES ADDR2DF.
               05 ADDR2DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 ADDR2DI                        PIC X(24).
            03 ADDR3DL                        PIC S9(4) COMP.
            03 ADDR3DF                        PIC X.
            03 FILLER REDEFINES ADDR3DF.
               05 ADDR3DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 ADDR3DI                        PIC X(24).
            03 AUTH1DL                        PIC S9(4) COMP.
            03 AUTH1DF                        PIC X.
            03 FILLER REDEFINES AUTH1DF.
               05 AUTH1DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 AUTH1DI                        PIC X(32).
            03 AUTH2DL                        PIC S9(4) COMP.
            03 AUTH2DF                        PIC X.
            03 FILLER REDEFINES AUTH2DF.
               05 AUTH2DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 AUTH2DI                        PIC X(32).
            03 AUTH3DL                        PIC S9(4) COMP.
            03 AUTH3DF                        PIC X.
            03 FILLER REDEFINES AUTH3DF.
               05 AUTH3DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 AUTH3DI                        PIC X(32).
            03 AUTH4DL                        PIC S9(4) COMP.
            03 AUTH4DF                        PIC X.
            03 FILLER REDEFINES AUTH4DF.
               05 AUTH4DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 AUTH4DI                        PIC X(32).
            03 CARDSDL                        PIC S9(4) COMP.
            03 CARDSDF                        PIC X.
            03 FILLER REDEFINES CARDSDF.
               05 CARDSDA                        PIC X.
            03 FILLER                         PIC X(4).
            03 CARDSDI                        PIC X(1).
            03 IMODL                          PIC S9(4) COMP.
            03 IMODF                          PIC X.
            03 FILLER REDEFINES IMODF.
               05 IMODA                          PIC X.
            03 FILLER                         PIC X(4).
            03 IMODI                          PIC X(2).
            03 IDAYDL                         PIC S9(4) COMP.
            03 IDAYDF                         PIC X.
            03 FILLER REDEFINES IDAYDF.
               05 IDAYDA                         PIC X.
            03 FILLER                         PIC X(4).
            03 IDAYDI                         PIC X(2).
            03 IYRDL                          PIC S9(4) COMP.
            03 IYRDF                          PIC X.
            03 FILLER REDEFINES IYRDF.
               05 IYRDA                          PIC X.
            03 FILLER                         PIC X(4).
            03 IYRDI                          PIC X(2).
            03 RSNDL                          PIC S9(4) COMP.
            03 RSNDF                          PIC X.
            03 FILLER REDEFINES RSNDF.
               05 RSNDA                          PIC X.
            03 FILLER                         PIC X(4).
            03 RSNDI                          PIC X(1).
            03 CCODEDL                        PIC S9(4) COMP.
            03 CCODEDF                        PIC X.
            03 FILLER REDEFINES CCODEDF.
               05 CCODEDA                        PIC X.
            03 FILLER                         PIC X(4).
            03 CCODEDI                        PIC X(1).
            03 APPRDL                         PIC S9(4) COMP.
            03 APPRDF                         PIC X.
            03 FILLER REDEFINES APPRDF.
               05 APPRDA                         PIC X.
            03 FILLER                         PIC X(4).
            03 APPRDI                         PIC X(3).
            03 SCODE1DL                       PIC S9(4) COMP.
            03 SCODE1DF                       PIC X.
            03 FILLER REDEFINES SCODE1DF.
               05 SCODE1DA                       PIC X.
            03 FILLER                         PIC X(4).
            03 SCODE1DI                       PIC X(1).
            03 SCODE2DL                       PIC S9(4) COMP.
            03 SCODE2DF                       PIC X.
            03 FILLER REDEFINES SCODE2DF.
               05 SCODE2DA                       PIC X.
            03 FILLER                         PIC X(4).
            03 SCODE2DI                       PIC X(1).
            03 SCODE3DL                       PIC S9(4) COMP.
            03 SCODE3DF                       PIC X.
            03 FILLER REDEFINES SCODE3DF.
               05 SCODE3DA                       PIC X.
            03 FILLER                         PIC X(4).
            03 SCODE3DI                       PIC X(1).
            03 STATTLDL                       PIC S9(4) COMP.
            03 STATTLDF                       PIC X.
            03 FILLER REDEFINES STATTLDF.
               05 STATTLDA                       PIC X.
            03 FILLER                         PIC X(4).
            03 STATTLDI                       PIC X(15).
            03 STATDL                         PIC S9(4) COMP.
            03 STATDF                         PIC X.
            03 FILLER REDEFINES STATDF.
               05 STATDA                         PIC X.
            03 FILLER                         PIC X(4).
            03 STATDI                         PIC X(2).
            03 LIMTTLDL                       PIC S9(4) COMP.
            03 LIMTTLDF                       PIC X.
            03 FILLER REDEFINES LIMTTLDF.
               05 LIMTTLDA                       PIC X.
            03 FILLER                         PIC X(4).
            03 LIMTTLDI                       PIC X(18).
            03 LIMITDL                        PIC S9(4) COMP.
            03 LIMITDF                        PIC X.
            03 FILLER REDEFINES LIMITDF.
               05 LIMITDA                        PIC X.
            03 FILLER                         PIC X(4).
            03 LIMITDI                        PIC X(8).
            03 HISTTLDL                       PIC S9(4) COMP.
            03 HISTTLDF                       PIC X.
            03 FILLER REDEFINES HISTTLDF.
               05 HISTTLDA                       PIC X.
            03 FILLER                         PIC X(4).
            03 HISTTLDI                       PIC X(71).
            03 HIST1DL                        PIC S9(4) COMP.
            03 HIST1DF                        PIC X.
            03 FILLER REDEFINES HIST1DF.
               05 HIST1DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 HIST1DI                        PIC X(61).
            03 HIST2DL                        PIC S9(4) COMP.
            03 HIST2DF                        PIC X.
            03 FILLER REDEFINES HIST2DF.
               05 HIST2DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 HIST2DI                        PIC X(61).
            03 HIST3DL                        PIC S9(4) COMP.
            03 HIST3DF                        PIC X.
            03 FILLER REDEFINES HIST3DF.
               05 HIST3DA                        PIC X.
            03 FILLER                         PIC X(4).
            03 HIST3DI                        PIC X(61).
            03 MSGDL                          PIC S9(4) COMP.
            03 MSGDF                          PIC X.
            03 FILLER REDEFINES MSGDF.
               05 MSGDA                          PIC X.
            03 FILLER                         PIC X(4).
            03 MSGDI                          PIC X(70).
            03 VFYDL                          PIC S9(4) COMP.
            03 VFYDF                          PIC X.
            03 FILLER REDEFINES VFYDF.
               05 VFYDA                          PIC X.
            03 FILLER                         PIC X(4).
            03 VFYDI                          PIC X(1).

      *  Output Data For Map ACCTDTL
         01 ACCTDTLO REDEFINES ACCTDTLI.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TITLEDC                        PIC X.
            03 TITLEDP                        PIC X.
            03 TITLEDH                        PIC X.
            03 TITLEDV                        PIC X.
            03 TITLEDO                        PIC X(14).
            03 FILLER                         PIC X(3).
            03 ACCTDC                         PIC X.
            03 ACCTDP                         PIC X.
            03 ACCTDH                         PIC X.
            03 ACCTDV                         PIC X.
            03 ACCTDO                         PIC X(5).
            03 FILLER                         PIC X(3).
            03 SNAMEDC                        PIC X.
            03 SNAMEDP                        PIC X.
            03 SNAMEDH                        PIC X.
            03 SNAMEDV                        PIC X.
            03 SNAMEDO                        PIC X(18).
            03 FILLER                         PIC X(3).
            03 FNAMEDC                        PIC X.
            03 FNAMEDP                        PIC X.
            03 FNAMEDH                        PIC X.
            03 FNAMEDV                        PIC X.
            03 FNAMEDO                        PIC X(12).
            03 FILLER                         PIC X(3).
            03 MIDC                           PIC X.
            03 MIDP                           PIC X.
            03 MIDH                           PIC X.
            03 MIDV                           PIC X.
            03 MIDO                           PIC X(1).
            03 FILLER                         PIC X(3).
            03 TTLDC                          PIC X.
            03 TTLDP                          PIC X.
            03 TTLDH                          PIC X.
            03 TTLDV                          PIC X.
            03 TTLDO                          PIC X(4).
            03 FILLER                         PIC X(3).
            03 TELDC                          PIC X.
            03 TELDP                          PIC X.
            03 TELDH                          PIC X.
            03 TELDV                          PIC X.
            03 TELDO                          PIC X(10).
            03 FILLER                         PIC X(3).
            03 ADDR1DC                        PIC X.
            03 ADDR1DP                        PIC X.
            03 ADDR1DH                        PIC X.
            03 ADDR1DV                        PIC X.
            03 ADDR1DO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 ADDR2DC                        PIC X.
            03 ADDR2DP                        PIC X.
            03 ADDR2DH                        PIC X.
            03 ADDR2DV                        PIC X.
            03 ADDR2DO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 ADDR3DC                        PIC X.
            03 ADDR3DP                        PIC X.
            03 ADDR3DH                        PIC X.
            03 ADDR3DV                        PIC X.
            03 ADDR3DO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 AUTH1DC                        PIC X.
            03 AUTH1DP                        PIC X.
            03 AUTH1DH                        PIC X.
            03 AUTH1DV                        PIC X.
            03 AUTH1DO                        PIC X(32).
            03 FILLER                         PIC X(3).
            03 AUTH2DC                        PIC X.
            03 AUTH2DP                        PIC X.
            03 AUTH2DH                        PIC X.
            03 AUTH2DV                        PIC X.
            03 AUTH2DO                        PIC X(32).
            03 FILLER                         PIC X(3).
            03 AUTH3DC                        PIC X.
            03 AUTH3DP                        PIC X.
            03 AUTH3DH                        PIC X.
            03 AUTH3DV                        PIC X.
            03 AUTH3DO                        PIC X(32).
            03 FILLER                         PIC X(3).
            03 AUTH4DC                        PIC X.
            03 AUTH4DP                        PIC X.
            03 AUTH4DH                        PIC X.
            03 AUTH4DV                        PIC X.
            03 AUTH4DO                        PIC X(32).
            03 FILLER                         PIC X(3).
            03 CARDSDC                        PIC X.
            03 CARDSDP                        PIC X.
            03 CARDSDH                        PIC X.
            03 CARDSDV                        PIC X.
            03 CARDSDO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 IMODC                          PIC X.
            03 IMODP                          PIC X.
            03 IMODH                          PIC X.
            03 IMODV                          PIC X.
            03 IMODO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 IDAYDC                         PIC X.
            03 IDAYDP                         PIC X.
            03 IDAYDH                         PIC X.
            03 IDAYDV                         PIC X.
            03 IDAYDO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 IYRDC                          PIC X.
            03 IYRDP                          PIC X.
            03 IYRDH                          PIC X.
            03 IYRDV                          PIC X.
            03 IYRDO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 RSNDC                          PIC X.
            03 RSNDP                          PIC X.
            03 RSNDH                          PIC X.
            03 RSNDV                          PIC X.
            03 RSNDO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 CCODEDC                        PIC X.
            03 CCODEDP                        PIC X.
            03 CCODEDH                        PIC X.
            03 CCODEDV                        PIC X.
            03 CCODEDO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 APPRDC                         PIC X.
            03 APPRDP                         PIC X.
            03 APPRDH                         PIC X.
            03 APPRDV                         PIC X.
            03 APPRDO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 SCODE1DC                       PIC X.
            03 SCODE1DP                       PIC X.
            03 SCODE1DH                       PIC X.
            03 SCODE1DV                       PIC X.
            03 SCODE1DO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 SCODE2DC                       PIC X.
            03 SCODE2DP                       PIC X.
            03 SCODE2DH                       PIC X.
            03 SCODE2DV                       PIC X.
            03 SCODE2DO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 SCODE3DC                       PIC X.
            03 SCODE3DP                       PIC X.
            03 SCODE3DH                       PIC X.
            03 SCODE3DV                       PIC X.
            03 SCODE3DO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 STATTLDC                       PIC X.
            03 STATTLDP                       PIC X.
            03 STATTLDH                       PIC X.
            03 STATTLDV                       PIC X.
            03 STATTLDO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 STATDC                         PIC X.
            03 STATDP                         PIC X.
            03 STATDH                         PIC X.
            03 STATDV                         PIC X.
            03 STATDO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 LIMTTLDC                       PIC X.
            03 LIMTTLDP                       PIC X.
            03 LIMTTLDH                       PIC X.
            03 LIMTTLDV                       PIC X.
            03 LIMTTLDO                       PIC X(18).
            03 FILLER                         PIC X(3).
            03 LIMITDC                        PIC X.
            03 LIMITDP                        PIC X.
            03 LIMITDH                        PIC X.
            03 LIMITDV                        PIC X.
            03 LIMITDO                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 HISTTLDC                       PIC X.
            03 HISTTLDP                       PIC X.
            03 HISTTLDH                       PIC X.
            03 HISTTLDV                       PIC X.
            03 HISTTLDO                       PIC X(71).
            03 FILLER                         PIC X(3).
            03 HIST1DC                        PIC X.
            03 HIST1DP                        PIC X.
            03 HIST1DH                        PIC X.
            03 HIST1DV                        PIC X.
            03 HIST1DO                        PIC X(61).
            03 FILLER                         PIC X(3).
            03 HIST2DC                        PIC X.
            03 HIST2DP                        PIC X.
            03 HIST2DH                        PIC X.
            03 HIST2DV                        PIC X.
            03 HIST2DO                        PIC X(61).
            03 FILLER                         PIC X(3).
            03 HIST3DC                        PIC X.
            03 HIST3DP                        PIC X.
            03 HIST3DH                        PIC X.
            03 HIST3DV                        PIC X.
            03 HIST3DO                        PIC X(61).
            03 FILLER                         PIC X(3).
            03 MSGDC                          PIC X.
            03 MSGDP                          PIC X.
            03 MSGDH                          PIC X.
            03 MSGDV                          PIC X.
            03 MSGDO                          PIC X(70).
            03 FILLER                         PIC X(3).
            03 VFYDC                          PIC X.
            03 VFYDP                          PIC X.
            03 VFYDH                          PIC X.
            03 VFYDV                          PIC X.
            03 VFYDO                          PIC X(1).

      *  Input Data For Map ACCTERR
         01 ACCTERRI.
            03 FILLER                         PIC X(12).
            03 TRANEL                         PIC S9(4) COMP.
            03 TRANEF                         PIC X.
            03 FILLER REDEFINES TRANEF.
               05 TRANEA                         PIC X.
            03 FILLER                         PIC X(4).
            03 TRANEI                         PIC X(4).
            03 PGMEL                          PIC S9(4) COMP.
            03 PGMEF                          PIC X.
            03 FILLER REDEFINES PGMEF.
               05 PGMEA                          PIC X.
            03 FILLER                         PIC X(4).
            03 PGMEI                          PIC X(8).
            03 RSNEL                          PIC S9(4) COMP.
            03 RSNEF                          PIC X.
            03 FILLER REDEFINES RSNEF.
               05 RSNEA                          PIC X.
            03 FILLER                         PIC X(4).
            03 RSNEI                          PIC X(60).
            03 FILEEL                         PIC S9(4) COMP.
            03 FILEEF                         PIC X.
            03 FILLER REDEFINES FILEEF.
               05 FILEEA                         PIC X.
            03 FILLER                         PIC X(4).
            03 FILEEI                         PIC X(22).

      *  Output Data For Map ACCTERR
         01 ACCTERRO REDEFINES ACCTERRI.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TRANEC                         PIC X.
            03 TRANEP                         PIC X.
            03 TRANEH                         PIC X.
            03 TRANEV                         PIC X.
            03 TRANEO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 PGMEC                          PIC X.
            03 PGMEP                          PIC X.
            03 PGMEH                          PIC X.
            03 PGMEV                          PIC X.
            03 PGMEO                          PIC X(8).
            03 FILLER                         PIC X(3).
            03 RSNEC                          PIC X.
            03 RSNEP                          PIC X.
            03 RSNEH                          PIC X.
            03 RSNEV                          PIC X.
            03 RSNEO                          PIC X(60).
            03 FILLER                         PIC X(3).
            03 FILEEC                         PIC X.
            03 FILEEP                         PIC X.
            03 FILEEH                         PIC X.
            03 FILEEV                         PIC X.
            03 FILEEO                         PIC X(22).

      *  Input Data For Map ACCTMSG
         01 ACCTMSGI.
            03 FILLER                         PIC X(12).
            03 MSGL                           PIC S9(4) COMP.
            03 MSGF                           PIC X.
            03 FILLER REDEFINES MSGF.
               05 MSGA                           PIC X.
            03 FILLER                         PIC X(4).
            03 MSGI                           PIC X(79).

      *  Output Data For Map ACCTMSG
         01 ACCTMSGO REDEFINES ACCTMSGI.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 MSGC                           PIC X.
            03 MSGP                           PIC X.
            03 MSGH                           PIC X.
            03 MSGV                           PIC X.
            03 MSGO                           PIC X(79).

      *  Input Data For Map CONFIRM
         01 CONFIRMI.
            03 FILLER                         PIC X(12).

      *  Output Data For Map CONFIRM
         01 CONFIRMO REDEFINES CONFIRMI.
            03 FILLER                         PIC X(12).

      *  Input Data For Map ZCCTMN2
         01 ZCCTMN2I.
            03 FILLER                         PIC X(12).
            03 MNUNAMEL                       PIC S9(4) COMP.
            03 MNUNAMEF                       PIC X.
            03 FILLER REDEFINES MNUNAMEF.
               05 MNUNAMEA                       PIC X.
            03 FILLER                         PIC X(4).
            03 MNUNAMEI                       PIC X(60).

      *  Output Data For Map ZCCTMN2
         01 ZCCTMN2O REDEFINES ZCCTMN2I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 MNUNAMEC                       PIC X.
            03 MNUNAMEP                       PIC X.
            03 MNUNAMEH                       PIC X.
            03 MNUNAMEV                       PIC X.
            03 MNUNAMEO                       PIC X(60).

