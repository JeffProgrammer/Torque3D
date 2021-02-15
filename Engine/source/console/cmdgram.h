typedef union {
   Token< char >           c;
   Token< int >            i;
   Token< const char* >    s;
   Token< char* >          str;
   Token< double >         f;
   StringTableEntry        ste;
   StmtNode*               stmt;
   ExprNode*               expr;
   SlotAssignNode*         slist;
   VarNode*                var;
   SlotDecl                slot;
   InternalSlotDecl        intslot;
   ObjectBlockDecl         odcl;
   ObjectDeclNode*         od;
   AssignDecl              asn;
   IfStmtNode*             ifnode;
   ParamNode*              param;
   AssignExprNode*         aen;
} YYSTYPE;
#define	rwDEFINE	258
#define	rwENDDEF	259
#define	rwDECLARE	260
#define	rwDECLARESINGLETON	261
#define	rwBREAK	262
#define	rwELSE	263
#define	rwCONTINUE	264
#define	rwGLOBAL	265
#define	rwIF	266
#define	rwNIL	267
#define	rwRETURN	268
#define	rwWHILE	269
#define	rwDO	270
#define	rwENDIF	271
#define	rwENDWHILE	272
#define	rwENDFOR	273
#define	rwDEFAULT	274
#define	rwFOR	275
#define	rwFOREACH	276
#define	rwFOREACHSTR	277
#define	rwIN	278
#define	rwDATABLOCK	279
#define	rwSWITCH	280
#define	rwCASE	281
#define	rwSWITCHSTR	282
#define	rwCASEOR	283
#define	rwPACKAGE	284
#define	rwNAMESPACE	285
#define	rwCLASS	286
#define	rwASSERT	287
#define	rwINT	288
#define	rwFLOAT	289
#define	rwSTRING	290
#define	rwBOOL	291
#define	rwVOID	292
#define	ILLEGAL_TOKEN	293
#define	CHRCONST	294
#define	INTCONST	295
#define	TTAG	296
#define	VAR	297
#define	IDENT	298
#define	TYPEIDENT	299
#define	DOCBLOCK	300
#define	STRATOM	301
#define	TAGATOM	302
#define	FLTCONST	303
#define	opINTNAME	304
#define	opINTNAMER	305
#define	opMINUSMINUS	306
#define	opPLUSPLUS	307
#define	STMT_SEP	308
#define	opSHL	309
#define	opSHR	310
#define	opPLASN	311
#define	opMIASN	312
#define	opMLASN	313
#define	opDVASN	314
#define	opMODASN	315
#define	opANDASN	316
#define	opXORASN	317
#define	opORASN	318
#define	opSLASN	319
#define	opSRASN	320
#define	opCAT	321
#define	opEQ	322
#define	opNE	323
#define	opGE	324
#define	opLE	325
#define	opAND	326
#define	opOR	327
#define	opSTREQ	328
#define	opCOLONCOLON	329
#define	opMDASN	330
#define	opNDASN	331
#define	opNTASN	332
#define	opSTRNE	333
#define	UNARY	334


extern YYSTYPE CMDlval;
