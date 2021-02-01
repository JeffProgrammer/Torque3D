typedef union {
   Token< char >           c;
   Token< int >            i;
   Token< const char* >    s;
   Token< char* >          str;
   Token< double >         f;
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
#define	ILLEGAL_TOKEN	288
#define	CHRCONST	289
#define	INTCONST	290
#define	TTAG	291
#define	VAR	292
#define	IDENT	293
#define	TYPEIDENT	294
#define	DOCBLOCK	295
#define	STRATOM	296
#define	TAGATOM	297
#define	FLTCONST	298
#define	opINTNAME	299
#define	opINTNAMER	300
#define	opMINUSMINUS	301
#define	opPLUSPLUS	302
#define	STMT_SEP	303
#define	opSHL	304
#define	opSHR	305
#define	opPLASN	306
#define	opMIASN	307
#define	opMLASN	308
#define	opDVASN	309
#define	opMODASN	310
#define	opANDASN	311
#define	opSTRAPPEND	312
#define	opXORASN	313
#define	opORASN	314
#define	opSLASN	315
#define	opSRASN	316
#define	opCAT	317
#define	opEQ	318
#define	opNE	319
#define	opGE	320
#define	opLE	321
#define	opAND	322
#define	opOR	323
#define	opSTREQ	324
#define	opCOLONCOLON	325
#define	opMDASN	326
#define	opNDASN	327
#define	opNTASN	328
#define	opSTRNE	329
#define	UNARY	330


extern YYSTYPE CMDlval;
