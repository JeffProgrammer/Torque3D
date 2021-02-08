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
   ParamNode*              param;
   TypeNode*               typeNode;
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
#define	ILLEGAL_TOKEN	292
#define	CHRCONST	293
#define	INTCONST	294
#define	TTAG	295
#define	VAR	296
#define	IDENT	297
#define	TYPEIDENT	298
#define	DOCBLOCK	299
#define	STRATOM	300
#define	TAGATOM	301
#define	FLTCONST	302
#define	opINTNAME	303
#define	opINTNAMER	304
#define	opMINUSMINUS	305
#define	opPLUSPLUS	306
#define	STMT_SEP	307
#define	opSHL	308
#define	opSHR	309
#define	opPLASN	310
#define	opMIASN	311
#define	opMLASN	312
#define	opDVASN	313
#define	opMODASN	314
#define	opANDASN	315
#define	opXORASN	316
#define	opORASN	317
#define	opSLASN	318
#define	opSRASN	319
#define	opCAT	320
#define	opEQ	321
#define	opNE	322
#define	opGE	323
#define	opLE	324
#define	opAND	325
#define	opOR	326
#define	opSTREQ	327
#define	opCOLONCOLON	328
#define	opMDASN	329
#define	opNDASN	330
#define	opNTASN	331
#define	opSTRNE	332
#define	UNARY	333


extern YYSTYPE CMDlval;
