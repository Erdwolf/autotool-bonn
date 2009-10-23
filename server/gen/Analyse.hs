module Analyse (
    analyseModule
) where

import Language.Haskell.Exts
import Types
import Basic

-- Module SrcLoc ModuleName [OptionPragma] (Maybe WarningText)
--   (Maybe [ExportSpec]) [ImportDecl] [Decl]
analyseModule :: Module -> [AData]
analyseModule (Module _ _ _ _ _ _ decls) = let
    decls' = [d | d@(DataDecl {}) <- decls]
  in
    map analyseDataDecl decls'

-- DataDecl SrcLoc DataOrNew Context Name [TyVarBind] [QualConDecl] [Deriving]
analyseDataDecl :: Decl -> AData
analyseDataDecl (DataDecl _ _ [] (Ident nm) tvs cons _) = let
  in
    AData (AType nm (map analyseTyVarBind tvs)) (map analyseQualConDecl cons)
analyseDataDecl x = error ("analyseDataDecl " ++ show x)

-- QualConDecl SrcLoc [TyVarBind] Context ConDecl
analyseQualConDecl :: QualConDecl -> ACons
analyseQualConDecl (QualConDecl _ [] [] condecl) = analyseConDecl condecl
analyseQualConDecl x = error ("analyseQualConDecl " ++ show x)

-- ConDecl Name [BangType]
-- InfixConDecl BangType Name BangType
-- RecDecl Name [([Name], BangType)]
analyseConDecl :: ConDecl -> ACons
analyseConDecl (ConDecl (Ident nm) ts) = ACons nm (map analyseBangType ts)
analyseConDecl (RecDecl (Ident nm) ts) = ARec nm [(r, analyseBangType t) | ([Ident r], t) <- ts]
analyseConDecl x = error ("analyseConDecl " ++ show x)

-- BangedTy Type
-- UnBangedTy Type
-- UnpackedTy Type
analyseBangType :: BangType -> AType
analyseBangType (BangedTy ty)   = analyseType ty
analyseBangType (UnBangedTy ty) = analyseType ty
analyseBangType (UnpackedTy ty) = analyseType ty

-- TyForall (Maybe [TyVarBind]) Context Type
-- TyFun Type Type
-- TyTuple Boxed [Type]
-- TyList Type
-- TyApp Type Type
-- TyVar Name
-- TyCon QName
-- TyParen Type
-- TyInfix Type QName Type
-- TyKind Type Kind
analyseType :: Type -> AType
analyseType (TyCon qn) = basic (unqual qn)
analyseType (TyVar (Ident nm)) = AVar nm
analyseType (TyApp t1 t2) = let
   (AType t1' args) = analyseType t1
   t2' = analyseType t2
 in
   AType t1' (args ++ [t2'])
analyseType x = error ("analyseType " ++ show x)

-- KindedVar Name Kind
-- UnkindedVar Name
analyseTyVarBind :: TyVarBind -> AType
analyseTyVarBind (UnkindedVar (Ident nm)) = AVar nm
analyseTyVarBind x = error ("analyseTyVarBind " ++ show x)

-- Qual ModuleName Name
-- UnQual Name
-- Special SpecialCon
unqual :: QName -> String
unqual (Qual _ (Ident nm)) = nm
unqual (UnQual (Ident nm)) = nm
unqual (Special ListCon) = "List"
unqual x = error ("unqual " ++ show x)
