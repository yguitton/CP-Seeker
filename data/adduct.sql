BEGIN TRANSACTION;
INSERT INTO `adduct` (adduct,charge,formula_add,formula_ded,multi) VALUES ('M+H',1,'H1','H0',1),
 ('M+NH4',1,'N1H4','H0',1),
 ('M+Na',1,'Na1','H0',1),
 ('M+K',1,'K1','H0',1),
 ('M+',1,'H0','H0',1),
 ('M-H',-1,'H0','H1',1),
 ('M-2H',-2,'H0','H2',1),
 ('M-3H',-3,'H0','H3',1),
 ('M+FA-H',-1,'C1O2H2','H1',1),
 ('M+Hac-H',-1,'C2O2H4','H1',1),
 ('M-',-1,'H0','H0',1),
 ('M+3H',3,'H3','H0',1),
 ('M+2H+Na',3,'H2Na1','H0',1),
 ('M+H+2Na',3,'H1Na2','H0',1),
 ('M+3Na',3,'Na3','H0',1),
 ('M+2H',2,'H2','H0',1),
 ('M+H+NH4',2,'H1N1H4','H0',1),
 ('M+H+Na',2,'H1Na1','H0',1),
 ('M+H+K',2,'H1K1','H0',1),
 ('M+ACN+2H',2,'C2H5N1','H0',1),
 ('M+2Na',2,'Na2','H0',1),
 ('M+2ACN+2H',2,'C4H8N2','H0',1),
 ('M+3ACN+2H',2,'C6H11N3','H0',1),
 ('M+CH3OH+H',1,'C1H5O1','H0',1),
 ('M+ACN+H',1,'C2H4N1','H0',1),
 ('M+2Na-H',1,'Na2','H1',1),
 ('M+IsoProp+H',1,'C3H9O1','H0',1),
 ('M+ACN+Na',1,'C2H3N1Na1','H0',1),
 ('M+2K-H',1,'K2','H1',1),
 ('M+DMSO+H',1,'C2H7S1O1','H0',1),
 ('M+2ACN+H',1,'C4H7N2','H0',1),
 ('M+IsoProp+Na+H',1,'C3H9O1Na1','H0',1),
 ('2M+H',1,'H1','H0',2),
 ('2M+NH4',1,'N1H4','H0',2),
 ('2M+Na',1,'Na1','H0',2),
 ('2M+3H2O+2H',1,'H8O6','H0',2),
 ('2M+K',1,'K1','H0',2),
 ('2M+ACN+H',1,'C2H4N1','H0',2),
 ('2M+ACN+Na',1,'C2H3N1Na1','H0',2),
 ('M-H2O-H',-1,'H0','H3O1',1),
 ('M+Na-2H',-1,'Na1','H2',1),
 ('M+Cl',-1,'Cl1','H0',1),
 ('M+K-2H',-1,'K1','H2',1),
 ('M+Br',-1,'Br1','H0',1),
 ('M+TFA-H',-1,'C2F3O2H1','H1',1),
 ('2M-H',-1,'H0','H1',2),
 ('2M+FA-H',-1,'C1O2H2','H1',2),
 ('2M+Hac-H',-1,'C2O2H4','H1',2),
 ('M+NO3',-1,'N1O3','H0',1),
 ('M-Cl',-1,'H0','Cl1',1),
 ('M-H-Cl',-1,'H0','Cl1H1',1);
COMMIT;
